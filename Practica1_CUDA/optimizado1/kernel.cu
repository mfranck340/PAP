#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <curand_kernel.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <list>

using namespace std;

__constant__ int dev_N;		//Número de columnas (memoria constante)
__constant__ int dev_M;		//Número de filas (memoria constante)
__constant__ int dev_DIF;	//Dificultad (memoria constante)

int vidas = 0;				//Número de vidas
int N = 0;					//Número de columnas
int M = 0;					//Número de filas
int dif;					//Dificultad (4 -> Fácil / 6 -> Difícil)
char ejecucion;				//Tipo de ejecución (a -> Automática / m -> Manual)

//Da a cada columna una semilla para generar números aleatorios
__global__ void setup_kernel(curandState* state, unsigned long seed) {
	int col = (blockIdx.x * blockDim.x) + threadIdx.x;								//Columna del hilo en el tablero
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;								//Fila del hilo en el tablero
	int id = (fil * dev_N) + col;													//Posición del hilo en el tablero
	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (fil == 0)
			curand_init(seed, id, 0, &state[id]);									//Inicializamos la semilla (seed cambia con el tiempo)
	}
}

//Genera fichas aleatorias en los bloques de aire que hay arriba
__global__ void generar_fichas(char* dev_tablero, curandState* globalState, int* dev_fichasInf) {
	int col = (blockIdx.x * blockDim.x) + threadIdx.x;								//Columna del hilo en el tablero
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;								//Fila del hilo en el tablero
	int pos = ((fil * dev_N) + col) * 2;											//Posición del hilo en el tablero

	//Si tenemos un bloque de aire en la primera fila, entonces generamos una ficha aleatoria en esa posición
	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (fil == 0 && dev_tablero[pos] == '0') {
			int idx = threadIdx.x;
			curandState localState = globalState[idx];								//Cogemos la semilla calculada anteriormente
			dev_tablero[pos] = (int)(curand_uniform(&localState) * dev_DIF) + 1;	//Obtenemos el valor aleatorio y actualizamos la semilla
			globalState[idx] = localState;											//Guardamos la semilla actualizada

			atomicSub(&dev_fichasInf[1], 1);										//Restamos 1 al número de bloques de aire
		}
	}

}

//Baja las fichas cuando se detectan bloques de aire
__global__ void bajar_fichas(char* dev_tablero) {
	int col = (blockIdx.x * blockDim.x) + threadIdx.x;								//Columna del hilo en el tablero
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;								//Fila del hilo en el tablero
	int pos = ((fil * dev_N) + col) * 2;											//Posición del hilo en el tablero

	//Si la fila se corresponde con la última del tablero, nos recorremos la columna hacia arriba hasta encontrar bloques de aire
	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (fil == dev_M - 1) {
			for (int i = pos; i >= dev_N * 2; i -= dev_N * 2) {
				//Si tenemos un bloque de aire y el de arriba no lo es, tenemos que hacer que caiga la ficha
				if (dev_tablero[i] == '0' && dev_tablero[(i - dev_N * 2)] != '0') {
					dev_tablero[i] = dev_tablero[(i - dev_N * 2)];					//Bajamos la ficha
					dev_tablero[i + 1] = dev_tablero[(i - dev_N * 2) + 1];
					dev_tablero[(i - dev_N * 2)] = '0';								//Ponemos el bloque de aire en la posición de encima
					dev_tablero[(i - dev_N * 2) + 1] = '0';
				}
			}
		}
	}
}

//Elimina las fichas (la que se toca el usuario y las equivalentes que están juntas)
__global__ void eliminar_fichas(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;								//Columna del hilo en el tablero
	int fil = blockIdx.y * blockDim.y + threadIdx.y;								//Fila del hilo en el tablero
	int idx = (fil * dev_N + col) * 2;												//Posición del hilo en el tablero
	char elem = dev_fichaInf[2];													//Elemento que ha tocado el usuario
	bool encontrado = false;														//Variable de control para llevar a cabo el borrado

	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (dev_tablero[idx] == elem && dev_tablero[idx] != '0') {					//Si el elemento del hilo coincide con el elemento que ha tocado el usuario y no ha sido borrado, comprobamos si se puede borrar
			if (col + 1 < dev_N && dev_tablero[idx + 2] == '0') {
				encontrado = true;													//Si a la derecha tiene un bloque de aire, ha encontrado una ruta al bloque que ha tocado el usuario y debe ser borrado
			}
			else if (col - 1 >= 0 && dev_tablero[idx - 2] == '0') {
				encontrado = true;													//Si a la izquierda tiene un bloque de aire, ha encontrado una ruta al bloque que ha tocado el usuario y debe ser borrado
			}
			else if (fil + 1 < dev_M && dev_tablero[idx + (dev_N * 2)] == '0') {
				encontrado = true;													//Si abajo tiene un bloque de aire, ha encontrado una ruta al bloque que ha tocado el usuario y debe ser borrado
			}
			else if (fil - 1 >= 0 && dev_tablero[idx - (dev_N * 2)] == '0') {
				encontrado = true;													//Si arriba tiene un bloque de aire, ha encontrado una ruta al bloque que ha tocado el usuario y debe ser borrado
			}
			if (encontrado) {														//Si se ha encontrado una ruta hasta el bloque que ha tocado el usuario, borramos el bloque
				dev_tablero[idx] = '0';												//Colocamos un bloque de aire
				atomicAdd(&dev_fichaInf[1], 1);										//Aumenta el contador de fichas eliminadas
				atomicSub(&dev_fichaInf[0], 1);										//Restamos 1 al contador de fichas del tipo que se quiere eliminar
			}
		}
	}
	__syncthreads();

}

//Coloca el punto de busqueda de eliminar fichas y fichas especiales
__global__ void colocar_fichaEX(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, curandState* globalState) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;								//Columna del hilo en el tablero
	int fil = blockIdx.y * blockDim.y + threadIdx.y;								//Fila del hilo en el tablero
	int idx = (fil * dev_N + col) * 2;												//Posición del hilo en el tablero
	int touch = (dev_coordenadas[1] * dev_N + dev_coordenadas[0]) * 2;

	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (idx == touch) {															//Comprobamos que somos el hilo de las coordenadas del usuario
			if (dev_fichaInf[1] == 1) {												//Si solo se ha eliminado 1
				dev_tablero[idx] = dev_fichaInf[2];									//Volvemos a la ficha anterior
				dev_fichaInf[1] -= 1;
			}
			else if (dev_fichaInf[1] == 0) {										//Si aun no se ha eliminado ninguna
				dev_fichaInf[2] = dev_tablero[idx];									//Colocamos el punto de busqueda para eliminar fichas
				dev_tablero[idx] = '0';
				dev_fichaInf[1] += 1;
			}
			else if (dev_fichaInf[1] == 5) {										//Si se han eliminado 5 fichas
				dev_tablero[idx] = 'B';												//Se coloca una bomba en el tablero
				dev_fichaInf[1] -= 1;
			}
			else if (dev_fichaInf[1] == 6) {										//Si se han eliminado 6 fichas
				dev_tablero[idx] = 'T';												//Se coloca una TNT en el talbero
				dev_fichaInf[1] -= 1;
			}
			else if (dev_fichaInf[1] >= 7) {										//Si se han eliminado 7 fichas
				dev_tablero[idx] = 'R';												//Se coloca un tompecabezas en el tablero
				int id = col;
				curandState localState = globalState[id];
				float r = (curand_uniform(&localState) * dev_DIF) + 1;				//Seleccionamos el complemento del rompecabezas de forma aleatoria
				globalState[id] = localState;
				dev_fichaInf[1] -= 1;
				dev_tablero[idx + 1] = (int)r;
			}
		}
		else {
			atomicAdd(&dev_fichaInf[0], 1);											//Se contabilizan las fichas que son del mismo tipo que la ficha tocada por el usuario
		}
	}
}

//Elimina la bomba (borra la fila o columna aleatoriamente)
__global__ void eliminar_bomba(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, int aleatorio) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;								//Columna del hilo en el tablero
	int fil = blockIdx.y * blockDim.y + threadIdx.y;								//Fila del hilo en el tablero
	int pos = (fil * dev_N + col) * 2;												//Posición del hilo en el tablero

	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (aleatorio == 0) {														//Si aleatorio es 0, comprueba que coincide la columna con la del bloque que ha tocado el usuario
			if (fil == dev_coordenadas[1]) {
				dev_tablero[pos] = '0';												//Se borran todos los elementos de la columna
				atomicAdd(&dev_fichaInf[1], 1);
			}
		}
		else {																		//Si aleatorio es 1, comprueba que coincide la fila con la del bloque que ha tocado el usuario
			if (col == dev_coordenadas[0]) {										
				dev_tablero[pos] = '0';												//Se borran todos los elementos de la fila
				atomicAdd(&dev_fichaInf[1], 1);										//Suma 1 a los bloques que se han convertido en aire
			}
		}
	}
}

//Elimina la TNT (borra los bloques adyacentes en un radio de 4)
__global__ void eliminar_tnt(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;								//Columna del hilo en el tablero
	int fil = blockIdx.y * blockDim.y + threadIdx.y;								//Fila del hilo en el tablero
	int pos = (fil * dev_N + col) * 2;												//Posición del hilo en el tablero
	int distancia = sqrt((float)pow((double)col - dev_coordenadas[0], 2.0) + pow((double)fil - dev_coordenadas[1], 2.0));	//Calculamos la distancia entre dos puntos

	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (distancia <= 4) {														//Si el bloque está en un radio de 4, lo borra
			dev_tablero[pos] = '0';
			atomicAdd(&dev_fichaInf[1], 1);											//Suma 1 a los bloques que se han convertido en aire
		}
	}
}

//Elimina el rompecabezas (borra los bloques cuyo elemento coincida con el del rompecabezas)
__global__ void eliminar_rompecabezas(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;								//Columna del hilo en el tablero
	int fil = blockIdx.y * blockDim.y + threadIdx.y;								//Fila del hilo en el tablero
	int pos = (fil * dev_N + col) * 2;												//Posición del hilo en el tablero
	int touch = (dev_coordenadas[1] * dev_N + dev_coordenadas[0]) * 2;
	char elem = dev_tablero[touch + 1];

	__syncthreads();
	if (dev_N > col && dev_M > fil) {												//Comprobamos si estamos dentro del bloque
		if (dev_tablero[pos] == elem || pos == touch) {								//Si el elemento coincide con el que ha tocado el usuario o si es el que ha tocado el usuario,
			dev_tablero[pos] = '0';													//Borra el elemento
			atomicAdd(&dev_fichaInf[1], 1);											//Suma 1 a los bloques que se han convertido en aire
		}
	}
}

void update_tablero() {

}

void run_game() {

}

//Rellena con bloques de aire todo el tablero
void vaciar_tablero(char* tablero) {
	for (int i = 0; i < N * M * 2; i++) {
		tablero[i] = '0';
	}
}

//Imprime el tablero por pantalla
void mostrar_tablero(char* tablero) {
	for (int i = 0; i < M; i++) {													//Recorremos las filas												
		printf("\n\n| ");
		for (int j = 0; j < N; j++) {												//Recorremos las columnas
			if ((int)tablero[((i * N) + j) * 2] > 0 && (int)tablero[((i * N) + j) * 2] <= 6)
				printf("%d | ", tablero[((i * N) + j) * 2]);						//Si es una ficha

			else if (tablero[((i * N) + j) * 2] == 'R')								//Si es un rompecabezas
				printf("%c%d | ", tablero[((i * N) + j) * 2], tablero[((i * N) + j) * 2 + 1]);

			else
				printf("%c | ", tablero[((i * N) + j) * 2]);						//Si es otro bloque especial
		}
		printf("\n");
		for (int j = 0; j < N; j++) {
			printf(" ---");
		}
	}
	printf("\n-----------------------------------------------------------\n");
}

int main(int argc, const char* argv[]) {
	cudaFree(0);

	//Datos usuario
	vidas = 200;
	//N = 9;					//columnas
	//M = 3;					//filas
	//dif = 4;
	//ejecucion = 'a';

	//Pedir datos al usuario
	do {
		printf("Introduce el numero de filas del tablero: ");
		scanf("%d", &M);
	} while ((int)M < 1);

	do {
		printf("Introduce el numero de columnas del tablero: ");
		scanf("%d", &N);
	} while ((int)N < 1);

	do {
		printf("Introduce el tipo de ejecucion (m --> Manual / a --> Automatica): ");
		fflush(stdout);
		scanf("%c", &ejecucion);
	} while (ejecucion != 'm' && ejecucion != 'a');

	do {
		printf("Introduce la dificultad del juego (1 --> Facil / 2 --> Dificil): ");
		scanf("%d", &dif);
	} while (dif != 1 && dif != 2);

	if (dif == 1) dif = 4;
	else dif = 6;

	//Optimizar dimensiones
	cudaDeviceProp deviceProp;
	cudaGetDeviceProperties(&deviceProp, 0);
	printf("\nNombre GPU: %s\n", deviceProp.name);
	int BLOCK_SIZE = sqrt(deviceProp.maxThreadsPerBlock);
	printf("\nBlock size: %d\n", BLOCK_SIZE);

	//Declaración de variables
	int SIZE = N * M * 2 * sizeof(char);
	int size_coord = 2 * sizeof(int);
	int size_ficha = 3 * sizeof(int);
	int* h_coordenadas = (int*)malloc(size_coord);								//Coordenadas del bloque que toca el usuario
	int* h_fichaInf = (int*)malloc(size_ficha);									//Información de las fichas (número de bloques eliminados / número de fichas del mismo tipo que ha pulsado el usuario)
	char* h_tablero = (char*)malloc(SIZE);										//Tablero

	//Variables GPU
	curandState* dev_states;													//Guarda las semillas para los números aleatorios
	char* dev_tablero;															//Tablero
	int* dev_coordenadas;														//Coordenadas del bloque que toca el usuario
	int* dev_fichaInf;															//Información de las fichas (número de fichas del mismo tipo que ha pulsado el usuario / número de bloques eliminados)

	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));								//Pasa dif a dev_DIF (memoria constante)
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));									//Pasa N a dev_N (memoria constante)
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));									//Pasa M a dev_M (memoria constante)
	cudaMalloc(&dev_states, N * sizeof(curandState));							//Reserva memoria global para dev_states
	cudaMalloc((void**)&dev_coordenadas, size_coord);							//Reserva memoria global para dev_coordenadas
	cudaMalloc((void**)&dev_fichaInf, size_ficha);								//Reserva memoria global para dev_fichaInf
	cudaMalloc((void**)&dev_tablero, SIZE);										//Reserva memoria global para dev_tablero

	dim3 blocksInGrid(2, 2);
	dim3 threadsInBlock(5, 2);

	//Inicializar tablero
	//---------------------------------------------------------------------------------------------
	h_fichaInf[0] = 0;															//El usuario no ha tocado ningún bloque
	h_fichaInf[1] = N * M;														//Todas las casillas son bloques de aire

	vaciar_tablero(h_tablero);													//Vaciamos el tablero
	mostrar_tablero(h_tablero);													//Mostramos el tablero

	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);			//Transfiere el contenido de h_tablero a dev_tablero
	cudaMemcpy(dev_fichaInf, h_fichaInf, size_ficha, cudaMemcpyHostToDevice);	//Transfiere el contenido de h_fichaInf a dev_fichaInf

	setup_kernel << <blocksInGrid, threadsInBlock >> > (dev_states, time(0));	//Generamos las semillas

	while (h_fichaInf[1] != 0) {												//Se llama iterativamente a bajar_fichas y generar_fichas hasta que no queden bloques de aire
		bajar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero);
		generar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_states, dev_fichaInf);
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);												//Mostramos el tablero
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
	}

	//Bucle principal
	//---------------------------------------------------------------------------------------------
	int ficha;
	srand(time(NULL));

	printf("\nComienza el juego :)\n");

	//Mientras tengamos vidas, el juego continúa
	while (vidas > 0) {
		printf("\nVidas: %d\n", vidas);
		if (ejecucion == 'm') {													//Ejecución manual
			printf("Ejecucion manual <m>\n");
			do {																//Solicitamos las coordenadas al usuario
				printf("Introduce el numero de columna (%d - %d): ", 0, N - 1);
				scanf("%d", &h_coordenadas[0]);
			} while (h_coordenadas[0] < 0 || h_coordenadas[0] >= N);

			do {
				printf("Introduce el numero de fila (%d - %d): ", 0, M - 1);
				scanf("%d", &h_coordenadas[1]);
			} while (h_coordenadas[1] < 0 || h_coordenadas[1] >= M);
		}
		else {																	//Ejecución automática
			printf("Ejecucion automatica <a>\n");
			h_coordenadas[0] = rand() % N;										//Seleccionamos las coordenadas aleatoriamente
			h_coordenadas[1] = rand() % M;
		}
		printf("Coordenadas: (%d, %d)\n", h_coordenadas[0], h_coordenadas[1]);

		ficha = (h_coordenadas[1] * N + h_coordenadas[0]) * 2;					//Posición del bloque con la coordenadas

		cudaMemcpy(dev_coordenadas, h_coordenadas, size_coord, cudaMemcpyHostToDevice);

		if ((int)h_tablero[ficha] <= 6) {										//Eliminamos fichas
			colocar_fichaEX << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);		//Colocamos la ficha a eliminar
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
			int salir;
			do {																//Eliminamos fichas del tablero de 
				salir = h_fichaInf[0];
				eliminar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
				cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
			} while (salir != h_fichaInf[0]);

			colocar_fichaEX << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);		//Sustituimos la ficha tocada por su equivalente de fichas eliminadas
		}
		else if (h_tablero[ficha] == 'B') {										//Eliminamos bomba
			int aleatorio = rand() % 2;
			eliminar_bomba << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, aleatorio);
		}
		else if (h_tablero[ficha] == 'T') {										//Eliminamos TNT
			eliminar_tnt << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		else {																	//Eliminamos rompecabezas
			eliminar_rompecabezas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		printf("Talbero despues de eliminar: \n");
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);												//Mostramos el tablero

		printf("\nEliminadas: %d\n", h_fichaInf[1]);
		if (h_fichaInf[1] == 0) vidas--;										//Restamos una vida si se ha eliminado sólamente un bloque

		while (h_fichaInf[1] != 0) {											//Se llama iterativamente a bajar_fichas y generar_fichas hasta que no queden bloques de aire
			bajar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero);
			generar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_states, dev_fichaInf);
			cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
			mostrar_tablero(h_tablero);											//Mostramos el tablero
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
		}

		h_fichaInf[0] = 0;														//Reiniciamos las fichas que podrian borrarse
		cudaMemcpy(dev_fichaInf, h_fichaInf, size_ficha, cudaMemcpyHostToDevice);
	}
	printf("\nVidas: %d\n", vidas);
	printf("\nGAME OVER :(\n");

	//Liberar memoria
	//---------------------------------------------------------------------------------------------
	cudaFree(dev_tablero);
	cudaFree(dev_coordenadas);
	cudaFree(dev_fichaInf);
	cudaFree(dev_states);
	free(h_tablero);
	free(h_coordenadas);
	free(h_fichaInf);

}
