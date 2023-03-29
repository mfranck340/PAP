#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <curand_kernel.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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
	int id = threadIdx.x;
	curand_init(seed, id, 0, &state[id]);
}

//Genera fichas aleatorias en los bloques de aire que hay arriba
__global__ void generar_fichas(char* dev_tablero, curandState* globalState, int* dev_fichasInf) {
	int pos = ((threadIdx.y * blockDim.x) + threadIdx.x) * 2;	//Posición del hilo en el tablero

	//Si tenemos un bloque de aire, entonces generamos una ficha aleatoria en lo alto de la columna
	if (dev_tablero[pos] == '0') {
		int idx = threadIdx.x;
		curandState localState = globalState[idx];								//Cogemos la semilla calculada anteriormente
		dev_tablero[pos] = (int) (curand_uniform(&localState) * dev_DIF) + 1;	//Obtenemos el valor aleatorio y actualizamos la semilla
		globalState[idx] = localState;											//Guardamos la semilla actualizada
		
		atomicSub(&dev_fichasInf[1], 1);										//Restamos 1 al número de bloques de aire
	}
}

//Baja las fichas cuando se detectan bloques de aire debajo
__global__ void bajar_fichas(char* dev_tablero) {
	int pos = (dev_N * dev_M - threadIdx.x) - 1;

	for (int i = pos; i >= dev_N; i -= dev_N) {
		if (dev_tablero[i * 2] == '0') {
			if (dev_tablero[(i - dev_N) * 2] != '0') {
				dev_tablero[i * 2] = dev_tablero[(i - dev_N) * 2];
				dev_tablero[i * 2 + 1] = dev_tablero[(i - dev_N) * 2 + 1];
				dev_tablero[(i - dev_N) * 2] = '0';
				dev_tablero[(i - dev_N) * 2 + 1] = '0';
			}
		}
	}
}

__global__ void eliminar_fichas(char* dev_tablero, int* dev_coordenadas,  int* dev_fichaInf, curandState* globalState) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int idx = (fil * blockDim.x + col) * 2;
	int touch = (dev_coordenadas[1] * blockDim.x + dev_coordenadas[0]) * 2;
	char elem = dev_tablero[touch];

	if (idx == touch) {
		dev_tablero[idx] = '0';
	}
	else if (dev_tablero[idx] == dev_tablero[touch]) {
		atomicAdd(&dev_fichaInf[0], 1);
	}
	
	__syncthreads();
	int total = dev_fichaInf[0];
	dev_fichaInf[1] = dev_fichaInf[0];
	bool encontrado = false;
	do {
		dev_fichaInf[0] = dev_fichaInf[1];
		__syncthreads();
		if (dev_tablero[idx] == elem && dev_tablero[idx] != '0') {
			if (col + 1 < blockDim.x && dev_tablero[idx + 2] == '0') {
				encontrado = true;
			}
			else if (col - 1 >= 0 && dev_tablero[idx - 2] == '0') {
				encontrado = true;
			}
			else if (fil + 1 < blockDim.y && dev_tablero[idx + (blockDim.x * 2)] == '0') {
				encontrado = true;
			}
			else if (fil - 1 >= 0 && dev_tablero[idx - (blockDim.x * 2)] == '0') {
				encontrado = true;
			}
			if (encontrado) {
				atomicSub(&dev_fichaInf[1], 1);
				dev_tablero[idx] = '0';
			}
		}
		__syncthreads();
	} while (dev_fichaInf[0] != dev_fichaInf[1]);
	
	if (idx == touch) {
		dev_fichaInf[1] = total - dev_fichaInf[1] + 1;
		if (dev_fichaInf[1] == 5) {
			dev_tablero[idx] = 'B';
			dev_fichaInf[1] -= 1;
		}
		else if (dev_fichaInf[1] == 6) {
			dev_tablero[idx] = 'T';
			dev_fichaInf[1] -= 1;
		}	
		else if (dev_fichaInf[1] >= 7) {
			dev_tablero[idx] = 'R';
			int id = threadIdx.x;
			curandState localState = globalState[id];
			float r = (curand_uniform(&localState) * dev_DIF) + 1;
			globalState[id] = localState;
			dev_fichaInf[1] -= 1;
			dev_tablero[idx + 1] = (int)r;
		}
	}
}

__global__ void eliminar_bomba(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, int aleatorio) {
	int pos = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	if (aleatorio == 0) {
		if (threadIdx.y == dev_coordenadas[1]) {
			dev_tablero[pos] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
	else {
		if (threadIdx.x == dev_coordenadas[0]) {
			dev_tablero[pos] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
}

__global__ void eliminar_tnt(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int pos = (threadIdx.y * blockDim.x + threadIdx.x) * 2;
	int distancia = sqrt((float)pow((double)threadIdx.x - dev_coordenadas[0], 2.0) + pow((double)threadIdx.y - dev_coordenadas[1], 2.0));

	if (distancia <= 4) {
		dev_tablero[pos] = '0';
		atomicAdd(&dev_fichaInf[1], 1);
	}
}

__global__ void eliminar_rompecabezas(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int pos = (threadIdx.y * blockDim.x + threadIdx.x) * 2;
	int touch = (dev_coordenadas[1] * blockDim.x + dev_coordenadas[0]) * 2;
	char elem = dev_tablero[touch + 1];

	__syncthreads();

	if (dev_tablero[pos] == elem || pos == touch) {
		dev_tablero[pos] = '0';
		atomicAdd(&dev_fichaInf[1], 1);
	}
}

void update_tablero() {

}

void run_game() {

}

void vaciar_tablero(char* tablero) {
	for (int i = 0; i < N * M * 2; i++) {
		tablero[i] = '0';
	}
}

void mostrar_tablero(char* tablero) {
	for (int i = 0; i < M; i++) {
		printf("\n\n| ");
		for (int j = 0; j < N; j++) {
			if ((int)tablero[((i * N) + j) * 2] > 0 && (int)tablero[((i * N) + j) * 2] <= 6)
				printf("%d | ", tablero[((i * N) + j) * 2]);

			else if (tablero[((i * N) + j) * 2] == 'R') 
				printf("%c%d | ", tablero[((i * N) + j) * 2], tablero[((i * N) + j) * 2 + 1]);

			else 
				printf("%c | ", tablero[((i * N) + j) * 2]);
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
	vidas = 100;
	N = 11;				//columnas
	M = 6;				//filas
	dif = 6;
	ejecucion = 'm';

	//Declaraci�n de variables
	int SIZE = N * M * 2 * sizeof(char);
	int size_coord = 2 * sizeof(int);
	int* h_coordenadas = (int*)malloc(size_coord);
	int* h_fichaInf = (int*)malloc(size_coord);
	char* h_tablero = (char*)malloc(SIZE);

	//Variables GPU
	curandState* dev_states;
	char* dev_tablero;
	int* dev_coordenadas;
	int* dev_fichaInf;

	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));
	cudaMalloc(&dev_states, N * sizeof(curandState));
	cudaMalloc((void**)&dev_coordenadas, size_coord);
	cudaMalloc((void**)&dev_fichaInf, size_coord);
	cudaMalloc((void**)&dev_tablero, SIZE);

	//Inicializar tablero
	//---------------------------------------------------------------------------------------------
	h_fichaInf[0] = 0;
	h_fichaInf[1] = N * M;

	vaciar_tablero(h_tablero);
	mostrar_tablero(h_tablero);

	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_fichaInf, h_fichaInf, size_coord, cudaMemcpyHostToDevice);
	dim3 blocksInGrid(1);
	dim3 threadsInBlock(N);
	setup_kernel <<<blocksInGrid, threadsInBlock>>> (dev_states, time(0));

	while (h_fichaInf[1] != 0) {
		bajar_fichas <<<blocksInGrid, threadsInBlock>>> (dev_tablero);
		generar_fichas <<<blocksInGrid, threadsInBlock>>> (dev_tablero, dev_states, dev_fichaInf);

		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
	}

	//Bucle principal
	//---------------------------------------------------------------------------------------------
	int ficha;
	srand(time(NULL));
	dim3 bloques(1);
	dim3 hilos(N, M);

	printf("\nComienza el juego :)\n");
	while (vidas > 0) {
		printf("\nVidas: %d\n", vidas);
		if (ejecucion == 'm') {
			printf("Ejecucion manual <m>\n");
			do {
				printf("Introduce el numero de columna (%d - %d): ", 0, N - 1);
				scanf("%d", &h_coordenadas[0]);
			} while (h_coordenadas[0] < 0 || h_coordenadas[0] >= N);
			
			do {
				printf("Introduce el numero de fila (%d - %d): ", 0, M - 1);
				scanf("%d", &h_coordenadas[1]);
			} while (h_coordenadas[1] < 0 || h_coordenadas[1] >= M);
		}
		else {
			printf("Ejecucion automatica <a>\n");
			h_coordenadas[0] = rand() % N;
			h_coordenadas[1] = rand() % M;
		}
		printf("Coordenadas: (%d, %d)\n", h_coordenadas[0], h_coordenadas[1]);

		ficha = (h_coordenadas[1] * N + h_coordenadas[0]) * 2;
		
		cudaMemcpy(dev_coordenadas, h_coordenadas, size_coord, cudaMemcpyHostToDevice);

		if ((int)h_tablero[ficha] <= 6) {
			printf("\nEn eliminar\n");
			eliminar_fichas <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);
		}
		else if (h_tablero[ficha] == 'B') {
			int aleatorio = rand() % 2;
			eliminar_bomba <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf, aleatorio);
		}
		else if (h_tablero[ficha] == 'T') {
			printf("\nEn tnt\n");
			eliminar_tnt <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		else {
			printf("\nEn rompe\n");
			eliminar_rompecabezas <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);

		if (h_fichaInf[1] == 1) vidas--;

		while (h_fichaInf[1] != 0) {
			bajar_fichas <<<blocksInGrid, threadsInBlock>>> (dev_tablero);
			generar_fichas <<<blocksInGrid, threadsInBlock>>> (dev_tablero, dev_states, dev_fichaInf);

			cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
			mostrar_tablero(h_tablero);
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
		}
		
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
