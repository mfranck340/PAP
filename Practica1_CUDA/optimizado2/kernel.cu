#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <curand_kernel.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <list>

using namespace std;

__constant__ int dev_N;
__constant__ int dev_M;
__constant__ int dev_DIF;

int vidas = 0;
int N = 0;
int M = 0;
int dif;
char ejecucion;
const int BLQ_X = 2;
const int BLQ_Y = 2;
const int TESELA_X = 5;
const int TESELA_Y = 2;

__global__ void setup_kernel(curandState* state, unsigned long seed) {
	int col = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;
	int id = (fil * dev_N) + col;
	if (dev_N > col && dev_M > fil) {
		if (fil == 0)
			curand_init(seed, id, 0, &state[id]);
	}
}

__global__ void generar_fichas(char* dev_tablero, curandState* globalState, int* dev_fichasInf) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;
	int pos = ((fil * dev_N) + col) * 2;

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[pos];
		tab_shared[pos_shared + 1] = dev_tablero[pos + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		if (fil == 0 && tab_shared[pos_shared] == '0') {
			int idx = threadIdx.x;
			curandState localState = globalState[idx];								//Cogemos la semilla calculada anteriormente
			tab_shared[pos_shared] = (int)(curand_uniform(&localState) * dev_DIF) + 1;	//Obtenemos el valor aleatorio y actualizamos la semilla
			globalState[idx] = localState;											//Guardamos la semilla actualizada
			atomicSub(&dev_fichasInf[1], 1);										//Restamos 1 al n�mero de bloques de aire
		}
	}

	__syncthreads();

	if (dev_N > col && dev_M > fil) 
		dev_tablero[pos] = tab_shared[pos_shared];
}

__global__ void bajar_fichas(char* dev_tablero) {
	int col = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fil = (blockIdx.y * blockDim.y) + threadIdx.y;
	int pos = ((fil * dev_N) + col) * 2;

	if (dev_N > col && dev_M > fil) {
		if (fil == dev_M - 1) {
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
	}
}

__global__ void eliminar_fichas(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int idx = (fil * dev_N + col) * 2;
	char elem = dev_fichaInf[2];
	bool encontrado = false;

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[idx];
		tab_shared[pos_shared + 1] = dev_tablero[idx + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	if (blockIdx.x == 0 && blockIdx.y == 0) {
		if (idx == 0) {
			for (int i = 0; i < TESELA_X * TESELA_Y * 2; i += 2) {
				printf("elem: %d\n", tab_shared[i]);
			}
		}
	}

	if (dev_N > col && dev_M > fil && tab_shared[pos_shared] == elem && tab_shared[pos_shared] != '0') {
		/*if (blockIdx.x == 0 && blockIdx.y == 0)
			printf("Yo si entro, pos: %d\n", idx);

		if (col + 1 < dev_N) {
			if (threadIdx.x + 1 < TESELA_X) {
				if (tab_shared[pos_shared + 2] == '0')
					encontrado = true;
			}
			else {
				if (dev_tablero[idx + 2] == '0')
					encontrado = true;
			}
		}
		if (col - 1 >= 0) { 
			if (threadIdx.x - 1 >= 0) {
				if (tab_shared[pos_shared - 2] == '0')
					encontrado = true;
			}
			else {
				if (dev_tablero[idx - 2] == '0')
					encontrado = true;
			}
		}
		if (fil + 1 < dev_M) { // && dev_tablero[idx + (dev_N * 2)] == '0') {
			if (threadIdx.y + 1 < TESELA_Y) {
				if (tab_shared[pos_shared + (TESELA_X * 2)] == '0')
					encontrado = true;
			}
			else {
				if (dev_tablero[idx + (dev_N * 2)] == '0')
					encontrado = true;
			}
		}
		if (fil - 1 >= 0) { // && dev_tablero[idx - (dev_N * 2)] == '0') {
			if (threadIdx.y - 1 >= 0) {
				if (tab_shared[pos_shared - (TESELA_X * 2)] == '0')
					encontrado = true;
			}
			else {
				if (dev_tablero[idx - (dev_N * 2)] == '0')
					encontrado = true;
			}
		}

		if (encontrado) {
			tab_shared[pos_shared] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
			atomicSub(&dev_fichaInf[0], 1);
		}*/

		if (col + 1 < dev_N && dev_tablero[idx + 2] == '0') {
			encontrado = true;
		}
		else if (col - 1 >= 0 && dev_tablero[idx - 2] == '0') {
			encontrado = true;
		}
		else if (fil + 1 < dev_M && dev_tablero[idx + (dev_N * 2)] == '0') {
			encontrado = true;
		}
		else if (fil - 1 >= 0 && dev_tablero[idx - (dev_N * 2)] == '0') {
			encontrado = true;
		}
		if (encontrado) {
			dev_tablero[idx] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
			atomicSub(&dev_fichaInf[0], 1);
		}
	}
	__syncthreads();

	/*if (dev_N > col && dev_M > fil) {
		dev_tablero[idx] = tab_shared[pos_shared];
		dev_tablero[idx + 1] = tab_shared[pos_shared + 1];
	}*/

}

__global__ void colocar_fichaEX(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, curandState* globalState) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int idx = (fil * dev_N + col) * 2;
	int touch = (dev_coordenadas[1] * dev_N + dev_coordenadas[0]) * 2;

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[idx];
		tab_shared[pos_shared + 1] = dev_tablero[idx + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		if (idx == touch) {
			if (dev_fichaInf[1] == 0) {
				dev_fichaInf[2] = tab_shared[pos_shared];
				tab_shared[pos_shared] = '0';
				dev_fichaInf[1] += 1;
			}
			else if (dev_fichaInf[1] == 5) {
				tab_shared[pos_shared] = 'B';
				dev_fichaInf[1] -= 1;
			}
			else if (dev_fichaInf[1] == 6) {
				tab_shared[pos_shared] = 'T';
				dev_fichaInf[1] -= 1;
			}
			else if (dev_fichaInf[1] >= 7) {
				tab_shared[pos_shared] = 'R';
				int id = col;
				curandState localState = globalState[id];
				tab_shared[pos_shared + 1] = (int)(curand_uniform(&localState) * dev_DIF) + 1;
				globalState[id] = localState;
				dev_fichaInf[1] -= 1;
			}
		}
		else {
			atomicAdd(&dev_fichaInf[0], 1);
		}
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		dev_tablero[idx] = tab_shared[pos_shared];
		dev_tablero[idx + 1] = tab_shared[pos_shared + 1];
	}
}

__global__ void eliminar_bomba(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, int aleatorio) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int pos = (fil * dev_N + col) * 2;

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[pos];
		tab_shared[pos_shared + 1] = dev_tablero[pos + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		if (aleatorio == 0) {
			if (fil == dev_coordenadas[1]) {
				tab_shared[pos_shared] = '0';
				atomicAdd(&dev_fichaInf[1], 1);
			}
		}
		else {
			if (col == dev_coordenadas[0]) {
				tab_shared[pos_shared] = '0';
				atomicAdd(&dev_fichaInf[1], 1);
			}
		}
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		dev_tablero[pos] = tab_shared[pos_shared];
		dev_tablero[pos + 1] = tab_shared[pos_shared + 1];
	}
}

__global__ void eliminar_tnt(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int pos = (fil * dev_N + col) * 2;
	int distancia = sqrt((float)pow((double)col - dev_coordenadas[0], 2.0) + pow((double)fil - dev_coordenadas[1], 2.0));

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[pos];
		tab_shared[pos_shared + 1] = dev_tablero[pos + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		if (distancia <= 4) {
			tab_shared[pos_shared] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		dev_tablero[pos] = tab_shared[pos_shared];
		dev_tablero[pos + 1] = tab_shared[pos_shared + 1];
	}
}

__global__ void eliminar_rompecabezas(char* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	__shared__ char tab_shared[TESELA_X * TESELA_Y * 2];
	int pos_shared = (threadIdx.y * blockDim.x + threadIdx.x) * 2;

	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int pos = (fil * dev_N + col) * 2;
	int touch = (dev_coordenadas[1] * dev_N + dev_coordenadas[0]) * 2;
	char elem = dev_tablero[touch + 1];

	if (dev_N > col && dev_M > fil) {
		tab_shared[pos_shared] = dev_tablero[pos];
		tab_shared[pos_shared + 1] = dev_tablero[pos + 1];
	}
	else {
		tab_shared[pos_shared] = '0';
		tab_shared[pos_shared + 1] = '0';
	}
	__syncthreads();

	__syncthreads();
	if (dev_N > col && dev_M > fil) {
		if (tab_shared[pos_shared] == elem || pos == touch) {
			tab_shared[pos_shared] = '0';
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
	__syncthreads();

	if (dev_N > col && dev_M > fil) {
		dev_tablero[pos] = tab_shared[pos_shared];
		dev_tablero[pos + 1] = tab_shared[pos_shared + 1];
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
	N = 9;					//columnas
	M = 3;					//filas
	dif = 4;
	ejecucion = 'm';

	//Optimizar dimensiones
	cudaDeviceProp deviceProp;
	cudaGetDeviceProperties(&deviceProp, 0);
	printf("\nNombre GPU: %s\n", deviceProp.name);
	int BLOCK_SIZE = sqrt(deviceProp.maxThreadsPerBlock);
	printf("\nBlock size: %d\n", BLOCK_SIZE);

	//Declaraci�n de variables
	int SIZE = N * M * 2 * sizeof(char);
	int size_coord = 2 * sizeof(int);
	int size_ficha = 3 * sizeof(int);
	int* h_coordenadas = (int*)malloc(size_coord);
	int* h_fichaInf = (int*)malloc(size_ficha);
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
	cudaMalloc((void**)&dev_fichaInf, size_ficha);
	cudaMalloc((void**)&dev_tablero, SIZE);

	dim3 blocksInGrid(BLQ_X, BLQ_Y);
	dim3 threadsInBlock(TESELA_X, TESELA_Y);

	//Inicializar tablero
	//---------------------------------------------------------------------------------------------
	h_fichaInf[0] = 0;
	h_fichaInf[1] = N * M;

	vaciar_tablero(h_tablero);
	mostrar_tablero(h_tablero);

	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_fichaInf, h_fichaInf, size_ficha, cudaMemcpyHostToDevice);

	setup_kernel << <blocksInGrid, threadsInBlock >> > (dev_states, time(0));

	while (h_fichaInf[1] != 0) {
		printf("\nHOla\n");
		bajar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero);
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);
		generar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_states, dev_fichaInf);
		
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
	}

	//Bucle principal
	//---------------------------------------------------------------------------------------------
	int ficha;
	srand(time(NULL));

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
			colocar_fichaEX << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
			int salir;
			do {
				salir = h_fichaInf[0];
				eliminar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
				cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
			} while (salir != h_fichaInf[0]);

			colocar_fichaEX << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);
		}
		else if (h_tablero[ficha] == 'B') {
			int aleatorio = rand() % 2;
			eliminar_bomba << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf, aleatorio);
		}
		else if (h_tablero[ficha] == 'T') {
			eliminar_tnt << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		else {
			eliminar_rompecabezas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);

		printf("\nEliminadas: %d\n", h_fichaInf[1]);
		if (h_fichaInf[1] == 1) vidas--;

		while (h_fichaInf[1] != 0) {
			bajar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero);
			generar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_states, dev_fichaInf);

			cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
			mostrar_tablero(h_tablero);
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_ficha, cudaMemcpyDeviceToHost);
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
