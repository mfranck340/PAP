#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <curand_kernel.h>
#include <stdio.h>
#include <stdlib.h>

__constant__ int dev_N;
__constant__ int dev_M;
__constant__ int dev_DIF;

int vidas = 0;
int N = 0;
int M = 0;
int dif;

__global__ void setup_kernel(curandState* state, unsigned long seed) {
	int id = threadIdx.x;
	curand_init(seed, id, 0, &state[id]);
}

__global__ void generar_fichas(int* dev_tablero, curandState* globalState, int* dev_fichasInf) {
	int pos = (blockIdx.x * blockDim.x) + threadIdx.x;

	if (dev_tablero[pos] == 0) {
		int idx = threadIdx.x;
		curandState localState = globalState[idx];
		float r = (curand_uniform(&localState) * dev_DIF) + 1;
		globalState[idx] = localState;
		dev_tablero[pos] = r;

		atomicSub(&dev_fichasInf[1], 1);
	}
}

__global__ void bajar_fichas(int* dev_tablero) {
	int pos = (dev_N * dev_M - threadIdx.x) - 1;

	for (int i = pos; i >= dev_M; i -= dev_M) {
		if (dev_tablero[i] == 0) {
			if (dev_tablero[i - dev_M] != 0) {
				dev_tablero[i] = dev_tablero[i - dev_M];
				dev_tablero[i - dev_M] = 0;
			}
		}
	}
}

__global__ void eliminar_fichas(int* dev_tablero, int* dev_coordenadas,  int* dev_fichaInf, curandState* globalState) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int idx = fil * blockDim.x + col;
	int touch = dev_coordenadas[1] * blockDim.x + dev_coordenadas[0];
	int elem = dev_tablero[touch];

	__shared__ int count[2];
	count[0] = 0;
	__syncthreads();

	if (idx == touch) {
		dev_tablero[idx] = 0;
		atomicAdd(&dev_fichaInf[1], 1);
	}
	else if (dev_tablero[idx] == dev_tablero[touch]) {
		atomicAdd(&count[0], 1);
	}

	__syncthreads();
	count[1] = count[0];

	bool encontrado = false;
	do {
		count[0] = count[1];
		__syncthreads();
		if (dev_tablero[idx] == elem && dev_tablero[idx] != 0) {
			if (col + 1 < blockDim.x && dev_tablero[idx + 1] == 0) {
				encontrado = true;
			}
			else if (col - 1 >= 0 && dev_tablero[idx - 1] == 0) {
				encontrado = true;
			}
			else if (fil + 1 < blockDim.y && dev_tablero[idx + blockDim.x] == 0) {
				encontrado = true;
			}
			else if (fil - 1 >= 0 && dev_tablero[idx - blockDim.x] == 0) {
				encontrado = true;
			}

			if (encontrado) {
				atomicSub(&count[1], 1);
				atomicAdd(&dev_fichaInf[1], 1);
				dev_tablero[idx] = 0;
			}
		}
		__syncthreads();
	} while (count[0] != count[1]);

	if (idx == touch) {
		if (dev_fichaInf[1] == 5) {
			dev_tablero[idx] = 8;
			atomicSub(&dev_fichaInf[1], 1);
		}
		else if (dev_fichaInf[1] == 6) {
			dev_tablero[idx] = 9;
			atomicSub(&dev_fichaInf[1], 1);
		}	
		else if (dev_fichaInf[1] >= 7) {
			int id = threadIdx.x;
			curandState localState = globalState[id];
			float r = (curand_uniform(&localState) * dev_DIF) + 1;
			globalState[id] = localState;
			dev_tablero[idx] = 10 + r;

			atomicSub(&dev_fichaInf[1], 1);
		}
	}
}

__global__ void eliminar_bomba(int* dev_tablero, int* dev_coordenadas, int* dev_fichaInf, curandState* globalState) {
	int pos = threadIdx.y * blockDim.x + threadIdx.x;
	if (pos == 0) {
		int id = threadIdx.x;
		curandState localState = globalState[id];
		float r = (curand_uniform(&localState) * 2);
		globalState[id] = localState;
		dev_fichaInf[0] = r;
	}
	__syncthreads();

	if (dev_fichaInf[0] == 0) {
		if (threadIdx.y == dev_coordenadas[1]) {
			dev_tablero[pos] = 0;
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
	else {
		if (threadIdx.x == dev_coordenadas[0]) {
			dev_tablero[pos] = 0;
			atomicAdd(&dev_fichaInf[1], 1);
		}
	}
}

__global__ void eliminar_tnt(int* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int pos = threadIdx.y * blockDim.x + threadIdx.x;
	int distancia = sqrt((float)pow((double)threadIdx.x - dev_coordenadas[0], 2.0) + pow((double)threadIdx.y - dev_coordenadas[1], 2.0));

	if (distancia <= 4) {
		dev_tablero[pos] = 0;
		atomicAdd(&dev_fichaInf[1], 1);
	}
}

__global__ void eliminar_rompecabezas(int* dev_tablero, int* dev_coordenadas, int* dev_fichaInf) {
	int pos = threadIdx.y * blockDim.x + threadIdx.x;
	int touch = dev_coordenadas[1] * blockDim.x + dev_coordenadas[0];
	int elem = dev_tablero[touch] % 10;

	__syncthreads();

	if (dev_tablero[pos] == elem || pos == touch) {
		dev_tablero[pos] = 0;
		atomicAdd(&dev_fichaInf[1], 1);
	}
}

void init_tablero() {

}
void update() {

}

void vaciar_tablero(int* tablero) {
	for (int i = 0; i < N * M; i++) {
		tablero[i] = 0;
	}
}

void mostrar_tablero(int* tablero) {
	for (int i = 0; i < N; i++) {
		printf("\n\n| ");
		for (int j = 0; j < M; j++) {
			int ficha = tablero[(i * N) + j];
			if (ficha == 8) printf("B | ");
			else if (ficha == 9) printf("T | ");
			else if (ficha > 10) {
				int fichaAux = ficha % 10;
				printf("R%d | ", fichaAux);
			}
			else printf("%d | ", ficha);
		}
		printf("\n");
		for (int j = 0; j < M; j++) {
			printf(" ---");
		}
	}
}

int main(int argc, const char* argv[]) {
	cudaFree(0);
	//Variables 
	vidas = 5;
	N = 10;
	M = 10;
	dif = 6;
	int SIZE = N * M * sizeof(int);
	int size_coord = 2 * sizeof(int);
	int* h_tablero = (int*)malloc(SIZE);
	int* h_coordenadas = (int*)malloc(size_coord);
	int* h_fichaInf = (int*)malloc(size_coord);			//0 --> nombre ficha, 1 --> fichas eliminadas
	h_fichaInf[0] = 0;
	h_fichaInf[1] = N * M;

	//Punteros GPU
	curandState* dev_states;
	int* dev_tablero;
	int* dev_coordenadas;
	int* dev_fichaInf;
	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));
	cudaMalloc((void**)&dev_tablero, SIZE);
	cudaMalloc((void**)&dev_coordenadas, size_coord);
	cudaMalloc((void**)&dev_fichaInf, size_coord);
	cudaMalloc(&dev_states, N * sizeof(curandState));

	//Inicializar tablero
	//---------------------------------------------------------------------------------------------

	vaciar_tablero(h_tablero);
	mostrar_tablero(h_tablero);
	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_fichaInf, h_fichaInf, size_coord, cudaMemcpyHostToDevice);
	dim3 blocksInGrid(1);
	dim3 threadsInBlock(N);
	setup_kernel<<<blocksInGrid, threadsInBlock>>>(dev_states, time(0));

	while (h_fichaInf[1] != 0) {
		generar_fichas<<<blocksInGrid, threadsInBlock>>>(dev_tablero, dev_states, dev_fichaInf);
		
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		//mostrar_tablero(h_tablero);
		//printf("\n-----------------------------------------------------------\n");
		bajar_fichas<<<blocksInGrid, threadsInBlock>>>(dev_tablero);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
	}
	//---------------------------------------------------------------------------------------------
	printf("\n|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||\n");
	mostrar_tablero(h_tablero);

	//Bucle principal
	//---------------------------------------------------------------------------------------------
	int ficha;
	while (vidas > 0) {
		printf("\nIntroduce el numero de columna: ");
		scanf("%d", &h_coordenadas[0]);
		printf("\nIntroduce el numero de fila: ");
		scanf("%d", &h_coordenadas[1]);

		ficha = h_coordenadas[1] * M + h_coordenadas[0];
		
		cudaMemcpy(dev_coordenadas, h_coordenadas, size_coord, cudaMemcpyHostToDevice);
		dim3 bloques(1);
		dim3 hilos(N, M);
		
		if (h_tablero[ficha] < 7) {
			eliminar_fichas <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);
		}
		else if (h_tablero[ficha] == 8) {
			eliminar_bomba <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf, dev_states);
		}
		else if (h_tablero[ficha] == 9) {
			eliminar_tnt <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		else {
			eliminar_rompecabezas <<<bloques, hilos>>> (dev_tablero, dev_coordenadas, dev_fichaInf);
		}
		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);
		while (h_fichaInf[1] != 0) {
			generar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero, dev_states, dev_fichaInf);

			cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
			mostrar_tablero(h_tablero);
			printf("\n-----------------------------------------------------------\n");
			bajar_fichas << <blocksInGrid, threadsInBlock >> > (dev_tablero);
			cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
		}

		vidas--;
	}
	//---------------------------------------------------------------------------------------------

	//Liberar memoria
	cudaFree(dev_tablero);
	cudaFree(dev_coordenadas);
	cudaFree(dev_fichaInf);
	cudaFree(dev_states);
	free(h_tablero);
	free(h_coordenadas);
	free(h_fichaInf);

}
