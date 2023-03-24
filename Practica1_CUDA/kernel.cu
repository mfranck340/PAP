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

__global__ void eliminar_fichas(int* dev_tablero, int* dev_tabAuxiliar, int* dev_coordenadas,  int* dev_fichaInf) {
	int col = blockIdx.x * blockDim.x + threadIdx.x;
	int fil = blockIdx.y * blockDim.y + threadIdx.y;
	int idx = fil * blockDim.x + col;
	int touch = dev_coordenadas[1] * blockDim.x + dev_coordenadas[0];

	__shared__ int count[2];
	count[0] = 0;
	__syncthreads();

	if (idx == touch) {
		dev_tabAuxiliar[idx] = 0;
	}
	else if (dev_tablero[idx] == dev_tablero[touch]) {
		atomicAdd(&count[0], 1);
		//dev_tabAuxiliar[idx] = -1;
		dev_tabAuxiliar[idx] = dev_tablero[idx];
	}
	else {
		dev_tabAuxiliar[idx] = dev_tablero[idx];
	}

	__syncthreads();
	count[1] = count[0];

	bool encontrado = false;
	do {
		count[0] = count[1];
		__syncthreads();
		if (dev_tablero[idx] == dev_tablero[touch] && dev_tabAuxiliar[idx] != 0) {
			if (col + 1 < blockDim.x && !encontrado) {
				if (dev_tabAuxiliar[idx + 1] == 0) {
					encontrado = true;
				}
			}
			if (col - 1 >= 0 && !encontrado) {
				if (dev_tabAuxiliar[idx - 1] == 0) {
					encontrado = true;
				}
			}
			if (fil + 1 < blockDim.y && !encontrado) {
				if (dev_tabAuxiliar[idx + blockDim.x] == 0) {
					encontrado = true;
				}
			}
			if (fil - 1 >= 0 && !encontrado) {
				if (dev_tabAuxiliar[idx - blockDim.x] == 0) {
					encontrado = true;
				}
			}
			if (encontrado) {
				printf("entro");
				atomicSub(&count[1], 1);
				atomicAdd(&dev_fichaInf[1], 1);
				dev_tabAuxiliar[idx] = 0;
			}
		}
		__syncthreads();
	} while (count[0] != count[1]);

	/*if (idx == touch) {
		if (dev_fichaInf[1] + 1 < 5) {
			dev_tabAuxiliar[idx] = 0;
			atomicAdd(&dev_fichaInf[1], 1);
		}
		else if (dev_fichaInf[1] + 1 == 5) dev_tabAuxiliar[idx] = 8;
		else if (dev_fichaInf[1] + 1 == 6) dev_tabAuxiliar[idx] = 9;
		else if (dev_fichaInf[1] + 1 >= 7) { 

			dev_tabAuxiliar[idx] = 10 + rand;
		}
	}*/
	//if (dev_tabAuxiliar[idx] == -1)
		//dev_tabAuxiliar[idx] = ;
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
			printf("%d | ", tablero[(i * N) + j]);
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
	int* dev_resultado;
	int* dev_coordenadas;
	int* dev_fichaInf;
	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));
	cudaMalloc((void**)&dev_tablero, SIZE);
	cudaMalloc((void**)&dev_resultado, SIZE);
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
	

	while (vidas > 0) {
		printf("\nIntroduce el numero de columna: ");
		scanf("%d", &h_coordenadas[0]);
		printf("\nIntroduce el numero de fila: ");
		scanf("%d", &h_coordenadas[1]);

		h_fichaInf[0] = h_coordenadas[1] * M + h_coordenadas[0];
		
		cudaMemcpy(dev_coordenadas, h_coordenadas, size_coord, cudaMemcpyHostToDevice);
		dim3 bloques(1);
		dim3 hilos(N, M);
		
		//if (h_tablero[h_fichaInf[0]] < 7)
		//obtener_ficha<<<bloques, hilos>>>(dev_tablero, dev_fichaInf);

		eliminar_fichas<<<bloques, hilos>>> (dev_tablero, dev_resultado, dev_coordenadas, dev_fichaInf);
		cudaMemcpy(dev_tablero, dev_resultado, SIZE, cudaMemcpyDeviceToDevice);
		cudaMemcpy(h_tablero, dev_resultado, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_fichaInf, dev_fichaInf, size_coord, cudaMemcpyDeviceToHost);
		mostrar_tablero(h_tablero);
		printf("/n--%d--/n", h_fichaInf[1]);
		vidas--;
	}
	//---------------------------------------------------------------------------------------------

	//Liberar memoria
	cudaFree(dev_tablero);
	cudaFree(dev_resultado);
	cudaFree(dev_coordenadas);
	cudaFree(dev_fichaInf);
	cudaFree(dev_states);
	free(h_tablero);
	free(h_coordenadas);
	free(h_fichaInf);

	//Salida del programa
	printf("\nPulsa INTRO para finalizar...");
	fflush(stdin);
	char tecla = getchar();
	return 0;
}
