#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <stdlib.h>

const int bloques[6] = {1, 2, 3, 4, 5, 6};
__constant__ int dev_N;
__constant__ int dev_DIF;

int vidas = 0;
int eje_x = -1;
int eje_y = -1;
int N = 0;
int M = 0;
int dif;

__global__ void test_function() {
	//printf("Hola Mundo");
}

__global__ void test() {
	//printf("Only test");
}

__global__ void init_tablero(int* punteroTablero) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;

	int pos = dev_N * fila + columna;

	printf("%d - ", pos);
	punteroTablero[pos] = 1;
	if (punteroTablero[pos] == 0) {
		//punteroTablero[pos] == 1 + rand() / (RAND_MAX / (dev_DIF - 1 + 1) + 1);
	}
}

void update() {
	test_function<<<1, 10>>>();
	test <<<1, 10>>> ();
}

void vaciar_tablero(int* tablero) {
	for (int i = 0; i < N * M; i++) {
		tablero[i] = 0;
	}
}

void show_tablero(int* tablero) {
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

	//Comando ./cundy -a 2 50 10

	//Variables 
	vidas = 1;

	N = 10;
	M = 10;
	dif = 6;
	int SIZE = N * M * sizeof(int);
	int* h_tablero = (int*) malloc(SIZE);

	vaciar_tablero(h_tablero);
	show_tablero(h_tablero);

	//Punteros GPU
	int* dev_tablero;
	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMalloc((void**)&dev_tablero, SIZE);

	//Copiar los datos del host a la CPU
	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);

	dim3 blocksInGrid(1);
	dim3 threadsInBlock(N);
	init_tablero<<<blocksInGrid, threadsInBlock>>>(dev_tablero);

	cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);

	show_tablero(h_tablero);
	

	//Bucle principal
	while (vidas > 0) {

		printf("\nIntroduce el numero de columna: ");
		scanf("%d", &eje_x);
		printf("\nIntroduce el numero de fila: ");
		scanf("%d", &eje_y);

		printf("%d -- %d\n", eje_x, eje_y);

		update();

		show_tablero(h_tablero);
		vidas--;
	}


	//Liberar memoria
	cudaFree(dev_tablero);
	free(h_tablero);
	return 0;
}
