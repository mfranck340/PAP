#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <stdlib.h>

const int bloques[6] = {1, 2, 3, 4, 5, 6};

__global__ void test_function() {
	printf("Hola Mundo");
}

__global__ void test() {
	printf("Only test");
}

__global__ void init_tablero(int* punteroTablero) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;
}

void update() {
	test_function<<<1, 10>>>();
	test <<<1, 10>>> ();
}

void vaciar_tablero(int* tablero, int N, int M) {
	for (int i = 0; i < N * M; i++) {
		tablero[i] = 0;
	}
}

void show_tablero(int* tablero, int N, int M) {
	for (int i = 0; i < N; i++) {
		printf("\n| ");
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
	int vidas = 1;
	int eje_x = -1;
	int eje_y = -1;

	int N = 10;
	int M = 10;
	int SIZE = N * M * sizeof(int);
	int* h_tablero = (int*) malloc(SIZE);

	vaciar_tablero(h_tablero, N, M);
	show_tablero(h_tablero, N, M);

	//Punteros GPU
	int* dev_tablero;
	cudaMalloc((void**)&dev_tablero, SIZE);

	//Copiar los datos del host a la CPU
	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);

	dim3 blocksInGrid(1);
	dim3 threadsInBlock(N, M);

	//init_tablero<<<blocksInGrid, threadsInBlock>>>();
	

	//Bucle principal
	while (vidas > 0) {

		printf("Introduce el numero de columna: ");
		scanf("%d", &eje_x);
		printf("Introduce el numero de fila: ");
		scanf("%d", &eje_y);

		printf("%d -- %d\n", eje_x, eje_y);

		update();

		show_tablero(h_tablero, N, M);
		vidas--;
	}


	//Liberar memoria
	cudaFree(dev_tablero);
	free(h_tablero);
	return 0;
}
