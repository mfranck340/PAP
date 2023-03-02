#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <stdlib.h>

__constant__ int dev_N;
__constant__ int dev_M;
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

void generar_random(int* punteroFichas) {
	for (int i = 0; i < M; i++) {
		punteroFichas[i] = 1 + rand() % 6;
	}
}

__global__ void bajar_fichas(int* punteroTablero) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;

	int pos = (dev_N * dev_M - threadIdx.x) - 1;

	for (int i = pos; i >= dev_M; i -= dev_M) {
		if (punteroTablero[i] == 0) {
			if (punteroTablero[i - dev_M] != 0) {
				punteroTablero[i] = punteroTablero[i - dev_M];
				punteroTablero[i - dev_M] = 0;
			}
		}
	}

	printf("%d. %d\n", threadIdx.x, pos);
}

__global__ void generar_fichas(int* punteroTablero, int* punteroFichas) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;

	int pos = dev_N * fila + columna;

	if (punteroTablero[pos] == 0) {
		punteroTablero[pos] = punteroFichas[pos];
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

	//Comando ./cundy -a 2 50 10

	//Variables 
	vidas = 1;

	N = 10;
	M = 10;
	dif = 6;
	int SIZE = N * M * sizeof(int);
	int size_fila = M * sizeof(int);
	int* h_tablero = (int*) malloc(SIZE);
	int* h_fichas = (int*) malloc(size_fila);

	generar_random(h_fichas);

	vaciar_tablero(h_tablero);
	mostrar_tablero(h_tablero);

	//Punteros GPU
	int* dev_tablero;
	int* dev_fichas;
	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));
	cudaMalloc((void**)&dev_tablero, SIZE);
	cudaMalloc((void**)&dev_fichas, SIZE);

	//Copiar los datos del host a la CPU
	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_fichas, h_fichas, size_fila, cudaMemcpyHostToDevice);

	dim3 blocksInGrid(1);
	dim3 threadsInBlock(N);

	for (int i = 0; i < N; i++) {
		generar_fichas<<<blocksInGrid, threadsInBlock>>>(dev_tablero, dev_fichas);

		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);

		mostrar_tablero(h_tablero);

		cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);

		bajar_fichas<<<blocksInGrid, threadsInBlock>>>(dev_tablero);

		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);

		mostrar_tablero(h_tablero);

		generar_random(h_fichas);
		cudaMemcpy(dev_fichas, h_fichas, size_fila, cudaMemcpyHostToDevice);
	}

	//Bucle principal
	while (vidas > 0) {
		printf("\nIntroduce el numero de columna: ");
		scanf("%d", &eje_x);
		printf("\nIntroduce el numero de fila: ");
		scanf("%d", &eje_y);

		printf("%d -- %d\n", eje_x, eje_y);

		update();

		mostrar_tablero(h_tablero);
		vidas--;
	}

	//Liberar memoria
	cudaFree(dev_tablero);
	free(h_tablero);

	//Salida del programa
	printf("\nPulsa INTRO para finalizar...");
	fflush(stdin);
	char tecla = getchar();
	return 0;
}
