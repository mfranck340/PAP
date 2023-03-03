#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <stdlib.h>

__constant__ int dev_N;
__constant__ int dev_M;
__constant__ int dev_DIF;

int vidas = 0;
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
	int pos = (dev_N * dev_M - threadIdx.x) - 1;

	for (int i = pos; i >= dev_M; i -= dev_M) {
		if (punteroTablero[i] == 0) {
			if (punteroTablero[i - dev_M] != 0) {
				punteroTablero[i] = punteroTablero[i - dev_M];
				punteroTablero[i - dev_M] = 0;
			}
		}
	}
}

__global__ void generar_fichas(int* punteroTablero, int* punteroFichas) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;

	int pos = dev_N * fila + columna;

	if (punteroTablero[pos] == 0) {
		punteroTablero[pos] = punteroFichas[pos];
	}
}

__device__ void eliminar(int* punteroTablero, int* bloquesCoincidentes, int objetivo, int posicionActual) {
	if (bloquesCoincidentes[posicionActual] == 1 && punteroTablero[posicionActual] != 0) {
		punteroTablero[posicionActual] = 0;
	}

	//Derecha
	if ((posicionActual + 1 <= (dev_N * dev_M)) && ((posicionActual + 1) % dev_M != 0)) {
		if (bloquesCoincidentes[posicionActual + 1] == 1) {
			punteroTablero[posicionActual + 1] = 0;
			bloquesCoincidentes[posicionActual + 1] = 0;
			eliminar(punteroTablero, bloquesCoincidentes, objetivo, posicionActual + 1);
		}
	}

	//Izquierda
	if ((posicionActual - 1 >= 0) && ((posicionActual - 1) % dev_M != (dev_M - 1))) {
		if (bloquesCoincidentes[posicionActual - 1] == 1) {
			punteroTablero[posicionActual - 1] = 0;
			bloquesCoincidentes[posicionActual - 1] = 0;
			eliminar(punteroTablero, bloquesCoincidentes, objetivo, posicionActual - 1);
		}
	}

	//Abajo
	if (posicionActual + dev_M < (dev_N * dev_M)) {
		if (bloquesCoincidentes[posicionActual + dev_M] == 1) {
			punteroTablero[posicionActual + dev_M] = 0;
			bloquesCoincidentes[posicionActual + dev_M] = 0;
			eliminar(punteroTablero, bloquesCoincidentes, objetivo, posicionActual + dev_M);
		}
	}

	//Arriba
	if (posicionActual - dev_M >= 0) {
		if (bloquesCoincidentes[posicionActual - dev_M] == 1) {
			punteroTablero[posicionActual - dev_M] = 0;
			bloquesCoincidentes[posicionActual - dev_M] = 0;
			eliminar(punteroTablero, bloquesCoincidentes, objetivo, posicionActual - dev_M);
		}
	}
}

__global__ void eliminar_fichas(int* punteroTablero, int* coordenadas, int* bloquesCoincidentes) {
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;

	int pos = dev_N * fila + columna;
	int objetivo = dev_N * coordenadas[0] + coordenadas[1];

	int bloque = punteroTablero[objetivo];

	bool borrar = false;

	if (punteroTablero[pos] == bloque) {
		printf("%d\n", pos);
		bloquesCoincidentes[pos] = 1;
	}

	__syncthreads();

	if (pos == objetivo) {
		eliminar(punteroTablero, bloquesCoincidentes, objetivo, objetivo);
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
	vidas = 3;

	N = 10;
	M = 10;
	dif = 6;
	int SIZE = N * M * sizeof(int);
	int size_fila = M * sizeof(int);
	int size_coordenadas = 2 * sizeof(int);
	int* h_tablero = (int*) malloc(SIZE);
	int* h_fichas = (int*) malloc(size_fila);
	int* h_coordenadas = (int*) malloc(size_coordenadas);
	int* h_bloquesCoincidentes = (int*) malloc(SIZE);

	generar_random(h_fichas);

	vaciar_tablero(h_tablero);
	mostrar_tablero(h_tablero);

	for (int i = 0; i < (N * M); i++) {
		h_bloquesCoincidentes[i] = 0;
	}

	//Punteros GPU
	int* dev_tablero;
	int* dev_fichas;
	int* dev_coordenadas;
	int* dev_bloquesCoincidentes;
	cudaMemcpyToSymbol(dev_DIF, &dif, sizeof(int));
	cudaMemcpyToSymbol(dev_N, &N, sizeof(int));
	cudaMemcpyToSymbol(dev_M, &M, sizeof(int));
	cudaMalloc((void**)&dev_tablero, SIZE);
	cudaMalloc((void**)&dev_fichas, size_fila);
	cudaMalloc((void**)&dev_bloquesCoincidentes, SIZE);

	//Copiar los datos del host a la CPU
	cudaMemcpy(dev_tablero, h_tablero, SIZE, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_fichas, h_fichas, size_fila, cudaMemcpyHostToDevice);
	cudaMemcpy(dev_bloquesCoincidentes, h_bloquesCoincidentes, SIZE, cudaMemcpyHostToDevice);

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
		scanf("%d", &h_coordenadas[0]);
		printf("\nIntroduce el numero de fila: ");
		scanf("%d", &h_coordenadas[1]);

		cudaMalloc((void**)&dev_coordenadas, size_coordenadas);

		cudaMemcpy(dev_coordenadas, h_coordenadas, size_coordenadas, cudaMemcpyHostToDevice);
		cudaMemcpy(dev_bloquesCoincidentes, h_bloquesCoincidentes, SIZE, cudaMemcpyHostToDevice);

		dim3 bloques(1);
		dim3 hilos(N, M);

		eliminar_fichas<<<bloques, hilos>>>(dev_tablero, dev_coordenadas, dev_bloquesCoincidentes);

		cudaMemcpy(h_tablero, dev_tablero, SIZE, cudaMemcpyDeviceToHost);
		cudaMemcpy(h_bloquesCoincidentes, dev_bloquesCoincidentes, SIZE, cudaMemcpyDeviceToHost);

		for (int i = 0; i < (N * M); i++) {
			h_bloquesCoincidentes[i] = 0;
		}
		
		printf("\n");
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
