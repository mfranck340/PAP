#include <stdio.h>
#include <stdlib.h>
#include "cuda_runtime.h"

#include <math.h>

const int N = 0;
const int M = 0;

__global__ void inicializarTablero(int* punteroTablero){
	int columna = (blockIdx.x * blockDim.x) + threadIdx.x;
	int fila = (blockIdx.y * blockDim.y) + threadIdx.y;
	


	punteroTablero[N * fila + columna] = 0;
}

int main(int argc, char** argv)
{
	//Crear tablero
	int tablero[argv[3]][argv[4]];
	
	//Punteros
	int* punteroTablero;

	//Reservar memoria para los punteros
    int tamano = N * M * sizeof(int);
    cudaMalloc((void**) &punteroTablero, tamano);

	//Copiar los datos del host a la CPU
    cudaMemcpy(punteroTablero, tablero, tamano, cudaMemcpyHostToDevice);

	//Lanzar el kernel
	dim3 numeroDeBloques(1);
	dim3 hilosEnBloque(N, M);
    inicializarTablero<<<numeroDeBloques, hilosEnBloque>>>(punteroTablero);

	//Recuperar datos de la GPU
    cudaMemcpy(tablero, punteroTablero, tamano, cudaMemcpyDeviceToHost);
    
	//Mostrar resultado
	printf("\nResultado:(\n");
	for (int i = 0; i < N; i++) {
		for (int j = 0; j < M; j++) {
			printf("%d,", tablero[i][j]);
		}
		printf("\n");
    }
    printf(")\n");

	//Liberar memoria GPU
    cudaFree(tablero);
	
	//Salida del programa
	printf("\nPulsa INTRO para finalizar...");
	fflush(stdin);
	char tecla = getchar();
	return 0;
}

