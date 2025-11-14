#include <iostream>
using namespace std;

/* Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando la menor cantidad posible de variables. Recordar de nir subtareas en caso de que sea estrictamente necesario.*/

void printNIt(int n, string s);
void printNRec(int n, string s);
//Propósito: imprime n veces un string s.

void cuentaRegresivaIt(int n);
void cuentaRegresivaRec(int n);
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.

void desdeCeroHastaNIt(int n);
void desdeCeroHastaNRec(int n);
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.

int multIt(int n, int m);
int multRec(int n, int m);
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

void primerosNIt(int n, string s);
void primerosNRec(int n, string s);
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.

bool perteneceIt(char c, string s);
bool perteneceRec(char c, string s);
//Propósito: indica si un char c aparece en el string s.

int aparicionesIt(char c, string s);
int aparicionesRec(char c, string s);
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.