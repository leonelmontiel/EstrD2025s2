#include <iostream>
#include "Par.h"
using namespace std;

 /* Precondicion: c1 < c2
  Propósito: muestra por consola los valores ASCII intermedios entre el primer char y el segundo, dados por parámetro. */ 
 void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
 }
 /*
 c1 = 'a'
 c2 = 'f'

        |   102   102   5   |
        ---------------------
        |   101   102   4   |
        |   100   102   3   |
        |   99    102   2   |
        |   98    102   1   |
        |   97    102   0   |
        =====================
 main   |  c1    c2     i   |
 
 */

 /* Precondición: n >= 0
  Propósito: retorna el factorial del número dado por parámetro. */
 int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
 }

 /*
 n = 3

        |   0   6  |
        ------------
        |   1   6  |
        |   2   3  |
        |   3   1  |
        ========== =
 main   |  n    x  |
 
 */

/* Precondición: n <= m
Propósito: calcula la suma de los enteros desde n hasta m, ambos inclusive. Es decir, devuelve n + (n+1) + ... + m */
 int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
 }
 /*
n = 2
m = 5    
         |  14   5  |
         ------------
ft3      |  5    5  |
ft2      |  4    5  |
ft1      |  3    5  |
ft0      |  2    5  |
         =============
         |  2    5  |
         =============
 main    |  n    m  |
 
 */

 int main() {
    Par par = consPar(5, 6);
    cout << "x = " << fst(par) << endl;
    cout << "y = " << snd(par) << endl;
    cout << "El máximo entre x e y es = " << maxDelPar(par) << endl;
    par = swap(par);
    cout << "Luego del intercambio, ahora x es " << fst(par) << " e y es " << snd(par) << endl;
    par = divisionYResto(5, 6);
    cout << "Nuevo par en donde la primera componente es la división = " << fst(par) << " y la segunda es su resto = " << snd(par) << endl;
    return 0;
}
 