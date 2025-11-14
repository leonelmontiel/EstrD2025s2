#include <iostream>
#include "Ejercicio4.h"
using namespace std;

/* 
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando  la menor cantidad posible de variables. Recordar de nir subtareas en caso de que sea estrictamente necesario.
 */

 //Propósito: imprime n veces un string s.
void printNIt(int n, string s) {
    while (n > 0) {
        cout << s << endl;
        n--;
    }
}
void printNRec(int n, string s) {
    if (n > 0) {
        cout << s << endl;
        printNRec(n-1, s);
    }
}

//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresivaIt(int n) {
    while (n >= 0) {
        cout << n << endl;
        n--;
    }
}
void cuentaRegresivaRec(int n) {
    if (n >= 0) {
        cout << n << endl;
        cuentaRegresivaRec(n-1);
    }
}

//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaNIt(int n) {
    int m = 0;
    while (m <= n) {
        cout << m << endl;
        m++;
    }
}
void desdeMHastaN(int m, int n) { // AUXILIAR
    if (m <= n) {
        cout << m << endl;
        desdeMHastaN(m+1, n);
    }
}
void desdeCeroHastaNRec(int n) {
    int m = 0;
    desdeMHastaN(m, n);
}

int multIt(int n, int m) {
    int current = 0;
    while (m > 0) {
        current += n;
        m--;
    }
    while (m < 0) {
        current -= n;
        m ++;
    }
    return current;
}
int multRec(int n, int m) {
    if (m > 0) {
        return n + multRec(n, m-1);        
    } else if (m < 0) {
        return - multRec(n, -m);
    }
    return 0;
}
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

void primerosNIt(int n, string s) {
    try {
        if (s.length() >= n) {
            for (int i = 0; i < n; i++) {
                cout << s[i] << endl;
            }
        } else {
            throw runtime_error("Error: La cadena de texto es menor a la cantidad de caracteres a imprimir.");
        }
    }
    catch (const runtime_error& error) {
        // Muestra el mensaje de error
        cerr << error.what() << endl;
    }
}
void imprimirNChars(int i, int n, string s) {
    if (i < n) {
        cout << s[i] << endl;
        imprimirNChars(i+1, n, s);
    }
}
void primerosNRec(int n, string s) {
    try {
        if (s.length() >= n) {
            imprimirNChars(0, n, s);
        } else {
            throw runtime_error("Error: La cadena de texto es menor a la cantidad de caracteres a imprimir.");
        }
    }
    catch (const runtime_error& error) {
        // Muestra el mensaje de error
        cerr << error.what() << endl;
    }
}
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.

bool perteneceIt(char c, string s) {
    for (int i = 0; i < s.length(); i++) {
        if (c == s[i]) {
            return true;
        }
    }
    return false;
}
bool perteneceA(int i, char c, string s) {
    if (i < s.length()) {
        return c == s[i] ? true : perteneceA(i+1, c, s);
    }
}
bool perteneceRec(char c, string s) {
    return perteneceA(0, c, s);    
}
//Propósito: indica si un char c aparece en el string s.

int aparicionesIt(char c, string s) {
    int apariciones = 0;
    for (int i = 0; i < s.length(); i++) {
        apariciones += c == s[i] ? 1 : 0;
    }
    return apariciones;
}
int unoSi(char c1, char c2) {
    return c1 == c2 ? 1 : 0;
} 
int contarApariciones(int i, char c, string s) {
    if (i < s.length()) {
        return unoSi(c, s[i]) + contarApariciones(i+1, c, s);
    }
    return 0;
}
int aparicionesRec(char c, string s) {
    int i = 0;
    return contarApariciones(i, c, s);
}
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.