#include <iostream>
#include "Fraccion.h"
using namespace std;

void validarDenominador(int denominador) {
    if (denominador == 0) {
        throw runtime_error("Error: El denominador no puede ser 0.");
    }
}
// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    validarDenominador(denominador);
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return (float) f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    int num = f1.numerador * f2.numerador;
    int den = f1.denominador * f2.denominador;
    return consFraccion(num, den);
}

// Próposito: devuelve el máximo común divisor
int mcd(int a, int b) {
    if (b == 0) return abs(a);
    return mcd(b, a % b);
}
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p) {
    int divisor = mcd(p.numerador, p.denominador);
    p.numerador /= divisor;
    p.denominador /= divisor;
    return p;    
}

// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2) {
    int num1 = f1.numerador;
    int num2 = f2.numerador;
    int den1 = f1.denominador;
    int den2 = f2.denominador;
    int nuevoNum;
    int nuevoDen;

    if (den1 == den2) {
        nuevoNum = num1 + num2;
        nuevoDen = den1;
    } else {
        nuevoNum = (num1 * den2) + (num2 * den1);
        nuevoDen = den1 * den2;
    }
    return simplificada(consFraccion(nuevoNum, nuevoDen));
}