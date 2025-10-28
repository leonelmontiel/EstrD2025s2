#include <iostream>
#include "Producto.h"
using namespace std;

Producto crearProducto(string n, bool s) {
    Producto p;
    p.nombre = n;
    p.enStock = s;
    return (p);
};

string obtenerNombre(Producto p) {
    return (p.nombre);
};

bool tieneStock(Producto p) {
    return (p.enStock);
};

void ShowProducto(Producto p) {
     cout << "Producto(";
     cout << "nombre <- \"" << p.nombre << "\", ";
     cout << "enStock <- " << p.enStock;
     cout << ")" << endl;
};