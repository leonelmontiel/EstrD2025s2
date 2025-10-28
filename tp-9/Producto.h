#include <iostream>
using namespace std;

struct RegistroDeProd {
  string nombre;
  bool enStock;
};

typedef struct RegistroDeProd Producto;

Producto crearProducto(string n, bool s);
//Propósito: Crea un nuevo producto.
//Retorna: Un Producto con los datos proporcionados.

string obtenerNombre(Producto p);
//Propósito: Devuelve el nombre del producto.
//Retorna: Una cadena de caracteres.

bool tieneStock(Producto p);
//Propósito: Indica si el producto tiene stock.
//Retorna: true si hay stock, false en caso contrario.

void ShowProducto(Producto p);