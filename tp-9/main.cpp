#include <iostream>
using namespace std;
#include "Producto.h"

int main() {
  Producto producto;
  producto = crearProducto("Auriculares Logitech G435", true);       
  ShowProducto(producto);
};
