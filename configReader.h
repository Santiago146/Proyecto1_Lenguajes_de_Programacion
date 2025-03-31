#include <iostream>
#include <cstring>
#include <fstream>

using namespace std;

int extractPortConfig(string filename, int port) { // en port se pasa el puerto por defecto que es el 8080
    ifstream configFile(filename);
    string line;

    if (configFile.is_open()) {
        while (getline(configFile, line)) {
            if (line.rfind("PORT=", 0) == 0) {  // Buscar "PORT=" al inicio de la línea
                port = std::stoi(line.substr(5));  // Convertir el valor a entero
                break;
            }
        }
        configFile.close();
    } // en caso de fallo en la lectura se usara el puerto por defecto

    return port;
}