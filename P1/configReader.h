#ifndef CONFIG_READER_H
#define CONFIG_READER_H

#include <string>
#include <fstream>
#include <iostream>
#include <sstream>

// Función existente para el servidor
int extractPortConfig(const std::string& filename, int defaultPort) {
    std::ifstream file(filename);
    std::string line;
    int port = defaultPort;
    
    if (file.is_open()) {
        while(getline(file, line)) {
            if(line.substr(0, 5) == "PORT=") {
                port = std::stoi(line.substr(5));
                break;
            }
        }
        file.close();
        std::cout << "Puerto configurado: " << port << std::endl;
        return port;
    } else {
        std::cout << "No se pudo abrir el archivo de configuración. Usando puerto por defecto: " << defaultPort << std::endl;
        return defaultPort;
    }
}

// Función modificada para el cliente
void extractClientConfig(const std::string& filename, std::string& ip, int& port) {
    std::ifstream file(filename);
    std::string line;
    
    if (file.is_open()) {
        while(getline(file, line)) {
            if(line.substr(0, 5) == "PORT=") {
                port = std::stoi(line.substr(5));
            }
            else if(line.substr(0, 3) == "IP=") {
                ip = line.substr(3);
            }
        }
        file.close();
        std::cout << "Configuración cargada: IP=" << ip << ", Puerto=" << port << std::endl;
    } else {
        std::cout << "No se pudo abrir el archivo de configuración. Usando valores por defecto: IP=" 
                  << ip << ", Puerto=" << port << std::endl;
    }
}

#endif