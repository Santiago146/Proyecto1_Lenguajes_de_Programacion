#include "configReader.h"
#include <sys/socket.h>
#include <arpa/inet.h> // Para inet_pton (convertir direcciones IP)
#include <unistd.h>

using namespace std;

int port = 8080;  // Puerto en el que está escuchando el servidor

int main() {
    port = extractPortConfig("config.txt", port);

    int client_fd;
    struct sockaddr_in serv_addr;
    char buffer[1024] = {0};

    // Crear el socket
    if ((client_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) { // Crea un socket IPv4/TCP.
        perror("Error al crear el socket");
        return -1;
    }
    // Configuracion de la direccion del servidor
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = INADDR_ANY;  // Se conecta al localhost por defecto

    // agregar do while para reintentar conexiones al salir

    // Conectar al servidor
    if (connect(client_fd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) { // connect intenta conectarse al servidor, y guarda el resultado en "client_fd"
        perror("Conexión fallida");
        return -1;
    }

    cout << "Conectado al servidor.\n" << endl;

    // Enviar el nombre de usuario antes de comenzar a chatear
    while (true) {
        cout << "Ingrese nombre de usuario: ";
        cin.getline(buffer, sizeof(buffer));

        if (send(client_fd, buffer, strlen(buffer), 0) == -1) {
            perror("Error al enviar el username");
            close(client_fd);
            return -1;
        }

        memset(buffer, 0, sizeof(buffer));
        int bytes_received = recv(client_fd, buffer, sizeof(buffer), 0);
        if (bytes_received <= 0) {
            cout << "El servidor cerró la conexión." << endl;
            close(client_fd);
            return -1;
        }

        cout << "Servidor: " << buffer << endl;

        if (strstr(buffer, "Bienvenido") != nullptr) {
            break; // nombre aceptado
        }
    }

    // Enviar y recibir mensajes en un bucle
    cout << "Presione (Ctrl + C) para salir" << endl << endl;
    while (true) {
        memset(buffer, 0, sizeof(buffer));
        int bytes_received = recv(client_fd, buffer, sizeof(buffer), 0);
        if (bytes_received <= 0) {
            cout << "Servidor desconectado" << endl;
            break;
        }
        if (strcmp(buffer, "Servidor cerrado.\n") == 0) {
            std::cout << "[Servidor] se ha cerrado. Saliendo del programa...\n";
            break; // o exit(0);
        }
        
        cout << "Respuesta del servidor: " << buffer << endl << endl;
    }

    // Cerrar conexión
    close(client_fd);
    printf("\n");
    return 0;
}
