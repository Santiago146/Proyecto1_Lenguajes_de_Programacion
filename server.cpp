#include "configReader.h"
#include <sys/socket.h> // Para la creación y uso de sockets
#include <netinet/in.h> // Para definir direcciones de red y estructura sockaddr_in
#include <unistd.h> // Para funciones de cierre de sockets y procesos
#include <unordered_map>
#include <arpa/inet.h> // para manejar ips

using namespace std;

int port = 8080; // puerto por defecto en caso de error con el archivo config.txt

// Estructura para almacenar información del usuario
struct Usuario {
    std::string ip;
    int socket_fd;
};

// Mapa para almacenar los usuarios conectados
std::unordered_map<std::string, Usuario> usuarios_conectados; //guarda un username, su IP y su ID de socket

// Función para obtener la IP del cliente
std::string obtenerIP(int client_socket) {
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len); // funcion para obtener la IP del usuario
    return inet_ntoa(addr.sin_addr);
}

void handleClient(int client_socket) {
    char buffer[1024]; // buffer para los mensajes
    char username[50];
    
    std::string user_ip = obtenerIP(client_socket);
    
    // Solicitar y registrar el nombre de usuario
    send(client_socket, "Ingrese su nombre de usuario: ", 29, 0);
    recv(client_socket, username, sizeof(username), 0);
    username[strcspn(username, "\n")] = 0; // Eliminar salto de línea

    // Verificar si el usuario ya está registrado
    if (usuarios_conectados.find(username) != usuarios_conectados.end()) {
        if (usuarios_conectados[username].ip != user_ip) {
            // Si las IPs no coinciden, denegamos la conexión
            send(client_socket, "Nombre de usuario ya en uso desde otra IP. Intente otro.\n", 44, 0);
            handleClient(client_socket);
        }
    }
    
    // Registrar al usuario, IP y socket ID, auqnue ya este en uso, el socket ID es diferente y necesario para actualizar
    // username: reconocer al usuario
    // IP: reconocer direccion en la que se conecta el usuario, y si lo hace desde varios dispositivos a la vez
    // reconocer el canal de comunicacion de cada dispositivo
    Usuario nuevo_usuario = {user_ip, client_socket};
    usuarios_conectados[username] = nuevo_usuario;
    
    string welcome_message = "Bienvenido, " + std::string(username) + "!\n";
    send(client_socket, welcome_message.c_str(), welcome_message.length(), 0);

    cout << "Usuario " << username << " registrado con IP: " << user_ip << endl; // depsues de pruebas cambiar por exitosamente

    while (true) {
        memset(buffer, 0, sizeof(buffer)); // limpia el buffer
        int bytes_received = recv(client_socket, buffer, sizeof(buffer), 0); // recibe datos, y guarda los datos en el buffer y la longitud del mensaje en bytes_received

        if (bytes_received <= 0) { // Si el cliente se desconecta o hay un error
            cout << "Cliente desconectado.\n";
            break;
        }

        cout << "Mensaje recibido: " << buffer << "\n"; // imprime en la consola del server

        // Enviar resultado de la comunicacion al cliente
        const char* response = "Mensaje recibido y procesado por el servidor";
        send(client_socket, response, strlen(response), 0);
    }
    close(client_socket); // cierra el proceso del cliente
}

int main() {
    port = extractPortConfig("config.txt", port);

    int server_fd, client_socket; // guarda el resultado de la conexion, si se hizo con exito o no
    struct sockaddr_in address; // Direccion IP del servidor
    socklen_t addrlen = sizeof(address);
    char buffer[1024] = {0}; // espacio donde se almacenara el mensaje recibido

    // Crear el socket
    
    // AF_INET configura IPv4 como la familia de direcciones a usar por el socket
    // SOCK_STREAM defien el tipo de protocolo a usar como TCP. El 0 indica usar el protocolo por defecto/estandar
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("Error al crear el socket");
        exit(EXIT_FAILURE);
    }
    cout << "Socket creado exitosamente" << endl;

    // Configurar la dirección y el puerto
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY; // Indica que se aceptan conexiones en cualquier IP disponible.
    address.sin_port = htons(port); // Pasa el puerto configurado como numero a formato de red

    // Modifica el socket para permitirle reutilizar el puerto para evitar errores al reiniciar el servidor rápidamente
    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    // Enlazar el socket al puerto
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) { // bind() asigna el socket a una dirección IP y un puerto específicos.
        perror("Error al enlazar el socket");
        exit(EXIT_FAILURE);
    }
    cout << "Socket enlazado correctamente" << endl;

    // Escuchar conexiones entrantes
    if (listen(server_fd, 3) < 0) { // TEST pre frok(), esto permite que el servidor escuche hasta 3 conexiones pendientes.
        perror("Error al escuchar");
        exit(EXIT_FAILURE);
    }
    cout << "Servidor escuchando en el puerto " << port << "..." << endl;



    // Aceptar una conexión entrante
    
    // accept() espera una conexión de un cliente y cuando un cliente se conecta, devuelve un nuevo resultado de conexion de socket "client_socket"
    if ((client_socket = accept(server_fd, (struct sockaddr*)&address, &addrlen)) < 0) {
        perror("Error al aceptar la conexión");
        exit(EXIT_FAILURE);
    }

    std::cout << "Cliente conectado.\n";

    // Manejar la comunicación con el cliente
    handleClient(client_socket);

    // Cerrar conexion del server
    close(server_fd);
    printf("\n");
    return 0;
}
