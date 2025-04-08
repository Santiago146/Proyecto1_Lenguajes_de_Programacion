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
    int puerto;
};

// Mapa para almacenar los usuarios conectados
std::unordered_map<std::string, Usuario> usuarios_conectados; //guarda un username, su IP y su ID de socket

// Función para enviar un mensaje a todos los clientes
void broadcastMensaje(const std::string& mensaje, int remitente_fd) {
    for (const auto& [username, usuario] : usuarios_conectados) {
        if (usuario.socket_fd != remitente_fd) { // No enviarse el mensaje a sí mismo
            send(usuario.socket_fd, mensaje.c_str(), mensaje.length(), 0);
        }
    }
}

void handleClient(int client_socket, string username, bool valido) {
    char buffer[1024];
    // Notificar a los demás usuarios
    string connect_msg = username + " se ha conectado.\n";
    broadcastMensaje(connect_msg, client_socket);

    while (true) {
        memset(buffer, 0, sizeof(buffer)); // limpia el buffer
        int bytes_received = recv(client_socket, buffer, sizeof(buffer), 0); // recibe datos, y guarda los datos en el buffer y la longitud del mensaje en bytes_received

        if (bytes_received <= 0) {
            if (valido)
                cout << username << " se ha desconectado.\n";
            break;
        }
    }
    close(client_socket);

    string leave_msg = string(username) + " se ha desconectado.\n";
    broadcastMensaje(leave_msg, -1);

    usuarios_conectados.erase(username);
}

int main() {
    port = extractPortConfig("config.txt", port);

    int server_fd, client_socket; // guarda el resultado de la conexion, si se hizo con exito o no
    struct sockaddr_in address, client_addr; // Direccion IP del servidor, y del cliente
    socklen_t addrlen = sizeof(address), client_addrlen = sizeof(client_addr);
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

    // Enlazar el socket al puerto
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) { // bind() asigna el socket a una dirección IP y un puerto específicos.
        perror("Error al enlazar el socket");
        exit(EXIT_FAILURE);
    }
    cout << "Socket enlazado correctamente" << endl;

    // Escuchar conexiones entrantes
    if (listen(server_fd, 10) < 0) { // TEST pre fork(), esto permite que el servidor escuche hasta 10 conexiones pendientes.
        perror("Error al escuchar");
        exit(EXIT_FAILURE);
    }
    cout << "Servidor escuchando en el puerto " << port << "..." << endl;



    // Aceptar conexiones entrante
    while (true) {
        // Aceptar nueva conexión
        // accept() espera una conexión de un cliente y cuando un cliente se conecta, devuelve un nuevo resultado de conexion de socket "client_socket"
        client_socket = accept(server_fd, (struct sockaddr*)&client_addr, &client_addrlen);
        if (client_socket < 0) {
            std::cerr << "Error al aceptar conexión\n";
            continue;
        }

        cout << "Nueva conexión aceptada\n";

        // Registro/Sesion del usuario
        string username;
        bool valido = false;
        while (true) {
            memset(buffer, 0, sizeof(buffer));
            int bytes_received = recv(client_socket, buffer, sizeof(buffer), 0);
            if (bytes_received <= 0) {
                close(client_socket);
                break;
            }
            username = string(buffer);
            // Validar que el nombre de usuario no este en uso
            if (usuarios_conectados.find(username) == usuarios_conectados.end()) {
                string user_ip = inet_ntoa(client_addr.sin_addr);
                int user_port = ntohs(client_addr.sin_port);
                usuarios_conectados[username] = {user_ip, client_socket, user_port};

                cout << "Usuario " << username << " registrado con IP: " << user_ip << ", Puerto: " << user_port << ", SocketID: " << client_socket << endl;

                string welcome = "Bienvenido, " + username + "!\n";
                send(client_socket, welcome.c_str(), welcome.length(), 0);
                valido = true;
                break;
            } else {
                string msg = "Nombre de usuario " + username + " en uso. Intente con otro nombre.\n";
                send(client_socket, msg.c_str(), msg.length(), 0);
            }
        } 
        if (!valido)
            cout << "Cliente: " << inet_ntoa(client_addr.sin_addr) << ":" << ntohs(client_addr.sin_port) << "," << client_socket << " se ha desconectado." << endl;

        // Crear un proceso hijo con fork()
        pid_t pid = fork(); // pid: process id
        if (pid == 0) { // Proceso hijo
            close(server_fd); // Cerrar el socket del servidor en el hijo para evitar conflictos
            handleClient(client_socket, username, valido); // Manejar la comunicación con el cliente
            exit(0); // Salir del proceso hijo
        } else if (pid > 0) { // Proceso padre
            close(client_socket); // El padre cierra el socket del cliente
        } else {
            std::cerr << "Error al crear el proceso hijo\n";
        }
    }

    // Cerrar conexion del server
    close(server_fd);
    printf("\n");
    return 0;
}
