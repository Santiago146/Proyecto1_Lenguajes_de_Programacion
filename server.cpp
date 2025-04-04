#include "configReader.h"
#include <sys/socket.h> // Para la creación y uso de sockets
#include <netinet/in.h> // Para definir direcciones de red y estructura sockaddr_in
#include <unistd.h> // Para funciones de cierre de sockets y procesos
#include <unordered_map>
#include <arpa/inet.h> // para manejar ips
#include <signal.h> // Para manejar señales
#include <sys/wait.h> // Para esperar procesos hijo
#include <iostream>
#include <cstring>

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

// Función para obtener la IP del cliente
std::string obtenerIP(int client_socket) {
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len); // funcion para obtener la IP del usuario
    return inet_ntoa(addr.sin_addr);
}

// Función para generar y enviar la lista de usuarios conectados
void enviarListaUsuarios(int client_socket) {
    std::string lista = "USUARIOS CONECTADOS:\n";
    
    if (usuarios_conectados.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : usuarios_conectados) {
            lista += "- " + par.first + "\n";
        }
    }
    
    send(client_socket, lista.c_str(), lista.length(), 0);
}

// Función para manejar la terminación de procesos hijos y evitar zombies
void manejadorSIGCHLD(int s) {
    // Esperar por todos los procesos hijo que hayan terminado
    while(waitpid(-1, NULL, WNOHANG) > 0);
}

void manejarCliente(int client_socket) {
    char buffer[1024]; // buffer para los mensajes
    char username[50];
    
    std::string user_ip = obtenerIP(client_socket);
    
    // Enviar lista de usuarios antes de solicitar el nombre
    enviarListaUsuarios(client_socket);

    // Solicitar y registrar el nombre de usuario
    send(client_socket, "Ingrese su nombre de usuario: ", 29, 0);
    int bytes_recibidos = recv(client_socket, username, sizeof(username), 0);
    
    if (bytes_recibidos <= 0) {
        close(client_socket);
        return;
    }
    username[strcspn(username, "\n")] = 0; // Eliminar salto de línea

    // Verificar si el usuario ya está registrado
    if (usuarios_conectados.find(username) != usuarios_conectados.end()) {
        if (usuarios_conectados[username].ip != user_ip) {
            // Si las IPs no coinciden, denegamos la conexión
            send(client_socket, "Nombre de usuario ya en uso desde otra IP. Intente otro.\n", 56, 0);
            close(client_socket);
            return;
        }
    }
    
    // Registrar al usuario, IP y socket ID, auqnue ya este en uso, el socket ID es diferente y necesario para actualizar
    // username: reconocer al usuario
    // IP: reconocer direccion en la que se conecta el usuario, y si lo hace desde varios dispositivos a la vez
    // reconocer el canal de comunicacion de cada dispositivo
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len);
    int puerto_cliente = ntohs(addr.sin_port);

    Usuario nuevo_usuario = {user_ip, client_socket};
    usuarios_conectados[username] = nuevo_usuario;
    
    string welcome_message = "Bienvenido, " + std::string(username) + "!\n";
    send(client_socket, welcome_message.c_str(), welcome_message.length(), 0);

    cout << "Usuario " << username << " registrado con IP: " << user_ip << endl; // depsues de pruebas cambiar por exitosamente

    // Bucle principal de comunicación con el cliente
    while (true) {
        memset(buffer, 0, sizeof(buffer)); // limpia el buffer
        int bytes_received = recv(client_socket, buffer, sizeof(buffer), 0);

        if (bytes_received <= 0) { // Si el cliente se desconecta o hay un error
            cout << "Usuario " << username << " desconectado.\n";
            break;
        }

        string mensaje(buffer);

        // Verificar si es una solicitud para listar usuarios
        if (mensaje == "LISTAR_USUARIOS") {
            cout << "Usuario " << username << " solicitó la lista de usuarios" << endl;
            enviarListaUsuarios(client_socket);
            continue;
        }

        cout << "Mensaje recibido de " << username << ": " << mensaje << "\n";
        
        // Verificar si el mensaje es para un usuario específico (formato: "DESTINATARIO:mensaje")
        size_t pos_separador = mensaje.find(":");
        if (pos_separador != string::npos) {
            string destinatario = mensaje.substr(0, pos_separador);
            string contenido = mensaje.substr(pos_separador + 1);
            
            // Verificar si el destinatario existe
            if (usuarios_conectados.find(destinatario) != usuarios_conectados.end()) {
                // Construir mensaje con formato "DE:username MENSAJE:contenido"
                string mensaje_formateado = "DE:" + string(username) + " MENSAJE:" + contenido;
                
                // Enviar mensaje al destinatario
                send(usuarios_conectados[destinatario].socket_fd, 
                     mensaje_formateado.c_str(), 
                     mensaje_formateado.length(), 0);
                
                // Confirmar al remitente
                string confirmacion = "Mensaje enviado a " + destinatario;
                send(client_socket, confirmacion.c_str(), confirmacion.length(), 0);
            } else {
                // Usuario no encontrado
                string error = "Usuario " + destinatario + " no encontrado o no está conectado";
                send(client_socket, error.c_str(), error.length(), 0);
            }
        } else {
            // Formato incorrecto
            string error = "Formato incorrecto. Use: DESTINATARIO:mensaje";
            send(client_socket, error.c_str(), error.length(), 0);
        }
    }
    
    // Eliminar al usuario de la lista de conectados
    usuarios_conectados.erase(username);
    close(client_socket); // cierra el proceso del cliente
}

int main() {
    port = extractPortConfig("config.txt", port);

    int server_fd, client_socket; // guarda el resultado de la conexion, si se hizo con exito o no
    struct sockaddr_in address; // Direccion IP del servidor
    socklen_t addrlen = sizeof(address);
    char buffer[1024] = {0}; // espacio donde se almacenara el mensaje recibido

    // Configurar manejador de señal para procesos hijo
    struct sigaction sa;
    sa.sa_handler = manejadorSIGCHLD;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("Error en sigaction");
        exit(EXIT_FAILURE);
    }

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

    // Bucle principal del servidor
    while(true) {
        // Aceptar una conexión entrante
        if ((client_socket = accept(server_fd, (struct sockaddr*)&address, &addrlen)) < 0) {
            perror("Error al aceptar la conexión");
            continue; // Continuamos en lugar de salir para mantener el servidor funcionando
        }

        cout << "Nuevo cliente conectado desde " << inet_ntoa(address.sin_addr) 
            << ":" << ntohs(address.sin_port) << endl;

        // Crear un proceso hijo para manejar este cliente
        pid_t pid = fork();
        
        if (pid < 0) {
            perror("Error en fork");
            close(client_socket);
        } 
        else if (pid == 0) {
            // Proceso hijo: maneja la comunicación con el cliente
            close(server_fd); // El hijo no necesita el socket del servidor
            manejarCliente(client_socket);
            exit(0); // Terminar el proceso hijo cuando termina de manejar al cliente
        } 
        else {
            // Proceso padre: cierra el socket del cliente y continúa aceptando conexiones
            close(client_socket);
        }
    }

    // Este código nunca se ejecutará a menos que salgas del bucle while
    close(server_fd);
    printf("\n");
    return 0;
}
