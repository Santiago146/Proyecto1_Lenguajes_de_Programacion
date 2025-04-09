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
#include <sys/mman.h> // Para memoria compartida
#include <mutex> // Para mutex y lock_guard
#include <semaphore.h> // Para semáforos

using namespace std;

int port = 8080; // puerto por defecto en caso de error con el archivo config.txt

// Estructura para almacenar información del usuario
struct Usuario {
    string ip;
    int socket_fd;
    int puerto;
};

struct SharedData {
    unordered_map<string, Usuario> usuarios_conectados;
    sem_t usuarios_sem;
};

SharedData* shared_data;

// Función para inicializar memoria compartida
void inicializarMemoriaCompartida() {
    size_t tamano_memoria = sizeof(SharedData);
    void* memoria = mmap(NULL, tamano_memoria,
                         PROT_READ | PROT_WRITE,
                         MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (memoria == MAP_FAILED) {
        perror("Error al asignar memoria compartida");
        exit(EXIT_FAILURE);
    }
    shared_data = new (memoria) SharedData();
    sem_init(&shared_data->usuarios_sem, 1, 1);
}

// Función para obtener la IP del cliente
string obtenerIP(int client_socket) {
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len); // funcion para obtener la IP del usuario
    return inet_ntoa(addr.sin_addr);
}

// Función para generar y enviar la lista de usuarios conectados
void enviarListaUsuarios(int client_socket) {
    string lista = "USUARIOS CONECTADOS:\n";
    
    if (shared_data->usuarios_conectados.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
            lista += "- " + par.first + "\n";
        }
    }
    
    send(client_socket, lista.c_str(), lista.length(), 0);
}

// Función para enviar la lista de usuarios conectados a todos los clientes
void pasarListaUsuarios() {
    string lista = "USUARIOS CONECTADOS:\n";
    
    if (shared_data->usuarios_conectados.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
            lista += "- " + par.first + "\n";
        }
    }

    for (const auto& par : shared_data->usuarios_conectados) {
        send(par.second.socket_fd, lista.c_str(), lista.length(), 0);
    }
}

// Función para mostrar los usuarios conectados en el servidor
void mostrarUsuariosConectados() {
    cout << "Usuarios conectados actualmente:\n";
    if (shared_data->usuarios_conectados.empty()) {
        cout << "- No hay usuarios conectados.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
            cout << "- " << par.first << " (IP: " << par.second.ip << ", Socket: " << par.second.socket_fd << ")\n";
        }
    }
}

// Función para manejar la terminación de procesos hijos y evitar zombies
void manejadorSIGCHLD(int s) {
    // Esperar por todos los procesos hijo que hayan terminado
    while(waitpid(-1, NULL, WNOHANG) > 0);
}

void manejarCliente(int client_socket) {
    char buffer[1024]; // buffer para los mensajes
    char username[50];
    
    string user_ip = obtenerIP(client_socket);
    
    // Enviar lista de usuarios antes de solicitar el nombre
    enviarListaUsuarios(client_socket);

    // Solicitar y registrar el nombre de usuario
    send(client_socket, "Ingrese su nombre de usuario: ", 29, 0);
    int bytes_recibidos = recv(client_socket, username, sizeof(username), 0);
    
    if (bytes_recibidos <= 0) {
        cerr << "Error al recibir el nombre de usuario o conexión cerrada por el cliente." << endl;
        close(client_socket);
        return;
    }

    username[bytes_recibidos] = '\0'; // Asegurar que el buffer termine en null
    string username_str(username);

    // Eliminar espacios en blanco y saltos de línea al inicio y al final
    username_str.erase(0, username_str.find_first_not_of(" \n\r\t"));
    username_str.erase(username_str.find_last_not_of(" \n\r\t") + 1);

    // Validar que el nombre de usuario no esté vacío
    if (username_str.empty()) {
        send(client_socket, "El nombre de usuario no puede estar vacío.\n", 43, 0);
        close(client_socket);
        return;
    }

    // Usar un semáforo para evitar condiciones de carrera
    sem_wait(&shared_data->usuarios_sem);
    {
        // Verificar si el usuario ya está registrado
        if (shared_data->usuarios_conectados.find(username_str) != shared_data->usuarios_conectados.end()) {
            if (shared_data->usuarios_conectados[username_str].ip != user_ip) {
                sem_post(&shared_data->usuarios_sem);
                send(client_socket, "Nombre de usuario ya en uso desde otra IP. Intente otro.\n", 56, 0);
                close(client_socket);
                return;
            }
        }

        // Registrar al usuario, IP y socket ID
        struct sockaddr_in addr;
        socklen_t addr_len = sizeof(addr);
        getpeername(client_socket, (struct sockaddr*)&addr, &addr_len);
        int puerto_cliente = ntohs(addr.sin_port);

        Usuario nuevo_usuario = {user_ip, client_socket, puerto_cliente};
        shared_data->usuarios_conectados[username_str] = nuevo_usuario;

        cout << "Usuario " << username_str << " registrado con IP: " << user_ip << " y puerto: " << puerto_cliente << endl;
    }
    sem_post(&shared_data->usuarios_sem);

    // Enviar mensaje de bienvenida
    string welcome_message = "Bienvenido, " + username_str + "!\n";
    send(client_socket, welcome_message.c_str(), welcome_message.length(), 0);

    // Mostrar usuarios conectados después de registrar al nuevo usuario
    mostrarUsuariosConectados();

    // Enviar la lista actualizada de usuarios a todos los clientes
    pasarListaUsuarios();

    // Bucle principal de comunicación con el cliente
    while (true) {
        memset(buffer, 0, sizeof(buffer)); // limpia el buffer
        int bytes_received = recv(client_socket, buffer, sizeof(buffer), 0);

        if (bytes_received <= 0) { // Si el cliente se desconecta o hay un error
            cerr << "Usuario " << username_str << " desconectado o conexión perdida." << endl;
            break;
        }

        string mensaje(buffer);

        // Verificar si es una solicitud para listar usuarios
        if (mensaje == "LISTAR_USUARIOS") {
            cout << "Usuario " << username_str << " solicitó la lista de usuarios" << endl;
            enviarListaUsuarios(client_socket);
            continue;
        }

        // Verificar si el mensaje es para un usuario específico (formato: "DESTINATARIO:mensaje")
        size_t pos_separador = mensaje.find(":");
        if (pos_separador != string::npos) {
            string destinatario = mensaje.substr(0, pos_separador);
            string contenido = mensaje.substr(pos_separador + 1);
            
            // Verificar si el destinatario existe
            {
                sem_wait(&shared_data->usuarios_sem);
                if (shared_data->usuarios_conectados.find(destinatario) != shared_data->usuarios_conectados.end()) {
                    // Construir mensaje con formato "DE:username MENSAJE:contenido"
                    string mensaje_formateado = "DE:" + username_str + " MENSAJE:" + contenido;
                    
                    // Enviar mensaje al destinatario
                    send(shared_data->usuarios_conectados[destinatario].socket_fd, 
                        mensaje_formateado.c_str(), 
                        mensaje_formateado.length(), 0);
                    
                    // Confirmar al remitente
                    string confirmacion = "\nMensaje enviado a " + destinatario;
                    send(client_socket, confirmacion.c_str(), confirmacion.length(), 0);
                } else {
                    // Usuario no encontrado
                    string error = "Usuario " + destinatario + " no encontrado o no está conectado";
                    send(client_socket, error.c_str(), error.length(), 0);
                }
                sem_post(&shared_data->usuarios_sem);
            }
        } else {
            // Formato incorrecto
            string error = "Formato incorrecto. Use: DESTINATARIO:mensaje";
            send(client_socket, error.c_str(), error.length(), 0);
        }
    }
    
    // Eliminar al usuario de la lista de conectados
    sem_wait(&shared_data->usuarios_sem);
    {
        auto it = shared_data->usuarios_conectados.find(username_str);
        if (it != shared_data->usuarios_conectados.end() && it->second.socket_fd == client_socket) {
            shared_data->usuarios_conectados.erase(it);
        }
    }
    sem_post(&shared_data->usuarios_sem);

    // Mostrar usuarios conectados después de eliminar al usuario
    mostrarUsuariosConectados();

    // Enviar la lista actualizada de usuarios a todos los clientes
    pasarListaUsuarios();

    close(client_socket); // cierra el proceso del cliente
}

void limpiarRecursos() {
    // Limpiar recursos al terminar
    sem_destroy(&shared_data->usuarios_sem);
    munmap(shared_data, sizeof(SharedData));
}

int main() {
    inicializarMemoriaCompartida(); // Inicializar memoria compartida

    port = extractPortConfig("config.txt", port);

    cout << "Configuración cargada: IP=0.0.0.0 (automático), Puerto=" << port << endl;

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
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("Error al crear el socket");
        exit(EXIT_FAILURE);
    }
    cout << "Socket creado exitosamente" << endl;

    // Configurar opciones del socket para reutilizar la dirección y el puerto
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt)) < 0) {
        perror("Error al configurar opciones del socket");
        exit(EXIT_FAILURE);
    }

    // Configurar la dirección y el puerto
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY; // Asignar automáticamente la IP (todas las interfaces disponibles)
    address.sin_port = htons(port); // Pasa el puerto configurado como numero a formato de red

    // Enlazar el socket al puerto
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) { // bind() asigna el socket a una dirección IP y un puerto específicos.
        perror("Error al enlazar el socket");
        cout << "Verifique que el puerto " << port << " no esté en uso." << endl;
        exit(EXIT_FAILURE);
    }
    cout << "Socket enlazado correctamente" << endl;

    // Escuchar conexiones entrantes
    if (listen(server_fd, 3) < 0) { // TEST pre fork(), esto permite que el servidor escuche hasta 3 conexiones pendientes.
        perror("Error al escuchar");
        exit(EXIT_FAILURE);
    }
    cout << "Servidor escuchando en el puerto " << port << "..." << endl;

    signal(SIGINT, [](int){
        limpiarRecursos();
        exit(0);
    });

    // Bucle principal del servidor
    while (true) {
        // Aceptar una conexión entrante
        client_socket = accept(server_fd, (struct sockaddr*)&address, &addrlen);
        if (client_socket < 0) {
            perror("Error al aceptar la conexión");
            cout << "Error al aceptar conexión desde IP: " << inet_ntoa(address.sin_addr) 
                 << ", Puerto: " << ntohs(address.sin_port) << endl;
            continue; // Continuamos en lugar de salir para mantener el servidor funcionando
        }

        // Validar el descriptor del socket
        if (client_socket > FD_SETSIZE) {
            cerr << "Descriptor de socket inválido: " << client_socket << endl;
            close(client_socket);
            continue;
        }

        cout << "Nuevo cliente conectado desde " << inet_ntoa(address.sin_addr) 
             << ":" << ntohs(address.sin_port) << " con descriptor " << client_socket << endl;

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

        // Mostrar usuarios conectados en el servidor principal
        mostrarUsuariosConectados();
    }

    // Este código nunca se ejecutará a menos que salgas del bucle while
    close(server_fd);
    printf("\n");
    return 0;
}
