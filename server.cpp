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
#include <vector> // Para usar std::vector
#include <sys/mman.h> // Para memoria compartida
<<<<<<< Updated upstream
<<<<<<< Updated upstream
#include <mutex> // Para mutex y lock_guard
#include <semaphore.h> // Para semáforos
=======
#include <pthread.h> // Para mutex entre procesos
#include <fcntl.h> // Para O_* constantes
#include <semaphore.h> // Para semáforos entre procesos
>>>>>>> Stashed changes
=======
#include <pthread.h> // Para mutex entre procesos
#include <fcntl.h> // Para O_* constantes
#include <semaphore.h> // Para semáforos entre procesos
>>>>>>> Stashed changes

using namespace std;

int port = 8080; // puerto por defecto en caso de error con el archivo config.txt
int next_user_id = 1; // Contador global para asignar IDs únicos a los usuarios

// Estructura para almacenar información del usuario
struct Usuario {
    int user_id;      // ID único para cada usuario
    string ip;
    int socket_fd;
    int puerto;
    pid_t proceso_id; // Agregar el ID del proceso que maneja este usuario
};

<<<<<<< Updated upstream
<<<<<<< Updated upstream
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
=======
// Estructura para compartir datos entre procesos
struct DatosCompartidos {
    pthread_mutex_t mutex;
    int num_usuarios;
    int next_id;      // ID único compartido
    char usuarios_data[1024 * 50]; // Buffer grande para almacenar datos serializados
};

// Puntero a la estructura compartida
DatosCompartidos* datos_compartidos;

// Mapa local para cada proceso
unordered_map<string, Usuario> usuarios_locales;

// Función para inicializar memoria compartida
void inicializarMemoriaCompartida() {
=======
// Estructura para compartir datos entre procesos
struct DatosCompartidos {
    pthread_mutex_t mutex;
    int num_usuarios;
    int next_id;      // ID único compartido
    char usuarios_data[1024 * 50]; // Buffer grande para almacenar datos serializados
};

// Puntero a la estructura compartida
DatosCompartidos* datos_compartidos;

// Mapa local para cada proceso
unordered_map<string, Usuario> usuarios_locales;

// Función para inicializar memoria compartida
void inicializarMemoriaCompartida() {
>>>>>>> Stashed changes
    // Reservar memoria compartida para la estructura
    void* memoria = mmap(NULL, sizeof(DatosCompartidos),
                         PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
>>>>>>> Stashed changes
    if (memoria == MAP_FAILED) {
        perror("Error al asignar memoria compartida");
        exit(EXIT_FAILURE);
    }
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    shared_data = new (memoria) SharedData();
    sem_init(&shared_data->usuarios_sem, 1, 1);
=======
=======
>>>>>>> Stashed changes
    
    // Inicializar la estructura en la memoria compartida
    datos_compartidos = new (memoria) DatosCompartidos();
    
    // Inicializar el mutex con atributos para compartir entre procesos
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    pthread_mutex_init(&datos_compartidos->mutex, &attr);
    pthread_mutexattr_destroy(&attr);
    
    // Inicializar el contador de usuarios y el contador de IDs
    datos_compartidos->num_usuarios = 0;
    datos_compartidos->next_id = 1;
    
    // Inicializar el buffer de datos
    memset(datos_compartidos->usuarios_data, 0, sizeof(datos_compartidos->usuarios_data));
    
    cout << "Memoria compartida inicializada correctamente" << endl;
}

// Función para obtener un nuevo ID único
int obtenerNuevoId() {
    int id;
    pthread_mutex_lock(&datos_compartidos->mutex);
    id = datos_compartidos->next_id++;
    pthread_mutex_unlock(&datos_compartidos->mutex);
    return id;
}

// Función para serializar el mapa de usuarios a la memoria compartida
void serializarUsuarios() {
    string datos_serializados;
    
    for (const auto& par : usuarios_locales) {
        datos_serializados += par.first + "," + 
                              to_string(par.second.user_id) + "," +
                              par.second.ip + "," + 
                              to_string(par.second.socket_fd) + "," + 
                              to_string(par.second.puerto) + "," +
                              to_string(par.second.proceso_id) + ";";
    }
    
    pthread_mutex_lock(&datos_compartidos->mutex);
    memset(datos_compartidos->usuarios_data, 0, sizeof(datos_compartidos->usuarios_data));
    strcpy(datos_compartidos->usuarios_data, datos_serializados.c_str());
    datos_compartidos->num_usuarios = usuarios_locales.size();
    pthread_mutex_unlock(&datos_compartidos->mutex);
}

// Función para deserializar los datos de la memoria compartida al mapa local
void deserializarUsuarios() {
    usuarios_locales.clear();
    
    pthread_mutex_lock(&datos_compartidos->mutex);
    string datos_serializados(datos_compartidos->usuarios_data);
    pthread_mutex_unlock(&datos_compartidos->mutex);
    
    if (datos_serializados.empty()) {
        return;
    }
    
    size_t pos = 0;
    size_t delim;
    
    while ((delim = datos_serializados.find(';', pos)) != string::npos) {
        string entrada = datos_serializados.substr(pos, delim - pos);
        pos = delim + 1;
        
        // Formato: username,user_id,ip,socket_fd,puerto,proceso_id;
        size_t comma1 = entrada.find(',');
        size_t comma2 = entrada.find(',', comma1 + 1);
        size_t comma3 = entrada.find(',', comma2 + 1);
        size_t comma4 = entrada.find(',', comma3 + 1);
        size_t comma5 = entrada.find(',', comma4 + 1);
        
        if (comma1 != string::npos && comma2 != string::npos && 
            comma3 != string::npos && comma4 != string::npos && 
            comma5 != string::npos) {
            
            string username = entrada.substr(0, comma1);
            int user_id = stoi(entrada.substr(comma1 + 1, comma2 - comma1 - 1));
            string ip = entrada.substr(comma2 + 1, comma3 - comma2 - 1);
            int socket_fd = stoi(entrada.substr(comma3 + 1, comma4 - comma3 - 1));
            int puerto = stoi(entrada.substr(comma4 + 1, comma5 - comma4 - 1));
            pid_t proceso_id = stoi(entrada.substr(comma5 + 1));
            
            usuarios_locales[username] = {user_id, ip, socket_fd, puerto, proceso_id};
        }
    }
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
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
    
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    if (shared_data->usuarios_conectados.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
=======
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : usuarios_locales) {
>>>>>>> Stashed changes
=======
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : usuarios_locales) {
>>>>>>> Stashed changes
            lista += "- " + par.first + "\n";
        }
    }
    
    // Agregar mensaje de depuración
    cout << "Enviando lista de usuarios: " << lista << endl;
    
    // Asegurarse de enviar toda la cadena
    send(client_socket, lista.c_str(), lista.length(), 0);
}

// Función para enviar la lista de usuarios conectados a todos los clientes
void pasarListaUsuarios() {
    string lista = "USUARIOS CONECTADOS:\n";
    
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    if (shared_data->usuarios_conectados.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
=======
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : usuarios_locales) {
>>>>>>> Stashed changes
=======
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        lista += "- No hay usuarios conectados actualmente.\n";
    } else {
        for (const auto& par : usuarios_locales) {
>>>>>>> Stashed changes
            lista += "- " + par.first + "\n";
        }
    }

<<<<<<< Updated upstream
<<<<<<< Updated upstream
    for (const auto& par : shared_data->usuarios_conectados) {
        send(par.second.socket_fd, lista.c_str(), lista.length(), 0);
=======
=======
>>>>>>> Stashed changes
    // Enviar la lista a todos los usuarios
    for (const auto& par : usuarios_locales) {
        if (par.second.socket_fd > 0) {
            // Enviar mensaje solo si el proceso actual es el que maneja este usuario
            if (par.second.proceso_id == getpid()) {
                send(par.second.socket_fd, lista.c_str(), lista.length(), 0);
            }
        }
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
    }
}

// Función para mostrar los usuarios conectados en el servidor
void mostrarUsuariosConectados() {
    cout << "Usuarios conectados actualmente:\n";
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    if (shared_data->usuarios_conectados.empty()) {
        cout << "- No hay usuarios conectados.\n";
    } else {
        for (const auto& par : shared_data->usuarios_conectados) {
            cout << "- " << par.first << " (IP: " << par.second.ip << ", Socket: " << par.second.socket_fd << ")\n";
=======
    
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        cout << "- No hay usuarios conectados.\n";
    } else {
=======
    
    // Actualizar datos locales desde la memoria compartida
    deserializarUsuarios();
    
    if (usuarios_locales.empty()) {
        cout << "- No hay usuarios conectados.\n";
    } else {
>>>>>>> Stashed changes
        for (const auto& par : usuarios_locales) {
            cout << "- " << par.first << " (IP: " << par.second.ip 
                 << ", Socket: " << par.second.socket_fd 
                 << ", ID: " << par.second.user_id
                 << ", Proceso: " << par.second.proceso_id << ")\n";
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
        }
    }
}

// Función para manejar la terminación de procesos hijos y evitar zombies
void manejadorSIGCHLD(int s) {
    // Esperar por todos los procesos hijo que hayan terminado
    while(waitpid(-1, NULL, WNOHANG) > 0);
}

// Función para enviar mensaje a un usuario específico
bool enviarMensajeAUsuario(const string& destinatario, const string& remitente, const string& contenido) {
    // Actualizar desde memoria compartida
    deserializarUsuarios();
    
    // Verificar si el destinatario existe
    if (usuarios_locales.find(destinatario) == usuarios_locales.end()) {
        return false;
    }
    
    // Obtener información del destinatario
    Usuario dest_info = usuarios_locales[destinatario];
    
    // Si estamos en el proceso que maneja al destinatario, enviamos directamente
    if (dest_info.proceso_id == getpid()) {
        string mensaje_formateado = "DE:" + remitente + " MENSAJE:" + contenido + "\n";
        send(dest_info.socket_fd, mensaje_formateado.c_str(), mensaje_formateado.length(), 0);
        return true;
    } 
    // Si el destinatario está manejado por otro proceso, usamos memoria compartida
    // Este código requeriría un sistema de IPC más elaborado que el actual
    // Lo dejamos para futuras mejoras
    
    return false;
}

// Función para gestionar mensajes internos entre procesos
bool procesarMensajeInterno(const string& mensaje, int socket_fd) {
    // Formato esperado: "INTERNAL_MSG:destinatario:remitente:contenido"
    if (mensaje.substr(0, 13) != "INTERNAL_MSG:") {
        return false;
    }
    
    size_t pos1 = mensaje.find(':', 13);
    if (pos1 == string::npos) return false;
    
    size_t pos2 = mensaje.find(':', pos1 + 1);
    if (pos2 == string::npos) return false;
    
    string destinatario = mensaje.substr(13, pos1 - 13);
    string remitente = mensaje.substr(pos1 + 1, pos2 - pos1 - 1);
    string contenido = mensaje.substr(pos2 + 1);
    
    // Eliminar saltos de línea al final
    if (!contenido.empty() && contenido.back() == '\n') {
        contenido.pop_back();
    }
    
    cout << "Mensaje interno recibido: de " << remitente << " para " << destinatario << endl;
    
    // Verificar si este proceso maneja al destinatario
    deserializarUsuarios();
    if (usuarios_locales.find(destinatario) == usuarios_locales.end()) {
        // Enviar error como respuesta
        send(socket_fd, "ERROR", 5, 0);
        return false; // No encontramos al destinatario en este proceso
    }
    
    Usuario& dest_info = usuarios_locales[destinatario];
    if (dest_info.proceso_id != getpid()) {
        // Enviar error como respuesta
        send(socket_fd, "ERROR", 5, 0);
        return false; // Este proceso no maneja al destinatario
    }
    
    // Enviar el mensaje al destinatario
    string mensaje_formateado = "DE:" + remitente + " MENSAJE:" + contenido + "\n";
    send(dest_info.socket_fd, mensaje_formateado.c_str(), mensaje_formateado.length(), 0);
    
    cout << "Mensaje interno reenviado al destinatario " << destinatario << endl;
    
    // Enviar confirmación al socket temporal
    send(socket_fd, "OK", 2, 0);
    return true;
}

void manejarCliente(int client_socket) {
    char buffer[1024]; // buffer para los mensajes
    
    // Verificar si es un mensaje interno (comunicación entre procesos)
    memset(buffer, 0, sizeof(buffer));
    int bytes_received = recv(client_socket, buffer, sizeof(buffer), MSG_PEEK | MSG_DONTWAIT);
    
    if (bytes_received > 0) {
        string posible_interno(buffer);
        if (posible_interno.substr(0, 13) == "INTERNAL_MSG:") {
            // Recibir el mensaje completo (no solo peek)
            memset(buffer, 0, sizeof(buffer));
            recv(client_socket, buffer, sizeof(buffer), 0);
            
            if (procesarMensajeInterno(buffer, client_socket)) {
                // No necesitamos hacer nada más, el mensaje ya fue procesado y
                // se envió una confirmación. Cerramos la conexión y volvemos.
                close(client_socket);
                return;
            } else {
                // Si falló el procesamiento, enviamos error y cerramos
                send(client_socket, "ERROR", 5, 0);
                close(client_socket);
                return;
            }
        }
    }
    
    // A partir de aquí sabemos que es un cliente normal, no un mensaje interno
    char username[50];
    string username_str;
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
    username_str = string(username);

    // Eliminar espacios en blanco y saltos de línea al inicio y al final
    username_str.erase(0, username_str.find_first_not_of(" \n\r\t"));
    username_str.erase(username_str.find_last_not_of(" \n\r\t") + 1);

    // Validar que el nombre de usuario no esté vacío
    if (username_str.empty()) {
        send(client_socket, "El nombre de usuario no puede estar vacío.\n", 43, 0);
        close(client_socket);
        return;
    }

<<<<<<< Updated upstream
<<<<<<< Updated upstream
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
=======
    // Actualizar desde la memoria compartida
    deserializarUsuarios();
    
    // Verificar si el usuario ya está registrado
    if (usuarios_locales.find(username_str) != usuarios_locales.end()) {
        if (usuarios_locales[username_str].ip != user_ip) {
            // Si las IPs no coinciden, denegamos la conexión
            send(client_socket, "Nombre de usuario ya en uso desde otra IP. Intente otro.\n", 56, 0);
            close(client_socket);
            return;
        }
>>>>>>> Stashed changes
=======
    // Actualizar desde la memoria compartida
    deserializarUsuarios();
    
    // Verificar si el usuario ya está registrado
    if (usuarios_locales.find(username_str) != usuarios_locales.end()) {
        if (usuarios_locales[username_str].ip != user_ip) {
            // Si las IPs no coinciden, denegamos la conexión
            send(client_socket, "Nombre de usuario ya en uso desde otra IP. Intente otro.\n", 56, 0);
            close(client_socket);
            return;
        }
>>>>>>> Stashed changes
    }
    sem_post(&shared_data->usuarios_sem);

    // Registrar al usuario, IP y socket ID
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len);
    int puerto_cliente = ntohs(addr.sin_port);

    // Obtener un ID único para este usuario
    int user_id = obtenerNuevoId();
    
    Usuario nuevo_usuario = {user_id, user_ip, client_socket, puerto_cliente, getpid()};
    usuarios_locales[username_str] = nuevo_usuario;
    
    // Guardar en la memoria compartida
    serializarUsuarios();

    cout << "Usuario " << username_str << " registrado con IP: " << user_ip 
         << ", puerto: " << puerto_cliente << ", ID: " << user_id << endl;

    // Registrar al usuario, IP y socket ID
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    getpeername(client_socket, (struct sockaddr*)&addr, &addr_len);
    int puerto_cliente = ntohs(addr.sin_port);

    // Obtener un ID único para este usuario
    int user_id = obtenerNuevoId();
    
    Usuario nuevo_usuario = {user_id, user_ip, client_socket, puerto_cliente, getpid()};
    usuarios_locales[username_str] = nuevo_usuario;
    
    // Guardar en la memoria compartida
    serializarUsuarios();

    cout << "Usuario " << username_str << " registrado con IP: " << user_ip 
         << ", puerto: " << puerto_cliente << ", ID: " << user_id << endl;

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
        // Eliminar espacios en blanco y saltos de línea al final
        mensaje.erase(mensaje.find_last_not_of(" \n\r\t") + 1);

        // Verificar si es un mensaje interno - no debería ocurrir aquí, pero por seguridad
        if (mensaje.substr(0, 13) == "INTERNAL_MSG:") {
            if (procesarMensajeInterno(mensaje, client_socket)) {
                continue;
            }
        }

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
            
            // Eliminar espacios en blanco al inicio y al final del destinatario
            destinatario.erase(0, destinatario.find_first_not_of(" \n\r\t"));
            destinatario.erase(destinatario.find_last_not_of(" \n\r\t") + 1);
            
            // Mensaje formateado para el destinatario
            string mensaje_formateado = "DE:" + username_str + " MENSAJE:" + contenido + "\n";
            
            // Actualizar desde memoria compartida
            deserializarUsuarios();
            
            // Verificar si el destinatario existe
<<<<<<< Updated upstream
<<<<<<< Updated upstream
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
=======
=======
>>>>>>> Stashed changes
            bool destinatario_existe = usuarios_locales.find(destinatario) != usuarios_locales.end();
            
            if (destinatario_existe) {
                Usuario dest_info = usuarios_locales[destinatario];
                
                // Depuración para el servidor
                cout << "Enviando mensaje: " << mensaje_formateado << " a " << destinatario << endl;
                
                bool mensaje_enviado = false;
                
                // Verificar si es mensaje para sí mismo
                if (destinatario == username_str) {
                    // Si el mensaje es para sí mismo, enviarlo directamente
                    send(client_socket, mensaje_formateado.c_str(), mensaje_formateado.length(), 0);
                    mensaje_enviado = true;
                } 
                // Si el destinatario es manejado por este mismo proceso
                else if (dest_info.proceso_id == getpid()) {
                    send(dest_info.socket_fd, mensaje_formateado.c_str(), mensaje_formateado.length(), 0);
                    mensaje_enviado = true;
                } 
                // Si el destinatario es manejado por otro proceso
                else {
                    // Crear un socket temporal para comunicarnos con el proceso del destinatario
                    int temp_socket = socket(AF_INET, SOCK_STREAM, 0);
                    if (temp_socket != -1) {
                        struct sockaddr_in dest_addr;
                        dest_addr.sin_family = AF_INET;
                        dest_addr.sin_port = htons(port); // Puerto del servidor
                        inet_pton(AF_INET, "127.0.0.1", &dest_addr.sin_addr); // Localhost
                        
                        if (connect(temp_socket, (struct sockaddr*)&dest_addr, sizeof(dest_addr)) != -1) {
                            // Formato especial para indicar que es un mensaje interno
                            string internal_msg = "INTERNAL_MSG:" + destinatario + ":" + username_str + ":" + contenido;
                            send(temp_socket, internal_msg.c_str(), internal_msg.length(), 0);
                            
                            char confirm[128] = {0};
                            if (recv(temp_socket, confirm, sizeof(confirm), 0) > 0) {
                                string respuesta(confirm);
                                if (respuesta == "OK") {
                                    mensaje_enviado = true;
                                    cout << "Confirmación recibida del proceso destino: mensaje entregado" << endl;
                                } else {
                                    cout << "Error en la confirmación del proceso destino" << endl;
                                }
                            }
                        }
                        // Cerrar el socket temporal siempre
                        close(temp_socket);
                    }
                }
                
                // Confirmar al remitente
                if (mensaje_enviado) {
                    string confirmacion = "Mensaje enviado a " + destinatario + "\n";
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
                    send(client_socket, confirmacion.c_str(), confirmacion.length(), 0);
                    cout << "Mensaje de " << username_str << " enviado a " << destinatario << endl;
                } else {
                    string error = "Error al enviar mensaje a " + destinatario;
                    send(client_socket, error.c_str(), error.length(), 0);
                }
<<<<<<< Updated upstream
<<<<<<< Updated upstream
                sem_post(&shared_data->usuarios_sem);
=======
=======
>>>>>>> Stashed changes
            } else {
                // Usuario no encontrado
                string error = "Usuario " + destinatario + " no encontrado o no está conectado";
                send(client_socket, error.c_str(), error.length(), 0);
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
            }
        } else {
            // Formato incorrecto
            string error = "Formato incorrecto. Use: DESTINATARIO:mensaje";
            send(client_socket, error.c_str(), error.length(), 0);
        }
    }
    
    // Eliminar al usuario de la lista de conectados
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    sem_wait(&shared_data->usuarios_sem);
    {
        auto it = shared_data->usuarios_conectados.find(username_str);
        if (it != shared_data->usuarios_conectados.end() && it->second.socket_fd == client_socket) {
            shared_data->usuarios_conectados.erase(it);
        }
    }
    sem_post(&shared_data->usuarios_sem);
=======
    deserializarUsuarios();
    usuarios_locales.erase(username_str);
    serializarUsuarios();
>>>>>>> Stashed changes
=======
    deserializarUsuarios();
    usuarios_locales.erase(username_str);
    serializarUsuarios();
>>>>>>> Stashed changes

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
    if (listen(server_fd, 10) < 0) { // Incrementado a 10 conexiones pendientes
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
    
    // Liberar recursos de memoria compartida (esto nunca se ejecutará en este ejemplo)
    pthread_mutex_destroy(&datos_compartidos->mutex);
    munmap(datos_compartidos, sizeof(DatosCompartidos));
    
    return 0;
}
