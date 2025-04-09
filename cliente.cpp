#include "configReader.h"
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <iostream>
#include <signal.h>
#include <sys/wait.h>

using namespace std;

// Colores para la consola
#define COLOR_RESET   "\033[0m"
#define COLOR_VERDE   "\033[32m"
#define COLOR_AZUL    "\033[34m"
#define COLOR_ROJO    "\033[31m"

int server_port = 8080; // Puerto por defecto
string server_ip = "127.0.0.1"; // IP por defecto

int socket_fd = -1; // Socket global para cerrar en el manejador de señales

// Manejador de señales para limpieza de recursos
void manejadorSignal(int signal_num) {
    if (socket_fd != -1) {
        close(socket_fd);
    }
    exit(signal_num);
}

// Función para el proceso de recepción de mensajes
void recibirMensajes(int socket_cliente) {
    char buffer[1024];
    
    while(true) {
        memset(buffer, 0, sizeof(buffer));
        int bytes_recibidos = recv(socket_cliente, buffer, sizeof(buffer), 0);
        
        if (bytes_recibidos <= 0) {
            cout << COLOR_ROJO << "Conexión con el servidor perdida." << COLOR_RESET << endl;
            kill(getppid(), SIGTERM); // Notificar al proceso padre
            break;
        }
        
        // Reemplazar esta línea:
        // cout << COLOR_AZUL << "Recibido: " << buffer << COLOR_RESET << endl;
        
        // Con este código más detallado:
        if (strncmp(buffer, "DE:", 3) == 0) {
            // Es un mensaje de otro usuario, formatear adecuadamente
            cout << "\n" << COLOR_AZUL << "[ MENSAJE RECIBIDO ] " << buffer << COLOR_RESET << endl;
        } else if (strncmp(buffer, "USUARIOS CONECTADOS:", 20) == 0) {
            // Es la lista de usuarios
            cout << "\n" << COLOR_AZUL << buffer << COLOR_RESET << endl;
        } else {
            // Otros mensajes del servidor
            cout << "\n" << COLOR_AZUL << "[ SERVIDOR ] " << buffer << COLOR_RESET << endl;
        }

        // Recordar al usuario que está en un menú
        cout << COLOR_VERDE << "\n===== MENÚ PRINCIPAL =====" << COLOR_RESET << endl;
        cout << "1. Ver lista de usuarios conectados" << endl;
        cout << "2. Enviar mensaje" << endl;
        cout << "3. Salir" << endl;
        cout << "Seleccione una opción: ";
    }
    
    exit(0);
}

// Función para el proceso de envío de mensajes
// Reemplazar la función enviarMensajes actual con esta implementación

// Función para el proceso de envío de mensajes
void enviarMensajes(int socket_cliente) {
    string input;
    int opcion;
    bool ejecutando = true;
    
    while(ejecutando) {
        // Mostrar menú principal
        cout << "\n" << COLOR_VERDE << "===== MENÚ PRINCIPAL =====" << COLOR_RESET << endl;
        cout << "1. Ver lista de usuarios conectados" << endl;
        cout << "2. Enviar mensaje" << endl;
        cout << "3. Salir" << endl;
        cout << "Seleccione una opción: ";
        
        // Leer opción
        getline(cin, input);
        
        try {
            opcion = stoi(input);
        }
        catch (const std::exception& e) {
            cout << COLOR_ROJO << "Por favor, ingrese un número válido." << COLOR_RESET << endl;
            continue;
        }
        
        // Procesar opción seleccionada
        switch(opcion) {
            case 1: // Ver lista de usuarios
                {
                    // Solicitar lista de usuarios al servidor
                    if (send(socket_cliente, "LISTAR_USUARIOS", 14, 0) < 0) {
                        cout << COLOR_ROJO << "Error al solicitar lista de usuarios" << COLOR_RESET << endl;
                        ejecutando = false;
                        break;
                    }
                    cout << "Solicitando lista de usuarios..." << endl;
                    sleep(1); // Dar tiempo para recibir respuesta
                }
                break;
                
            case 2: // Enviar mensaje
                {
                    string destinatario, mensaje, mensaje_completo;
                    
                    // Solicitar destinatario
                    cout << "\nIngrese el nombre del destinatario: ";
                    getline(cin, destinatario);
                    
                    if (destinatario.empty()) {
                        cout << COLOR_ROJO << "El nombre del destinatario no puede estar vacío." << COLOR_RESET << endl;
                        break;
                    }
                    
                    // Solicitar mensaje
                    cout << "Ingrese su mensaje: ";
                    getline(cin, mensaje);
                    
                    if (mensaje.empty()) {
                        cout << COLOR_ROJO << "El mensaje no puede estar vacío." << COLOR_RESET << endl;
                        break;
                    }
                    
                    // Construir mensaje completo en formato DESTINATARIO:mensaje
                    mensaje_completo = destinatario + ":" + mensaje;
                    
                    // Enviar mensaje al servidor
                    if (send(socket_cliente, mensaje_completo.c_str(), mensaje_completo.length(), 0) > 0) {
                        cout << COLOR_ROJO << "Error al enviar mensaje" << COLOR_RESET << endl;
                        ejecutando = false;
                        break;
                    }
                    
                    // Mostrar confirmación
                    cout << COLOR_VERDE << "Mensaje enviado a " << destinatario << COLOR_RESET << endl;
                }
                break;
                
            case 3: // Salir
                cout << "Cerrando aplicación..." << endl;
                ejecutando = false;
                break;
                
            default:
                cout << COLOR_ROJO << "Opción no válida. Por favor, intente de nuevo." << COLOR_RESET << endl;
                break;
        }
    }
    
    kill(0, SIGTERM); // Terminar todos los procesos del grupo
    exit(0);
}

int main() {
    // Configurar manejador de señales
    signal(SIGINT, manejadorSignal);
    signal(SIGTERM, manejadorSignal);
    
    // Leer configuración del cliente
    extractClientConfig("config.txt", server_ip, server_port);
    
    // Crear socket
    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (socket_fd == -1) {
        cerr << "Error al crear el socket" << endl;
        return 1;
    }
    
    // Configurar dirección del servidor
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(server_port);
    
    if (inet_pton(AF_INET, server_ip.c_str(), &server_addr.sin_addr) <= 0) {
        cerr << "Dirección IP inválida o no soportada" << endl;
        close(socket_fd);
        return 1;
    }
    
    // Conectar al servidor
    if (connect(socket_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        cerr << "Error de conexión al servidor" << endl;
        close(socket_fd);
        return 1;
    }
    
    // Modificar la sección después de la conexión exitosa en main()

    cout << COLOR_VERDE << "¡Conectado al servidor " << server_ip << ":" << server_port << "!" << COLOR_RESET << endl;
    cout << COLOR_VERDE << "===== BIENVENIDO AL SISTEMA DE MENSAJERÍA =====" << COLOR_RESET << endl;    

    // Proceso inicial de autenticación
    char buffer[1024];
    int bytes_recibidos = recv(socket_fd, buffer, sizeof(buffer), 0);
    
    if (bytes_recibidos <= 0) {
        cerr << "No se recibió respuesta del servidor" << endl;
        close(socket_fd);
        return 1;
    }
    
    buffer[bytes_recibidos] = '\0';
    cout << buffer; // Imprime: "Ingrese su nombre de usuario: "
    
    // Leer nombre de usuario
    string username;
    getline(cin, username);
    
    // Enviar nombre de usuario al servidor
    if (send(socket_fd, username.c_str(), username.length(), 0) < 0) {
        cerr << "Error al enviar nombre de usuario" << endl;
        close(socket_fd);
        return 1;
    }
    
    // Recibir respuesta del servidor
    bytes_recibidos = recv(socket_fd, buffer, sizeof(buffer), 0);
    if (bytes_recibidos <= 0) {
        cerr << "No se recibió respuesta del servidor" << endl;
        close(socket_fd);
        return 1;
    }
    
    buffer[bytes_recibidos] = '\0';
    cout << buffer << endl; // Mensaje de bienvenida
    
    // Bifurcar para manejar envío y recepción simultáneos
    pid_t pid = fork();
    
    if (pid < 0) {
        cerr << "Error al crear proceso hijo" << endl;
        close(socket_fd);
        return 1;
    } 
    else if (pid == 0) {
        // Proceso hijo: maneja la recepción de mensajes
        recibirMensajes(socket_fd);
    } 
    else {
        // Proceso padre: maneja el envío de mensajes
        enviarMensajes(socket_fd);
    }
    
    // No debería llegar aquí
    close(socket_fd);
    return 0;
}