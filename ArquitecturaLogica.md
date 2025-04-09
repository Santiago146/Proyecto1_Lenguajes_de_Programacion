# Arquitectura Lógica del Sistema de Mensajería

## Visión General

El sistema de mensajería implementa una arquitectura cliente-servidor con procesamiento concurrente, utilizando el paradigma imperativo en C++. Está diseñado para permitir la comunicación en tiempo real entre múltiples usuarios a través de una red.

![Diagrama de Arquitectura](https://i.imgur.com/XhQgZP3.png)

## Componentes Principales

La aplicación se divide en dos componentes principales:

1. **Servidor**: Maneja múltiples conexiones de clientes concurrentemente y coordina la comunicación entre ellos.
2. **Cliente**: Proporciona la interfaz de usuario para conectarse al servidor y comunicarse con otros usuarios.

## Estructura de Archivos

El sistema está compuesto por los siguientes archivos:

1. `server.cpp` - Implementación del servidor
2. `cliente.cpp` - Implementación del cliente
3. `configReader.h` - Biblioteca para leer configuración
4. `config.txt` - Archivo de configuración
5. `ManualUsuario.md` - Guía de usuario
6. `README.md` - Documentación general

## Explicación Detallada por Componente

### 1. Servidor (`server.cpp`)

El servidor implementa un modelo de comunicación multi-proceso con memoria compartida para coordinar los datos entre los procesos hijos que atienden a cada cliente.

#### Características Principales:

- **Manejo Concurrente de Conexiones**: Utiliza la llamada al sistema `fork()` para crear un proceso hijo por cada cliente conectado.
- **Memoria Compartida**: Implementa un mecanismo de memoria compartida entre procesos usando `mmap()` y sincronización con mutexes de POSIX.
- **Comunicación Entre Procesos (IPC)**: Utiliza sockets internos para permitir la comunicación entre usuarios atendidos por diferentes procesos.
- **Estructura de Usuarios**: Almacena información de cada usuario conectado incluyendo ID único, IP, socket, puerto y proceso que lo maneja.

#### Flujo de Ejecución:

1. El servidor inicia y carga la configuración del archivo `config.txt`
2. Inicializa la memoria compartida y estructuras de datos
3. Crea un socket y lo enlaza al puerto configurado
4. Entra en un bucle infinito donde:
   - Acepta nuevas conexiones de clientes
   - Crea un proceso hijo para manejar cada nueva conexión
   - El proceso hijo ejecuta la función `manejarCliente()`
5. Dentro de `manejarCliente()`:
   - Verifica si el mensaje es interno (IPC)
   - Registra al usuario en la memoria compartida
   - Entra en un bucle para procesar los mensajes del cliente
   - Maneja diferentes tipos de mensajes (listado de usuarios, mensajes a otros usuarios)

#### Comunicación Entre Procesos:

Un aspecto clave del diseño es cómo se comunican mensajes entre usuarios manejados por diferentes procesos:

1. Cuando un usuario envía un mensaje a otro que es manejado por otro proceso:
   - Se crea un socket temporal conectado al servidor
   - Se envía un mensaje con formato especial `INTERNAL_MSG:destinatario:remitente:contenido`
   - El servidor recibe este mensaje y lo redirige al proceso que maneja al destinatario
   - Se confirma la entrega del mensaje

### 2. Cliente (`cliente.cpp`)

El cliente implementa una interfaz de usuario basada en consola con capacidad de procesamiento simultáneo de envío y recepción de mensajes.

#### Características Principales:

- **Procesamiento Concurrente**: Utiliza `fork()` para crear dos procesos: uno para enviar mensajes y otro para recibirlos.
- **Interfaz de Usuario**: Implementa un menú interactivo para las funciones principales.
- **Comunicación con el Servidor**: Mantiene una conexión TCP con el servidor para enviar/recibir mensajes.
- **Formato de Colores**: Utiliza códigos ANSI para mejorar la visualización de mensajes en la consola.

#### Flujo de Ejecución:

1. El cliente inicia y carga la configuración
2. Crea un socket y se conecta al servidor
3. Recibe la lista inicial de usuarios conectados
4. Solicita y envía el nombre de usuario
5. Se bifurca en dos procesos:
   - Proceso hijo: ejecuta `recibirMensajes()` para mostrar mensajes entrantes
   - Proceso padre: ejecuta `enviarMensajes()` para manejar la interfaz de usuario
6. El proceso de envío muestra el menú principal con opciones para:
   - Ver la lista de usuarios conectados
   - Enviar un mensaje
   - Salir del sistema

### 3. Biblioteca de Configuración (`configReader.h`)

Esta biblioteca proporciona funciones para leer la configuración del sistema desde un archivo de texto.

#### Funciones:

- `extractPortConfig()`: Lee el puerto para el servidor
- `extractClientConfig()`: Lee la IP y puerto para el cliente

#### Propósito:

Permite configurar el sistema sin necesidad de recompilar el código, facilitando el despliegue en diferentes entornos.

### 4. Archivo de Configuración (`config.txt`)

Archivo de texto plano que contiene la configuración básica del sistema:

```
PORT=8080
IP=127.0.0.1
```

## Flujo de Datos

### Registro de Usuario:

1. Cliente se conecta al servidor
2. Servidor crea un proceso hijo para manejar al cliente
3. Cliente envía nombre de usuario
4. Servidor almacena información del usuario en memoria compartida
5. Servidor envía lista actualizada de usuarios a todos los clientes

### Envío de Mensajes:

1. Cliente A selecciona enviar mensaje a Usuario B
2. Cliente A envía mensaje al servidor en formato `DESTINATARIO:mensaje`
3. Servidor verifica si el destinatario existe
4. Si el destinatario es manejado por el mismo proceso:
   - Envía el mensaje directamente al socket del destinatario
5. Si el destinatario es manejado por otro proceso:
   - Crea un socket temporal
   - Envía mensaje interno al servidor con formato `INTERNAL_MSG:destinatario:remitente:contenido`
   - El servidor redirige el mensaje al proceso que maneja al Usuario B
   - El mensaje se entrega al Usuario B
6. Cliente B recibe y muestra el mensaje

## Mecanismos de Sincronización

El sistema utiliza varios mecanismos para garantizar la correcta sincronización y comunicación:

1. **Mutex POSIX**: Para sincronizar el acceso a la memoria compartida entre procesos
2. **Serialización/Deserialización**: Para mantener la consistencia de datos entre procesos
3. **Sockets Temporales**: Para la comunicación entre procesos de servidor
4. **Identificadores Únicos**: Para distinguir unívocamente a cada usuario

## Manejo de Excepciones y Errores

El sistema implementa varios mecanismos para manejar errores:

1. **Validación de Entrada**: Verifica que los nombres de usuario no estén vacíos
2. **Detección de Desconexiones**: Detecta cuando un cliente se desconecta y libera recursos
3. **Manejadores de Señales**: Captura señales del sistema para limpieza de recursos
4. **Procesos Zombies**: Implementa un manejador de SIGCHLD para evitar procesos zombies

## Seguridad

El sistema implementa medidas básicas de seguridad:

1. **Validación de Usuarios**: Verifica que un nombre de usuario no puede ser usado por diferentes IPs
2. **Sanitización de Entrada**: Elimina caracteres no deseados de los mensajes
3. **Mensajes de Confirmación**: Confirma la entrega de mensajes

## Escalabilidad

La arquitectura del sistema permite una escalabilidad moderada:

1. El uso de procesos separados permite manejar múltiples clientes simultáneamente
2. La memoria compartida proporciona un mecanismo eficiente para compartir datos entre procesos
3. El sistema de comunicación interna permite la comunicación entre cualquier par de usuarios

## Limitaciones

El diseño actual tiene algunas limitaciones:

1. El servidor maneja todo en memoria, sin persistencia de datos
2. No hay encriptación en la comunicación
3. La comunicación está limitada a una red local o a un servidor con IP conocida
4. No hay autenticación robusta de usuarios

## Conclusión

El sistema implementa una arquitectura cliente-servidor eficiente utilizando mecanismos avanzados del sistema operativo Linux para permitir la comunicación en tiempo real entre usuarios. El uso de procesos separados para cada cliente proporciona aislamiento y robustez, mientras que la memoria compartida y los mecanismos de IPC permiten la coordinación necesaria para un sistema de mensajería funcional.
