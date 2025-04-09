# Manual de Usuario - Sistema de Mensajería

Este manual detalla cómo utilizar el sistema de mensajería en tiempo real desarrollado como parte del proyecto de Lenguajes de Programación.

## Índice
1. [Iniciar el Sistema](#iniciar-el-sistema)
2. [Conectarse como Usuario](#conectarse-como-usuario)
3. [Funcionalidades Principales](#funcionalidades-principales)
   - [Listar Usuarios Conectados](#listar-usuarios-conectados)
   - [Enviar Mensajes](#enviar-mensajes)
   - [Enviar Mensajes a Sí Mismo](#enviar-mensajes-a-sí-mismo)
4. [Conectar Múltiples Usuarios](#conectar-múltiples-usuarios)
5. [Cerrar el Sistema](#cerrar-el-sistema)

## Iniciar el Sistema

Antes de poder utilizar la aplicación, es necesario iniciar el servidor.

1. Abra una terminal en el directorio del proyecto

2. Inicie el servidor ejecutando:
   ```bash
   ./server
   ```

   Debería ver una salida similar a:
   ```
   Memoria compartida inicializada correctamente
   Puerto configurado: 8080
   Configuración cargada: IP=0.0.0.0 (automático), Puerto=8080
   Socket creado exitosamente
   Socket enlazado correctamente
   Servidor escuchando en el puerto 8080...
   ```

3. Mantenga esta terminal abierta mientras usa el sistema

## Conectarse como Usuario

Una vez que el servidor esté en funcionamiento, puede conectarse como usuario:

1. Abra una nueva terminal en el directorio del proyecto

2. Inicie el cliente ejecutando:
   ```bash
   ./cliente
   ```

3. Verá un mensaje de bienvenida:
   ```
   ¡Conectado al servidor 127.0.0.1:8080!
   ===== BIENVENIDO AL SISTEMA DE MENSAJERÍA =====
   USUARIOS CONECTADOS:
   - No hay usuarios conectados actualmente.
   Ingrese su nombre de usuario:
   ```

4. Ingrese un nombre de usuario (por ejemplo, "Usuario1") y presione Enter

5. Si el registro es exitoso, verá un mensaje de bienvenida y el menú principal:
   ```
   Bienvenido, Usuario1!

   ===== MENÚ PRINCIPAL =====
   1. Ver lista de usuarios conectados
   2. Enviar mensaje
   3. Salir
   Seleccione una opción:
   ```

## Funcionalidades Principales

### Listar Usuarios Conectados

Para ver la lista de usuarios actualmente conectados al sistema:

1. En el menú principal, seleccione la opción 1:
   ```
   Seleccione una opción: 1
   ```

2. Verá un mensaje indicando que se está solicitando la lista y luego aparecerá:
   ```
   Solicitando lista de usuarios...

   USUARIOS CONECTADOS:
   - Usuario1
   ```

3. Esta lista se actualizará automáticamente cada vez que un usuario se conecte o desconecte

### Enviar Mensajes

Para enviar un mensaje a otro usuario:

1. En el menú principal, seleccione la opción 2:
   ```
   Seleccione una opción: 2
   ```

2. Ingrese el nombre del destinatario:
   ```
   Ingrese el nombre del destinatario: Usuario2
   ```

3. Escriba su mensaje:
   ```
   Ingrese su mensaje: Hola, ¿cómo estás?
   ```

4. Si el usuario existe y está conectado, recibirá una confirmación:
   ```
   Mensaje enviado a Usuario2
   ```

5. El destinatario verá en su pantalla:
   ```
   [ MENSAJE RECIBIDO ] DE:Usuario1 MENSAJE:Hola, ¿cómo estás?
   ```

### Enviar Mensajes a Sí Mismo

También puede enviarse mensajes a sí mismo:

1. Seleccione la opción 2 en el menú principal
   ```
   Seleccione una opción: 2
   ```

2. Ingrese su propio nombre como destinatario:
   ```
   Ingrese el nombre del destinatario: Usuario1
   ```

3. Escriba su mensaje:
   ```
   Ingrese su mensaje: Esto es una prueba
   ```

4. Recibirá la confirmación y verá el mensaje en su pantalla:
   ```
   Mensaje enviado a Usuario1

   [ MENSAJE RECIBIDO ] DE:Usuario1 MENSAJE:Esto es una prueba
   ```

## Conectar Múltiples Usuarios

Para simular una conversación entre varios usuarios:

1. Con el servidor en ejecución, abra una nueva terminal en el directorio del proyecto

2. Inicie otra instancia del cliente:
   ```bash
   ./cliente
   ```

3. Ingrese un nombre diferente (por ejemplo, "Usuario2") 

4. Ahora puede enviar mensajes entre "Usuario1" y "Usuario2"

5. Cada vez que se conecta un nuevo usuario, todos los usuarios conectados recibirán una actualización de la lista de usuarios

6. Para verificar esto, puede seleccionar la opción 1 en el menú principal de cualquier cliente:
   ```
   USUARIOS CONECTADOS:
   - Usuario1
   - Usuario2
   ```

7. Puede repetir este proceso para conectar tantos usuarios como desee

## Cerrar el Sistema

1. Para cerrar un cliente, seleccione la opción 3 en el menú principal:
   ```
   Seleccione una opción: 3
   Cerrando aplicación...
   ```

2. Para detener el servidor, presione Ctrl+C en la terminal donde se ejecuta el servidor

3. Todos los usuarios conectados serán desconectados automáticamente cuando el servidor se cierre

---

**Notas adicionales:**
- Las conexiones se realizan a la dirección IP 127.0.0.1 (localhost) y al puerto 8080 por defecto
- La configuración del sistema puede modificarse editando el archivo `config.txt`
- Los mensajes aparecen en color para facilitar la lectura y diferenciar los tipos de notificaciones
