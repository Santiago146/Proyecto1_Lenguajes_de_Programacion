# Sistema de Mensajería - Lenguajes de Programación

Tecnológico de Costa Rica

IC4700 - Lenguajes de Programación

Proyecto 1: Paradigma Imperativo

## Integrantes:

- Kevin Núñez Camacho 
- Yeri Porras Víquez 
- Roy Silva

II Semestre – 2025
 
## Tabla de contenido
- [Enlace al repositorio en GitHub](#enlace-al-repositorio-en-github)
- [Instalación del programa](#instalación-del-programa)
  - [Requisitos previos a la instalación](#requisitos-previos-a-la-instalación)
  - [Paso 1: Instalación del editor de código](#paso-1-instalación-del-editor-de-código)
  - [Paso 2: Actualizar repositorios del sistema](#paso-2-actualizar-repositorios-del-sistema)
  - [Paso 3: Instalar el compilador y herramientas de desarrollo](#paso-3-instalar-el-compilador-y-herramientas-de-desarrollo)
  - [Paso 4: Instalación de bibliotecas adicionales](#paso-4-instalación-de-bibliotecas-adicionales)
  - [Paso 5: Instalar GitHub Desktop](#paso-5-instalar-github-desktop)
- [Instalación de dependencias](#instalación-de-dependencias)
- [Compilación del programa](#compilación-del-programa)
  - [Paso 1: Clonar el repositorio de GitHub](#paso-1-clonar-el-repositorio-de-github)
  - [Paso 2: Compilar el proyecto](#paso-2-compilar-el-proyecto)
- [Manual de Usuario](#manual-de-usuario)
- [Arquitectura Lógica Utilizada](#arquitectura-lógica-utilizada)

## Enlace al repositorio en GitHub
El código fuente del proyecto está disponible en el siguiente repositorio: https://github.com/Santiago146/Proyecto1_Lenguajes_de_Programacion 

## Instalación del programa
### Requisitos previos a la instalación:
Este proyecto está hecho para poder correr en sistemas basados en Ubuntu/Debian, en específico Linux

### Paso 1: Instalación del editor de código
Debemos tener en claro que el programa se debe ejecutar en Linux, luego para poder ejecutarlo bien es recomendable instalar el editor de código con el cual se hicieron todas las pruebas, depuraciones y creación del programa, dicho editor de código es Visual Studio Code, el cuál se instala en la tienda del sistema operativo y se busca con el nombre Code.
 
### Paso 2: Actualizar repositorios del sistema
Debemos abrir la terminal de sistema y actualizar los repositorios del sistema con el siguiente comando:
```bash
sudo apt update
```

### Paso 3: Instalar el compilador y herramientas de desarrollo
Instalar el compilador g++ que nos ayuda a compilar los archivos .cpp en donde tenemos implementado el programa y las herramientas de desarrollo esenciales, con el siguiente comando:
```bash
sudo apt install build-essential
```

Este comando instala:
- El compilador g++
- El compilador gcc
- make y otras utilidades de desarrollo
- Bibliotecas básicas de desarrollo

Luego verificamos la versión de g++ para ver si se instaló correctamente, con el siguiente comando:
```bash
g++ --version
```

Si se instala de manera correcta debe de enviar un mensaje siguiendo la misma estructura que el siguiente ejemplo.
Debemos asegurarnos que la versión sea igual o superior a la 7.0 para garantizar el soporte completo de C++.

### Paso 4: Instalación de bibliotecas adicionales 
Ejecutar en la misma terminal los siguientes comandos uno por uno:
```bash
sudo apt install libstdc++6
sudo apt install gdb
```

### Paso 5: Instalar GitHub Desktop 
Para poder clonar el proyecto debemos instalar GitHub Desktop, para lo cual debemos ejecutar en la terminal los siguientes comandos:
```bash
wget -qO - https://mirror.mwt.me/shiftkey-desktop/gpgkey | gpg --dearmor | sudo tee /usr/share/keyrings/mwt-desktop.gpg > /dev/null
sudo sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/mwt-desktop.gpg] https://mirror.mwt.me/shiftkey-desktop/deb/ any main" > /etc/apt/sources.list.d/mwt-desktop.list'
sudo apt update && sudo apt install github-desktop
```

Y listo tenemos GitHub Desktop instalado en nuestro sistema Linux

## Instalación de dependencias
Nuestro programa necesita las siguientes bibliotecas estándar de Linux que generalmente ya vienen instaladas desde el momento de la configuración del sistema operativo, pero debemos asegurarnos que estén por lo tanto debemos ejecutar en la misma terminal los siguientes comandos:
```bash
sudo apt-get update
sudo apt-get install build-essential
```

## Compilación del programa
### Paso 1: Clonar el repositorio de GitHub 
Debemos ir al siguiente link:
https://github.com/Santiago146/Proyecto1_Lenguajes_de_Programacion 
y darle a la opción ver en GitHub Desktop, para luego de esto, clonar nuestro proyecto en algún lugar en específico y darle a la opción una vez ya clonado en la ruta seleccionada, presionar en la opción Open in Visual Studio Code

### Paso 2: Compilar el proyecto
Una vez se abra el proyecto en el editor de código, debemos abrir una terminal e irnos directos a la ruta en donde gurdamos el proyecto, para ello podemos ir al explorador de archivos, buscar la carpeta llamada "Proyecto1 Lenguajes de Programacion", y copiar la ruta que sale en la parte de arriba.

Una vez copiada esta ruta debemos irnos de nuevo a la terminal abierta previamente en Visual Studio y escribir cd y copiar la ruta que previamente encontramos.
Una vez nos encontramos en dicha ruta debemos compilar los archivos necesarios para que nuestro programa corra exitosamente, los cuales son, el servidor y el cliente, con los siguientes comandos:
```bash
g++ -o server server.cpp -pthread
g++ -o cliente cliente.cpp
```

Y ya podemos comenzar a ejecutar el programa, su ejecución se especifica más a fondo en el siguiente apartado

## Manual de Usuario
Para poder usar el programa debemos correr el servidor de primero y luego el cliente, en el cual se realizarán todas las acciones y funcionalidades del programa.
Una vez compilados ambos archivos, debemos abrir una nueva terminal en la ruta de la carpeta que encontramos en paso 2 de la compilación del programa y corremos el servidor con el siguiente comando:
```bash
./server
```

Y ejecutamos el cliente en el cual ya se realizarán las funcionalidades y acciones que el usuario desee, debemos abrir una nueva terminal en la ruta de la carpeta que encontramos en paso 2 de la compilación del programa y ejecutar el cliente con el comando:
```bash
./cliente
```

Una vez hechos estos pasos procedemos a la ejecución del programa.

## Arquitectura Lógica Utilizada
