# Memcached
A basic memcached implementation using C and Erlang.

## Compilación del programa
Todos los comandos que daremos a continuación deben ejecutarse en la raíz del proyecto para que funcionen de manera correcta.

Para compilar todo el proyecto, simplemente tenemos que hacer:

    make

  Ésto hará que se compile tanto el ```servidor``` memcached como el ```cliente```. En el caso de que únicamente queramos compilar el ```servidor``` tenemos que utilizar el siguiente comando:

    make server

Análogamente, si queremos compilar solamente el ```cliente```, podemos ejecutar el siguiente comando:

    make client

Por defecto, las compilaciones de C se hacen sin la bandera de depuración. Si queremos incluírla, tenemos que indicarlo con la macro correspondiente:

    make server debug=yes

Por último, si queremos limpiar los archivos objeto, los binarios, y los archivos de compilación de erlang podemos correr el siguiente comando:

    make clean

## Ejecución del server 
Luego de haber compilado el ```servidor``` de C, podemos ejecutarlo de manera simple con algunos valores default:

    ./bin/server

Si queremos ver las opciones que tenemos para correrlo podemos usar:

    ./bin/server --help

que nos imprimirá por pantalla las distintas banderas que podemos utilizar para correrlo. Las opciones completas son las siguientes:

    ./bin/server  --port <port_number> --memory <memory_limit> --threads <num_threads>

Si no se especifica algunas de las opciones, el servidor utilizará un valor por defecto para dicha opción.

En el caso de que queramos que nuestro servidor utilice un puerto privilegiado, debemos correr el siguiente comando previo a ejecutarlo:

    make set-bind-privilege

Al ser un comando que necesita permisos de administrador, nos pedirá nuestra contraseña. Ésto hace que el programa tenga privilegios a bindearse a puertos de bajo nivel. Cuando se logra bindear, pierde esos privilegios. Por otro lado, si queremos que nuestro programa ya no tenga estos privilegios iniciales, simplemente podemos correr:

    make remove-bind-privilege

## Ejecución del cliente
Luego de haber compilado el ```cliente``` de erlang, podemos ejecutar el siguiente comando en el directorio raiz del proyecto para utilizarlo:

    make run-client

Ésto nos abrirá una terminal de erlang en la que podemos comenzar a utilizar la interfaz del cliente. En la terminal tenemos disponibles todos los siguientes comandos:

    client:start/1         % Inicia un cliente con una lista de servidores.
    client:startDefault/0  % Inicia un cliente con servidores por defecto.
    client:quit/1          % Finaliza un cliente.
    client:put/3           % Pone un par clave-valor en un cliente.
    client:get/2           % Obtiene el valor de una cierta clave de un cliente.
    client:del/2           % Elimina el valor asociada a una cierta clave de un cliente.
    client:stats/1         % Imprime las estadisticas de los servidores conectados al cliente.
    client:status/1        % Imprime las estadisticas locales del cliente elegido.

Es importante aclarar que tenemos la posibilidad de crear múltiples clientes en la misma terminal de Erlang. Para aprovechar esta funcionalidad, las funciones que arrancan clientes nos devuelven un identificador que nos servirá para comunicarnos con cada uno de los clientes que tengamos corriendo.

## Ejemplo de uso básico

Para modelar un caso de uso, podemos modelar un cliente que se conecta a dos servidores en la misma computadora. Para lanzar los servidores tendremos que correr:

    ./bin/server --port 8000 --threads 4

el cual nos inicia un servidor bindeado al puerto 8000, con 4 threads y una cantidad de memoria predeterminada. En otra terminal, corremos:

    ./bin/server --port 9000 --memory 100000000

el cual nos inicia un servidor bindeado al puerto 9000, con un número predeterminado de threads y un total de 100000000 bytes (alrededor de 100 Mb). Ahora podemos hacer:

    make run-client

y en la terminal que se nos abre, correr:

    IdClient = client:startDefault().

que inicia un cliente asociado a dos servidores en la misma máquina, bindeados a los puertos 8000 y 9000 respectivamente. Luego de realizar ésto, podemos efectuar todas las opearciones que queramos sobre el cliente que acabamos de crear utilizando el ```IdClient```. Por ejemplo:

    client:put(IdClient, 3, "hola").

    client:stats(IdClient).
    
    client:quit(IdClient).