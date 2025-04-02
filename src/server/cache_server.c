
#include "cache_server_utils.h"

#include "../cache/cache.h"
#include "../helpers/quit.h"

#define GREEN   "\x1b[32m"
#define RED     "\x1b[31m"
#define ORANGE  "\x1b[38;5;214m"
#define BLUE    "\x1b[94m"

#define RESET   "\x1b[0m"

Cache global_cache;

static void print_error_msg(int thread_number) {
  printf(RED "[Thread %d] " RESET, thread_number);
  printf("Error en socket\n");
}

static void print_waiting_msg(int thread_number) {
  printf(GREEN "[Thread %d] " RESET, thread_number); 
  printf("Waiting for events\n");
}

static void print_accepted_msg(int thread_number) {
  printf(ORANGE "[Thread %d] " RESET, thread_number); 
  printf("Accepting client\n");
}

static void print_parsing_msg(int thread_number) {
  printf(BLUE "[Thread %d] " RESET, thread_number); 
  printf("Parsing request\n");
}

static void print_handling_msg(int thread_number) {
  printf(BLUE "[Thread %d] " RESET, thread_number); 
  printf("Handling request\n");
}



/**
 *  @brief Es la funcion con la que se lanza a cada uno de los threads que responden pedidos por el servidor. 
 * 
 *  Cada thread entra en un ciclo infinito en el que
 * 
 *    - Espera a ser levantado por la instancia de epoll asociada al servidor.
 *    - Al levantarse, recupera la informacion del cliente y evento que lo provoco. Aqui se obtiene `event_data`, la estructura que contiene el socket asociado al cliente que intenta realizar una operacion de IO y su buffer mas la informacion de parseo.
 * 
 *    - Si el socket del `event_data` es el socket del servidor, entonces se trata de un intento de conexion. El thread acepta la conexion, crea la estructura de datos para el nuevo cliente, reconstruye la instancia de epoll y vuelve a iniciar el ciclo.
 * 
 *    - Si el socket del `event_data` es distinto al del servidor, se trata de un cliente ya conectado intentando escribir. Se parsea el stream recibido y actualiza la informacion del cliente. Si el parseo llego a su estado final, se ejecuta el pedido. En todos los casos, se reconstruye la estancia de epoll y se vuelve a iniciar el ciclo.
 * 
 */
void* working_thread(void* thread_args) {


  ThreadArgs* thread_args_casted = (ThreadArgs*) thread_args;

  int server_epoll = thread_args_casted->server_epoll;
  int server_socket = thread_args_casted->server_socket;
  int thread_number = thread_args_casted->thread_number;
  // Hash hash = thread_args_casted->hash;

  struct epoll_event event;

  while(1) { 

    print_waiting_msg(thread_number);
    if (epoll_wait(server_epoll, &event, 1, -1) < 0)
      quit("[Error] EPOLL_WAIT");

    ClientData* event_data = (ClientData*) event.data.ptr;
    
    if (event.events & (EPOLLERR | EPOLLHUP | EPOLLRDHUP)) {
      print_error_msg(thread_number);
      drop_client(server_epoll, event_data);
    }

    else if (event_data->socket == server_socket) { // Alguien se quiere conectar
      
      int new_client_socket = accept(server_socket, NULL, NULL);
      print_accepted_msg(thread_number);

      // Creamos el epoll_event del cliente y lo cargamos al epoll
      ClientData* new_client_data = create_new_client_data(new_client_socket);
      construct_new_client_epoll(server_epoll, new_client_data);

      // Reconstruimos el evento de epoll del servidor
      event.events = EPOLLIN | EPOLLONESHOT;
      epoll_ctl(server_epoll, EPOLL_CTL_MOD, server_socket, &event);

    }
    
    else { // Es un cliente escribiendo

      // Parseamos su request
      print_parsing_msg(thread_number);

      // Si fracaso el parseo, droppeamos al cliente 
      if (parse_request(event_data) < 0) {
        print_error_msg(thread_number);
        drop_client(server_epoll, event_data);
      }

      // Si el parseo termino, ejecutamos su pedido
      else if (event_data->parsing_stage == PARSING_FINISHED) {
        print_handling_msg(thread_number);

        // Si fracaso el envio de la respuesta al cliente, lo droppeamos
        if (handle_request(event_data) < 0) {
          print_error_msg(thread_number);
          drop_client(server_epoll, event_data);
        }

        // Si no, reiniciamos sus buffers y reconstruimos su evento en el epoll.
        reset_client_data(event_data);
      
        reconstruct_client_epoll(server_epoll, &event, event_data);

      }
      // Caso contrario, continua la proxima iteracion.
      
    }
  }
}

 /**
  *   @brief Recibe la informacion del servidor (file descriptor de la instancia epoll, file descriptor del socket del servidor, y cantidad de threads) y lanza los threads trabajadores correspondientes, comenzando asi el funcionamiento del servidor.
  * 
  *   @param[in] server_args Puntero a la estructura que contiene la informacion de esta instancia del servidor.
  */
void start_server(ServerArgs* server_args) {

  ThreadArgs thread_args;
  thread_args.server_epoll = epoll_create1(0);
  thread_args.server_socket = server_args->server_socket;
  // thread_args.hash = server_args->hash;

  // El primer evento que controlamos es el de EPOLLIN al socket del servidor, que representa un intento de conexion. Solo cargamos el campo de socket, pues la informacion de parseo no aplica a este evento.
  ClientData server_data;
  server_data.socket = server_args->server_socket; 

  struct epoll_event event;
  event.events = EPOLLIN | EPOLLONESHOT; 

  // Al levantarse un thread, podra distinguir si se trata de un intento de conexion al chequear el socket que cargamos en server_data.
  event.data.ptr = &server_data;
  epoll_ctl(thread_args.server_epoll, EPOLL_CTL_ADD,
            server_data.socket, &event);

  // Leemos la cantidad de threads a lanzar y los creamos
  int num_threads = server_args->num_threads;

  // Creamos un ThreadArgs por cada thread, donde cada uno es una copia del thread_args que creamos en un principio.
  // ? Es necesario que cada uno tenga su copia? No podemos pasarle el mismo puntero a todos? Si al fin y al cabo son solo los fd del epoll y el socket.
  pthread_t threads[num_threads];
  ThreadArgs threads_args[num_threads];

  for (int i = 0 ; i < num_threads ; i++) {
    threads_args[i] = thread_args;
    threads_args[i].thread_number = i;
    
    pthread_create(&threads[i], NULL, working_thread, &threads_args[i]);
  }

  for (int i = 0 ; i < num_threads ; i++)
    pthread_join(threads[i], NULL);  
}


/**
 *  @brief Pone en funcionamiento al servidor memcached con el socket, numero de threads, e instancia de la cache pasados por argumento. No se chequea la correctitud de los argumentos pues esta funcion solo sera invocada por server_starter que no permite errores.
 */
int main(int argc, char** argv) { // Sabemos que los argumentos son correctos.

  setbuf(stdout, NULL); // Opcional

  ServerArgs server_args;
  server_args.server_socket = atoi(argv[1]);
  server_args.num_threads = atoi(argv[2]);
  global_cache = cache_create((HashFunction) kr_hash);
  server_args.cache = global_cache;

  start_server(&server_args);

  return 0;
}




