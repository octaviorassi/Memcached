#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>
#include <netinet/ip.h>

/*
 * Prender este flag para que el envío de datos sobre el socket sea
 * más al azar. Si tenemos que mandar 100 bytes, con este flag se van a
 * hacer cualquier cantidad de llamadas a write() con valores distintos,
 * en vez de escribir los 100 de una vez.
 *
 * Nunca está garantizado que si hacemos un write() de N bytes el
 * extremo va a ver N bytes, pero si la carga es baja o estamos dentro
 * de un mismo host (sin red), suele pasar. Con este flag podemos
 * testear una situación más real de una red cargada, y/o una red un
 * poco adversarial.
 */
#define RUIDO 0

#if RUIDO
static inline int min(int x, int y) { return x < y ? x : y; }
#endif

enum code {
	PUT = 11,
	DEL = 12,
	GET = 13,

	STATS = 21,

	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

static void die(char *s, ...)
{
	va_list v;

	va_start(v, s);
	vfprintf(stderr, s, v);
	fprintf(stderr, "\n");
	va_end(v);
	fprintf(stderr, " -- errno = %i (%m)\n", errno);

	fflush(stderr);
	abort();
}

int create_sock(char *host, char *port)
{
	int sock, ret;
	struct sockaddr_in addr;
	struct addrinfo *gai, hints;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
		die("socket");

	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;

	/*
	 * Consultamos la información sobre la dirección que nos
	 * dieron. Podemos pasar una IP, o un nombre que será
	 * consultado a /etc/hosts o al nameserver configurado
	 */
	ret = getaddrinfo(host, port, &hints, &gai);
	if (ret)
		die("getaddrinfo (%s)", gai_strerror(ret));

	/*
	 * getaddrinfo devuelve una lista enlazada con
	 * información, tomamos el primer nodo
	 */

	addr = *(struct sockaddr_in *)gai->ai_addr;

	freeaddrinfo(gai);

	/* Conectamos a esa dirección */
	ret = connect(sock, (struct sockaddr *)&addr, sizeof addr);
	if (ret < 0)
		die("connect");

	return sock;
}

int conn()
{
	return create_sock("localhost", "8889");
}

void writen(int fd, const void *buf, int len)
{
	int i = 0, rc;

	while (i < len) {
		int chunk = len - i;

		/*
		 * Si RUIDO, mandamos menos de lo que podemos. También
		 * se puede llevar al extremo de chunk=1.
		 */
		#if RUIDO
		 chunk = min(chunk, 1 + (rand() % 20));
		#endif

		rc = write(fd, buf + i, chunk);
		if (rc <= 0)
			die("writen");
		i += rc;
	}
}

void readn(int fd, void *buf, int len)
{
	int i = 0, rc;

	while (i < len) {
		rc = read(fd, buf + i, len - i);
		if (rc <= 0)
			die("readn");
		i += rc;
	}
}

void send_var(int fd, int len, const void *buf)
{
	int len_net = htonl(len);
	writen(fd, &len_net, 4);
	writen(fd, buf, len);
}

void recv_var(int fd, int *lenp, void **bufp)
{
	void *buf;
	int len;
	char lbuf[4];

	/* Recibir 4 bytes de la clave */
	readn(fd, lbuf, 4);

	/* Castear esos bytes a un int */
	int len_net = *(int*)lbuf;
	/* Pero eran big-endian, así que llevar al endianness de la máquina */
	len = ntohl(len_net);

	buf = malloc(len);
	if (!buf)
		die("recv_var oom");

	readn(fd, buf, len);

	*bufp = buf;
	*lenp = len;
}

void get(const char *k)
{
	int fd = conn();
	if (fd < 1)
		die("conn");

	/* Pedir */
	{ 
		int comm = GET;
		writen(fd, &comm, 1);
		send_var(fd, strlen(k), k);
	}

	/* Ver respuesta */
	{
		int cod = 0;
		readn(fd, &cod, 1);

		if (cod == ENOTFOUND) {
			fprintf(stderr, "ENOTFOUND\n");
			exit(1);
		} else if (cod != OK) {
			die("error en pedido, devolvió %i", cod);
		}

		int len;
		void *buf;
		recv_var(fd, &len, &buf);
		writen(1, buf, len);
		free(buf);
		fprintf(stderr, "\nOK\n");
	}
}

void del(const char *k)
{
	int fd = conn();
	if (fd < 1)
		die("conn");

	/* Pedir */
	{ 
		int comm = DEL;
		writen(fd, &comm, 1);
		send_var(fd, strlen(k), k);
	}

	/* Ver respuesta */
	{
		int cod = 0;
		read(fd, &cod, 1);

		if (cod == ENOTFOUND) {
			fprintf(stderr, "ENOTFOUND\n");
			exit(1);
		} else if (cod != OK)
			die("error en pedido, devolvió %i", cod);

		fprintf(stderr, "\nOK\n");
	}
}

/* Lee todo lo que haya en stdin */
char * input(int *lenp)
{
	char *ret = malloc(1024);
	int off = 0, sz = 1024;
	int rc;

	while ((rc = read(0, ret + off, sz - off)) > 0) {
		off += rc;
		if (off == sz) {
			sz *= 2;
			ret = realloc(ret, sz);
		}
	}

	if (rc < 0)
		die("input.read?");

	assert(rc == 0);
	/* OK, EOF */
	*lenp = off;
	return ret;
}

void put(const char *k)
{
	int fd = conn();
	if (fd < 1)
		die("conn");

	/* Pedir */
	{ 
		int comm = PUT;
		writen(fd, &comm, 1);
		send_var(fd, strlen(k), k);

		int len;
		char *buf = input(&len);
		send_var(fd, len, buf);
		free(buf);
	}

	/* Ver respuesta */
	{
		int cod = 0;
		readn(fd, &cod, 1);

		if (cod != OK)
			die("error en pedido, devolvió %i", cod);

		fprintf(stderr, "\nOK\n");
	}
}


void stats()
{
	int fd = conn();

	/* Pedir */
	{ 
		int comm = STATS;
		writen(fd, &comm, 1);
	}
	/* Ver respuesta */
	{
		int cod = 0;
		readn(fd, &cod, 1);

		if (cod != OK)
			die("error en pedido, devolvió %i", cod);

		int len;
		void *buf;
		recv_var(fd, &len, &buf);
		writen(1, buf, len);
		writen(1, "\n", 1);
		free(buf);

		fprintf(stderr, "\nOK\n");
	}
}

void usage(const char *progname)
{
	fprintf(stderr, "Uso: %s GET  <clave>\n", progname);
	fprintf(stderr, "   | %s DEL  <clave>\n", progname);
	fprintf(stderr, "   | %s PUT  <clave>\n", progname);
	fprintf(stderr, "   | %s STATS\n", progname);
	fprintf(stderr, "\n");
	fprintf(stderr, "Para PUT, el valor se lee desde la entrada estándar.\n"
			"Para GET el valor se escribe a la salida estándar.\n"
			"\n"
			"Por salida de error se imprime OK o ENOTFOUND\n"
			"\n"
			"Nota: las claves se toman como strings, este programa\n"
			"no permite toda la flexiblidad del modo binario.\n"
			"Las claves *no contienen* el '\\0' terminador del string.\n");
	exit(1);
}

int main(int argc, char **argv)
{
	/* Usamos int como entero de 4 bytes... si no usar uint32_t */
	assert(sizeof (int) == 4);

#if RUIDO
	srand(time(NULL) + getpid());
#endif

	if (argc == 3 && !strcmp(argv[1], "GET"))
		get(argv[2]);
	else if (argc == 3 && !strcmp(argv[1], "DEL"))
		del(argv[2]);
	else if (argc == 3 && !strcmp(argv[1], "PUT"))
		put(argv[2]);
	else if (argc == 2 && !strcmp(argv[1], "STATS"))
		stats();
	else
		usage(argv[0]);

	return 0;
}

