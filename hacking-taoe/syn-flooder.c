#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h> // for socket functions
#include <netinet/in.h> // internet address structs & consts
#include <netinet/ip.h> // IP header structure

#include <errno.h> // error handling


int main(int argc, char *argv[]) {
  int raw_socket; // raw IP socket
  char packet[100];
  struct sockaddr_in *address;
  socklen_t addrlen;

  if (argc != 3) {
    printf("Usage:\n");
    printf("./syn-flooder <target-ip> <sender-ip>\n");
    exit(1);
  }
  
  if ((raw_socket = socket(AF_INET, SOCK_RAW, IPPROTO_TCP)) == -1) {
    printf("Error creating socket: %s\n", strerror(errno));
    exit(1);
  }


  sendto(packet_socket, packet, 99, 0, address, addrlen);
  
}
