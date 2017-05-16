//#include <sys/types.h> included by net/ethernet.h
#include <sys/socket.h> 
#include <linux/if_packet.h> 
#include <net/ethernet.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>

#include "../hacking.h"
#include "../hacking-network.h"

void decode_ethernet(const u_char *header_start) {
  int i;
  const struct ether_hdr *ethernet_header;

  ethernet_header = (const struct ether_hdr *)header_start;
  printf("[[ Layer 2 :: Ethernet Header ]]\n");
  printf("[ Source: %02x", ethernet_header->ether_src_addr[0]);
  for (i = 1; i < 6; i++) {
    printf(":%02x", ethernet_header->ether_src_addr[i]);
  }

  printf("\tDest: %02x", ethernet_header->ether_dest_addr[0]);
  for (i = 1; i < 6; i++) {
    printf(":%02x", ethernet_header->ether_dest_addr[i]);
  }
  printf("\tType: %hu ]\n", ethernet_header->ether_type);
}

int main(int argc, char *argv[]) {
  int packet_socket, recv_length, i;
  char buffer[200];
  struct sockaddr_ll *address;
  socklen_t *addrlen;

  if ((address = (struct sockaddr_ll *) malloc(sizeof(struct sockaddr_ll))) == NULL) {
    printf("%s\n", "error on malloc");
    exit(1);
  }

  *addrlen = sizeof(struct sockaddr_ll);
  

  if ((packet_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ARP))) == -1) {
    printf("error creating socket: %s", strerror(errno));
    exit(1);
  }

  if ((recv_length = recvfrom(packet_socket, buffer, 199, 0,(struct sockaddr *)address, addrlen)) == -1) {
    printf("error: %d", errno);
    exit(1);
  } else {
    printf("ARP packet... captured? Len: %d\n", recv_length);

    decode_ethernet(buffer);
    
    printf("ssl_family: \t0x%hX\n", address->sll_family);
    printf("ssl_protocol\t0x%hX\n", ntohs(address->sll_protocol));
    printf("ssl_ifindex:\t  %u\n", address->sll_ifindex);
    printf("ssl_hatype: \t0x%hX\n", address->sll_hatype);
    printf("ssl_halen:  \t0x%hhX\n", address->sll_halen);
    printf("ssl_addr:   \t");

    for (i = 0; i < address->sll_halen; i++) {
      if (i > 0) {
	printf(":");
      }
      printf("%hhX", address->sll_addr[i]);
    }
    printf("\n");
  }

  exit(0);
}
