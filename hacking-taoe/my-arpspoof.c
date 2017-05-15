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
  int packet_socket, recv_length;
  char buffer[200];
  struct sockaddr_ll *address;
  

  if ((packet_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ARP))) == -1) {
    printf("error creating socket: %s", strerror(errno));
    exit(1);
  }

  if ((recv_length = recv(packet_socket, buffer, 199, 0)) == -1) {
    printf("error: %d", errno);
    exit(1);
  } else {
    printf("ARP packet... captured? Len: %d\n", recv_length);
    address = (struct sockaddr_ll *)buffer;

    decode_ethernet(buffer);
    
    printf("%hX\n", address->sll_family);
    printf("%hX\n", address->sll_protocol);
    printf("%u\n", address->sll_ifindex);
    printf("%hX\n", address->sll_hatype);
  }

  exit(0);
}
