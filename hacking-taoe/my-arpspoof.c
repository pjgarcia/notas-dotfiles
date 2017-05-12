//#include <sys/types.h> included by net/ethernet.h
#include <sys/socket.h> 
#include <linux/if_packet.h> 
#include <net/ethernet.h>

int main(int argc, char *argv[]) {
  int packet_socket;
  
  packet_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ARP));
}
