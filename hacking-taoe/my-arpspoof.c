#include <stdio.h>
#include <string.h>
#include <libnet.h>

void usage() {
  printf("Usage: my-arpspoof poisoned_ip poisoned_mac impersonated_ip\n");
  printf("-------------------------------------------\n");
  printf("Link layer:\tmy_mac\t\tpoisoned_mac\n");
  printf("IP Layer:\timpersonated_ip\tpoisoned_ip\n");
  printf("-------------------------------------------\n");
  exit(1);
}

// gets a string representing a MAC address and
// tranforms it in a 6 bytes number
int a_to_mac(char *a, struct ether_addr* mac) {
  
}

int main(int argc, char *argv[]) {
  char device[10] = "wlan0";
  libnet_t *libnet_context;
  char errbuff[LIBNET_ERRBUF_SIZE];
  u_int32_t local_ip;
  libnet_ptag_t ethernet_tag;
  libnet_ptag_t arp_tag;

  struct libnet_ether_addr *hwaddr;
  
  if (argc < 3)
    usage();

  libnet_context = libnet_init(LIBNET_LINK, device, errbuff);
  if (libnet_context == NULL) {
    fprintf(stderr, "%s", errbuff);
    exit(EXIT_FAILURE);
  }

  return 0;
}
