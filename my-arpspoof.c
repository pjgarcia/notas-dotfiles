#include <stdio.h>
#include <string.h>
#include <libnet.h>

void usage() {
  printf("Usage: synflood ip port\n");
  exit(1);
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

  libnet_context = libnet_init(LIBNET_LINK, device, errbuf);
  if (libnet_context == NULL) {
    fprintf(stderr, "%s", errbuff);
    exit(EXIT_FAILURE);
  }

  local_ip = libnet_get_ipaddr4(libnet_context);
  hwaddr = libnet_get_hwaddr(libnet_context);

  t = libnet_build_arp(ARPHRD_ETHER, // format of hardware address
		       ETHERTYPE_IP, // protocol
		       6,            // hardware address length
		       4,            // protocol address length
		       ARPOP_REPLY,  // ARP operation type
		       hwaddr->ether_addr_octet,     // sender's hardware address
		       (u_int8_t *)&local_ip, // sender's protocol address
		       enet_dst,
		       (u_int8_t *)&i,
		       NULL,
		       0,
		       libnet_context,
		       0);
  if (t == -1) {
    fprintf(stderr, "Can't build ARP header: %s\n", libnet_geterror(l));
    exit(EXIT_FAILURE);
  }

  t = libnet_autobuild_ethernet(enet_dst, ETHERTYPE_ARP, l);

  if (t == -1) {
    fprintf(stderr, "Can't build ethernet header: %s\n", libnet_geterror(l));
    exit(EXIT_FAILURE);
  }

  return 0;
}
