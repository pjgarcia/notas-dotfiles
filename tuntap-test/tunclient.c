#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tun-alloc.h"

int main(int argc, char *argv[]) {
  int nread = 0;
  int tun_fd;
  int err;
  char tun_name[IFNAMSIZ];
  char buffer[1500];
  
  strcpy(tun_name, "tun77");
  tun_fd = tun_alloc(tun_name, IFF_TUN | IFF_NO_PI);

  if (tun_fd < 0) {
    perror("ERROR allocating interface\n");
    exit(1);
  }

  while (1) {
    nread = read(tun_fd, buffer, sizeof(buffer));
    if (nread < 0) {
      printf("ERROR reading from interface\n");
      close(tun_fd);
      exit(1);
    }

    printf("read %d bytes from device %s\n", nread, tun_name);
  }

		    
}
