#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/types.h>
#include <unistd.h>

int spawn(char* program, char *arg_list[])
{
  pid_t child_pid;

  // duplicate this process
  child_pid = fork();
  if (child_pid != 0) {
    // this is the parent process
    return child_pid;
  } else {
    // this is the child process
    // now execute PROGRAM, searching for it in the path
    execvp(program, arg_list);
    // the execvp function returns only if an error occurs
    fprintf(stderr, "an error occurred in execvp\n");
    abort();
  }
}

int main()
{
  char* arg_list[] = {
    "ls", // argv[0], the name of the program
    "-l",
    "/",
    NULL // the arg list must end with a NULL
  };

  // spawn a child process running the "ls" command
  spawn(arg_list[0], arg_list);

  printf("done with main program\n");
  return 0;
}
