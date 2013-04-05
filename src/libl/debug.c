#include <stdlib.h>
#include <stdio.h>

static void * debug_int_(void *env, int *arg)
{
  fprintf(stderr, "###Debug: param=%d (0x%x)\n", *arg, *arg);
  int *ret = malloc(sizeof(int));
  *ret = *arg;
  return ret;
}
void *debug_int[2]={&debug_int_,0};
