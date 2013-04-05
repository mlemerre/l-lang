#include <stdlib.h> //exit,malloc
#include <stdio.h> // printf

/* This is the current runtime library for the L language.

   Note that the calling convention will change in the future, so
   this file should be kept small. */

void * assert_(void* env, int *value)
{
  fprintf(stderr, "Assertion ");
  int res = *value;
  if(res) {
    fprintf(stderr, "passed: received %d\n", res);
  }
  else
    {
      fprintf(stderr, "failed\n");
      exit( 3);
    }
  return 0;
}
void *assert[2]={&assert_,0};

void * not_(void *env, int *bool)
{
  int res = *bool;
  int *ret = malloc(sizeof(int));
  if(res) {
    *ret = 0;
  }
  else *ret = 1;

  return ret;
}
void *not[2]={&not_,0};

void * match_failure_(void *env, int *arg)
{
  printf("###Match failure\n");
  exit(3);
  return 0;
}
void *match_failure[2]={&match_failure_,0};

/* NOte: for functions with multiple arguments, we would have a
   tuple, with an extra level of indirection. */
