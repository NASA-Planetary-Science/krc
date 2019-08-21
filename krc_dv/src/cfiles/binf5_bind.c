#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "binding.h"


void CC_NAME(binf5)(char action[], char filename[], char header[],
                    int header_size, int id[], unsigned char data[], int *ret)
{
 
  extern void FTN_NAME(binf5)(char action[], char filename[], char header[],
                              int id[], unsigned char data[], int *ret,
                              int action_size, int filename_size,
                              int header_size);
  char *filename_mem, *header_mem;
  int filename_size;

  filename_size = strlen(filename) + 1;
  filename_mem = b_c2fstr(1, filename, filename_size, NULL, filename_size-1,
                          B_DISCARD_PAD);
  header_mem = b_c2fstr(1, header, header_size, NULL, header_size-1,
                        B_DISCARD_PAD);
 
  (void) FTN_NAME(binf5) (action, filename_mem, header_mem, id, data, ret,
                          1, filename_size-1, header_size-1);
  (void) b_f2cstr(1, header_mem, (header_size-1), B_DISCARD_PAD, header, 
                  header_size);
  return;
}

void  FTN_NAME(u_move1) (int *nbytes, void *ibuf, void *obuf)
{
  extern void CC_NAME(u_move1) (int nbytes, void *ibuf, void *obuf);
  (void) CC_NAME(u_move1) ((*nbytes), ibuf, obuf);
  return;
}

void  FTN_NAME(u_move4) (int *nlongs, int ibuf[], int obuf[])
{
  extern void CC_NAME(u_move4) (int longs, int ibuf[], int obuf[]);
  (void) CC_NAME(u_move4) ((*nlongs), ibuf, obuf);
  return;
}

void FTN_NAME(u_swapn) (int *nelem, int *esize, void *ibuf, void *obuf)
{
  extern void CC_NAME(u_swapn) (int nelem, int esize, void *ibuf, void *obuf);

  (void) CC_NAME(u_swapn) ((*nelem), (*esize), ibuf, obuf);
  return;
}

