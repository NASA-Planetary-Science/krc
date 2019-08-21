#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/tifov.f  */
 
IDL_RETURN tifov (int argc, void *argv[])

{
  float *ARG1;
  float *ARG2;
  float *ARG3;
  float *ARG4;
  float *ARG5;


extern  int FTN_NAME(tifov) (float *ARG1, float *ARG2, float *ARG3, float *ARG4,  float ARG5[]);

  if (argc != 5) {
    (void) fprintf(stderr,
      "IDL caller of tifov did not pass the 5 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (float  *) argv[2];
  ARG4  =  (float  *) argv[3];
  ARG5  =  (float  *) argv[4];
  
  (void) FTN_NAME(tifov) (ARG1, ARG2, ARG3, ARG4, ARG5 );
  
  return (0);
}
