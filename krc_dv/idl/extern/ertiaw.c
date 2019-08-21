#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

/* This interface routine modeled on  exfunctionw.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/  .f  */

IDL_FLOAT ertia (int argc, void *argv[])
{
  float *ARG1;
  float *ARG2;
  float *ARG3;
  float *ARG4;
  float result;
 extern float FTN_NAME(ertia) (float *ARG1, float *ARG2, float ARG3[] , float *ARG4 );

  if (argc != 4) {
    (void) fprintf(stderr,
      "IDL caller of ertia did not pass the 4 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (float  *) argv[2];
  ARG4  =  (float  *) argv[3];
  
  result =  FTN_NAME(ertia) (ARG1, ARG2, ARG3, ARG4 );
  return (result);
}
