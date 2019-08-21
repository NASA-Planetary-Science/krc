#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

/* This interface routine modeled on  exfunctionw.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/  .f  */

IDL_FLOAT elevate (int argc, void *argv[])
{
  float *ARG1;
  float *ARG2;
  float result;

 extern float FTN_NAME(elevate) (float *ARG1, float *ARG2  );

  if (argc != 2) {
    (void) fprintf(stderr,
      "IDL caller of elevate did not pass the 2 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  
  result =  FTN_NAME(elevate) (ARG1, ARG2 );

  return (result);
}
