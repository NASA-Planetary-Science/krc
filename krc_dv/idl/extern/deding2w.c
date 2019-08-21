#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN deding2 (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c   See the comments there.
       the Fortran source is at -/hkieffer/rad/mie/deding2.f    */
    
{
  float *ARG1;  
  float *ARG2;
  float *ARG3;
  float *ARG4;
  float *ARG5;
  float *ARG6;
  float *ARG7;
  float *ARG8;

 extern int FTN_NAME(deding2) (float *ARG1,  float *ARG2, float *ARG3 ,float *ARG4 ,float *ARG5, float *ARG6, float *ARG7, float ARG8[]);

 
  if (argc != 8) {
    (void) fprintf(stderr,
      "IDL caller of deding2 did not pass the 8 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (float  *) argv[2];
  ARG4  =  (float  *) argv[3];
  ARG5  =  (float  *) argv[4];
  ARG6  =  (float  *) argv[5];
  ARG7  =  (float  *) argv[6];
  ARG8  =  (float  *) argv[7];
  
  (void) FTN_NAME(deding2) (ARG1, ARG2, ARG3, ARG4,ARG5,ARG6,ARG7,ARG8);

  return (0);
}
