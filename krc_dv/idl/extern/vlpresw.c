#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

/* This interface routine modeled on  exfunctionw.c  See the comments there.
       the Fortran source is at /home/hkieffer/krc/tes/  .f  */

IDL_FLOAT vlpres (int argc, void *argv[])
{
  int   *ARG1;
  float *ARG2;

  float result; 

/*  List by type.   Preceed scalars with *    Append [] to arrays     */
 extern float FTN_NAME(vlpres) (int *ARG, float *ARG2 );

  if (argc != 2) {
    (void) fprintf(stderr,
      "IDL caller of vlpres did not pass the 2 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (int    *) argv[0]; 
  ARG2  =  (float  *) argv[1];
  
  result =  FTN_NAME(vlpres) (ARG1, ARG2 );

  return (result);
}
