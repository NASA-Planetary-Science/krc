#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN errdel (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/errdel.f  */
    
{
  int *ARG1;

extern  int FTN_NAME(errdel) (int *ARG1);

  if (argc != 1) {
    (void) fprintf(stderr,
      "IDL caller of errdel did not pass the 1 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (int  *) argv[0];
  
  (void) FTN_NAME(errdel) (ARG1 );

  return (0);
}
