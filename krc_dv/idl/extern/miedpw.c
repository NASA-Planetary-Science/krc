
#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN miedp (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c   See the comments there.
       the Fortran source is at -/hkieffer/rad/mie/miedp.f    */
    
{
  double *ARG1;   /* always preceed with *, no [] for arrays  */
  double *ARG2;
  double *ARG3;
  double *ARG4;
  double *ARG5;
  double *ARG6;

/* Declare the function prototype;
        append [] to arrays, preceed scalars with *       */
extern  int FTN_NAME(miedp) (double *ARG1,  double *ARG2, double *ARG3 ,double *ARG4 ,double *ARG5, double *ARG6);

 
  if (argc != 6) {
    (void) fprintf(stderr,
      "IDL caller of miedp did not pass the ?3 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (double  *) argv[0];
  ARG2  =  (double  *) argv[1];
  ARG3  =  (double  *) argv[2];
  ARG4  =  (double  *) argv[3];
  ARG5  =  (double  *) argv[4];
  ARG6  =  (double  *) argv[5];
  
  (void) FTN_NAME(miedp) (ARG1, ARG2, ARG3, ARG4,ARG5,ARG6);

  return (0);
}
