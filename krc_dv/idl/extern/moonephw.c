
#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN mooneph (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/lunar/DE/    */

{
  double *ARG1; /* always preceed with *, no trailing []  */
  double *ARG2;
  double *ARG3;
  double *ARG4;

/***********************************************************************
 Declare the function prototype;
         preceed scalars with * , append [] to arrays
	If no arguments, use { void} 
************************************************************************/ 
extern  int FTN_NAME(mooneph) (double *ARG1, double ARG2[], double ARG3[],
                         double ARG4[]);

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 4) {
    (void) fprintf(stderr,
      "IDL caller of mooneph did not pass the 4 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (double *) argv[0];
  ARG2  =  (double *) argv[1];
  ARG3  =  (double *) argv[2];
  ARG4  =  (double *) argv[3];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  (void) FTN_NAME(mooneph) (ARG1, ARG2,ARG3, ARG4);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (0);
}
