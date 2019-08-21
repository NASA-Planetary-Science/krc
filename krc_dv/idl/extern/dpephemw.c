
#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN dpephem (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/lunar/DE/    */

{
  double *ARG1; /* always preceed with *, no trailing []  */
  double *ARG2;
  double *ARG3;
  double *ARG4;
  double *ARG5;

/***********************************************************************
 Declare the function prototype;
         preceed scalars with * , append [] to arrays
	If no arguments, use { void} 
************************************************************************/ 
 extern int FTN_NAME(dpephem) (double *ARG1, double ARG2[], double ARG3[],
                         double ARG4[],double ARG5[]);

/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 5) {
    (void) fprintf(stderr,
      "IDL caller of dpephem did not pass the 5 required arguments, but %ld",
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
  ARG5  =  (double *) argv[4];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  (void) FTN_NAME(dpephem) (ARG1, ARG2,ARG3, ARG4,ARG5);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (0);
}
