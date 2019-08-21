

#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN kratlsq (int argc, void *argv[])
{
  int *ARG1; /* always preceed with *, no trailing [] for scalers or arrays */
  int *ARG2;
  double *ARG3;
  double *ARG4;
  int *ARG5;
  int *ARG6;
  double *ARG7;  
  double *ARG8;

/***********************************************************************
 Declare the function prototype;
         preceed scalars with * , append [] to arrays
	If no arguments, use { void} 
************************************************************************/ 
  extern  int FTN_NAME(kratlsq) (int *arg1, int *arg2,  double ARG3[], double ARG4[], int *arg5, int *arg6, double ARG7[], double ARG8[]);

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 8) {
    (void) fprintf(stderr,
      "IDL caller of kratlsq did not pass the 8 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (int *) argv[0];
  ARG2  =  (int *) argv[1];
  ARG3  =  (double *) argv[2];
  ARG4  =  (double *) argv[3];
  ARG5  =  (int *) argv[4];
  ARG6  =  (int *) argv[5];
  ARG7  =  (double *) argv[6];
  ARG8  =  (double *) argv[7];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  (void) FTN_NAME(kratlsq) (ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7,ARG8);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (0);
}
