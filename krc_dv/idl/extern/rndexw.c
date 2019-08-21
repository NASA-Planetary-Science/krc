#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"


IDL_FLOAT rndex (int argc, void *argv[])
/***********************************************************************
*_TITLE	rndex  C wrapper for Kieffer's RNDEX FORTRAN routine
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointer to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN fl1;          O      Return status code.
*
*_DESC  rndex is the IDL interface to Kieffer's RNDEX FORTRAN routine.

*       ARGUMENT MAPPING:   Count:  argc == 3
*        Pointer  Parameter         I/O  Description
*        argv[0]  REAL*4    *XIN;     I  Value to be located within array
*        argv[1]  REAL*4    A(*);     I  Array to be searched
*        argv[2]  INTEGER*4 *N:       I  Size of array A
*
*       USAGE:
*        Result = CALL_EXTERNAL(/F_VALUE, 'ftnwrap.so','rndex',$
*                               XIN,A,N)

*        Where "Result" is floating point index of the input
*        argument within a REAL*4 array
*
*
*_KEYS	HISTORY, SESSION_LOG, TERMINAL
*
*_HIST Sep 15 1997 Kris Becker USGS, Flagstaff Original Verison
*_END
***************************************************************************/
{
  float *XIN;
  float *A;
  int   *N;

  float result;

/* Declare the function prototype */
 extern float FTN_NAME(rndex) (float *xin, float a[], int *n);

/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 3) {
    (void) fprintf(stderr,
                   "IDL caller did not pass the 6 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  XIN        =  (float  *) argv[0];
  A          =  (float  *) argv[1];
  N          =  (int    *) argv[2];
  
/***********************************************************************
  Call the FORTRAN version of RNDEX
************************************************************************/
  result =  FTN_NAME(rndex) (XIN, A, N);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (result);
}
