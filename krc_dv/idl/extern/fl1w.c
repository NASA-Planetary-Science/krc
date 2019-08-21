#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"


IDL_RETURN fl1 (int argc, void *argv[])
/***********************************************************************
*_TITLE	fl1  C wrapper for Kieffer's FL1 FORTRAN routine
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointer to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN fl1;          O      Return status code.
*
*_DESC  fl1 is the IDL interface to Kieffer's FL1 FORTRAN routine.

*       ARGUMENT MAPPING:   Count:  argc == 6
*        Pointer  Parameter         I/O  Descriptoin
*        argv[0]  REAL*4    A(*)      I  Input Array
*        argv[1]  REAL*4    B(*);     O  Output Array, must not overlap
*        argv[2]  INTEGER*4 NIN;      I  Input number of points in array
*        argv[3]  INTEGER*4 KIN;      I  Input filter wing size
*        argv[4]  FLOAT4    *G;       I  Input wing shape
*        argv[5]  FLOAT4    T(*);     I  Half weighting function
*
*       USAGE:
*        Result = CALL_EXTERNAL('ftnwrap.so','fl1',$
*                               A,B,NIN,KIN,G,T)
*
*
*_KEYS	HISTORY, SESSION_LOG, TERMINAL
*
*_HIST Sep 15 1997 Kris Becker USGS, Flagstaff Original Verison
*_END
***************************************************************************/
{
  float *A;
  float *B;
  int   *NIN;
  int   *KIN;
  float *G;
  float *T;

/* Declare the function prototype */
extern int FTN_NAME(fl1) (float A[], float B[], int *NIN, int *KIN,
                     float *G, float T[]);
 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 6) {
    (void) fprintf(stderr,
                   "IDL caller did not pass the 6 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  A          =  (float  *) argv[0];
  B          =  (float  *) argv[1];
  NIN        =  (int    *) argv[2];
  KIN        =  (int    *) argv[3];
  G          =  (float  *) argv[4];
  T          =  (float  *) argv[5];
  
/***********************************************************************
  Call the FORTRAN version of FL1
************************************************************************/
  (void) FTN_NAME(fl1) (A, B, NIN, KIN, G, T);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (0);
}
