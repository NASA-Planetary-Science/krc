#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_FLOAT ccor2d (int argc, void *argv[])
/***********************************************************************
*_TITLE	ccor2d  C wrapper for IDL to use FORTRAN routine of same name
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointers to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN fl1;          O      Return status code.
*
*_DESC derived from skeleton,
*  see original fortran source code for argument description
*
*_HIST ?Sep 15 1997  etc
*_END
***************************************************************************/
{
  float *ARG1;
  float *ARG2;
  int   *ARG3;
  int   *ARG4;
  int   *ARG5;
  int   *ARG6;

  float result;

/* Declare the function prototype, append [] to arrays */
extern float FTN_NAME(ccor2d) (float ARG1[], float ARG2[], int *ARG3,
             int *ARG4,  int *ARG5, int *ARG6);

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 6) {
    (void) fprintf(stderr,
      "IDL caller of ccor2d did not pass the 6 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (int    *) argv[2];
  ARG4  =  (int    *) argv[3];
  ARG5  =  (int    *) argv[4];
  ARG6  =  (int    *) argv[5];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  result =  FTN_NAME(ccor2d) (ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (result);
}
