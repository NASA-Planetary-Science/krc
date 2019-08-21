#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_FLOAT eqoftime (int argc, void *argv[])
/***********************************************************************
*_TITLE	eqoftime  C wrapper for FORTRAN routine
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointer to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN ;          O      Function value
*
*_DESC  this is the IDL interface to Kieffer's FORTRAN routine of same name

*
*       USAGE:
*        Result = CALL_EXTERNAL(/F_VALUE, 'ftnwrap.so','eqoftime', arg1,arg2... )
*
*_KEYS	HISTORY, SESSION_LOG, TERMINAL
*
*_HIST Sep 15 1997 Kris Becker USGS, Flagstaff Original Version
*  97sep15  HHK modify Beckers skeleton
*_END
***************************************************************************/
{
  float *ARG1;
  float *ARG2;
  float *ARG3;

  float result;

/* Declare the function prototype */
 extern float FTN_NAME(eqoftime) (float *ARG1, float *ARG2, float *ARG3);

/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 3) {
    (void) fprintf(stderr,
                   "IDL caller passed wrong number of arguments, %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1 = (float  *) argv[0];
  ARG2 = (float  *) argv[1];
  ARG3 = (float  *) argv[2];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  result =  FTN_NAME(eqoftime) (ARG1,ARG2,ARG3);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (result);
}
