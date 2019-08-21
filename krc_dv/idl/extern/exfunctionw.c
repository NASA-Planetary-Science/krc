/*********************************************************
In this example,  ? preceeds things that must be changed
 there are no ? in the executable file

Replace ?name with the routine name without the .f ; 5 places
Replace ?3 with the number of arguments; 2 places
Check the type of the  FTN_NAME( statement
Extend or shorten the ARG1,ARG2,ARG3  ?... list in 4 places
Set the type of each argument in 3 places
      float for REAL*4
      int   for integer
Delete these comments
*************************************************************/

#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_FLOAT ?name (int argc, void *argv[])
/***********************************************************************
*_TITLE	?name  C wrapper for IDL to use FORTRAN routine of same name
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointers to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN ?name;          O      Return status code.
*
*_DESC derived from skeleton,
*  see original fortran source code for argument description
*
*_HIST ?Sep 15 1997  etc
*_END
**************************************************************************
 List all arguments by type; arrays and scalars are the same here  */
{
  float *ARG1;
  float *ARG2;
  int   *ARG3;

  float result;

/* Declare the function prototype; 
           Preceed scalars with *     Append [] to arrays      */
extern  float FTN_NAME(?name) (float ARG1[], float ARG2[], int *ARG3 ?...  );

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != ?3) {
    (void) fprintf(stderr,
      "IDL caller of ?name did not pass the ?3 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism. arrays and scalars are the same
    List by type       Note: argv[_] less than ARG_ 
************************************************************************/
  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (int    *) argv[2];
  ?...
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  result =  FTN_NAME(?name) (ARG1, ARG2, ARG3 ?...  );

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (result);
}
