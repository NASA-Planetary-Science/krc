/*********************************************************
in this example,  ? preceeds things that must be changed
 there are no ? in the executable file

replace ?name with the routine name without the .f ; 5 places
replace ?3 with the number of arguments; 2 places
extend or shorten the ARG1,ARG2,ARG3  ?... list in 4 places
set the type of each argument in 3 places
      float  for REAL*4
      int    for INTEGER*4  (IDL should define as Long)
      double for REAL*8
      IDL_STRING for CHARACTER
delete these comments
*************************************************************/

#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN ?name (int argc, void *argv[])

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at ??    */
    
/***********************************************************************
*_TITLE	?name  C wrapper for IDL to use FORTRAN routine of same name
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
  float *ARG1; /* always preceed with *, no trailing [] for scalers or arrays */
  int   *ARG2;
  IDL_STRING *ARG3;

  int ARG3_len;                /* String length */


/***********************************************************************
 Declare the function prototype;
         preceed scalars with * , append [] to arrays
	If no arguments, use { void} 
************************************************************************/ 
extern  int FTN_NAME(?name) (float ARG1[], int ARG2[], IDL_STRING *ARG3  ?... , 
                       int ARG3_len, ?for each string argument...? );

 
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
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (float  *) argv[0];
  ARG2  =  (int    *) argv[1];
  ARG3  =  (IDL_STRING *) argv[2];
  ?...
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  (void) FTN_NAME(?name) (ARG1, ARG2, IDL_STRBUF(ARG3) ?... ,
                          strlen(IDL_STRBUF(ARG3)), ?...);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (0);
}
