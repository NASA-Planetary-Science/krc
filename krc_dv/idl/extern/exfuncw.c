/*********************************************************
 Short skeleton for function
in this example,  ? preceeds things that must be changed
 there are no ? in the executable file

replace ?name with the routine name without the .f ; 5 places
replace ?3 with the number of arguments; 2 places
 cheect the type of the  FTN_NAME( statement
extend or shorten the ARG1,ARG2,ARG3  ?... list in 4 places
set the type of each argument in 3 places
      float for REAL*4
      int   for integer
delete these comments
*************************************************************/

#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_FLOAT ?name (int argc, void *argv[])
/* List all arguments by type; arrays same as scalars here  */
{
  float *ARG1;
  float *ARG2;
  int   *ARG3;
 ?...
  float result;

/* Declare the function prototype; 
          append [] to arrays, preceed scalars with *    */
 extern float FTN_NAME(?name) (float ARG1[], float ARG2[], int *ARG3 ?...  );

   if (argc != ?3) {
    (void) fprintf(stderr,
      "IDL caller of ?name did not pass the ?3 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/*Map the arguments to IDL passing mechanism; arrays same as scalars here   */
  ARG1  =  (float  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (int    *) argv[2];
  ?...
  
  result =  FTN_NAME(?name) (ARG1, ARG2, ARG3 ?...  );

  return (result);
}
