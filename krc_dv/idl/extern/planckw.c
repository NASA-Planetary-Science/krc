#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"


/* FORTRAN oxurce is at:  /home/hkieffer/lib/for/rad/planck.f */

IDL_FLOAT planck (int argc, void *argv[])
/* List all arguments by type; arrays same as scalars here  */
{
  int   *ARG1;
  float *ARG2;
  float *ARG3;
  float result;

/* Declare the function prototype; 
          append [] to arrays, preceed scalars with *    */
 extern float FTN_NAME(planck) (int *ARG1, float *ARG2, float *ARG3 );

   if (argc != 3) {
    (void) fprintf(stderr,
      "IDL caller of planck did not pass the 3 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/*Map the arguments to IDL passing mechanism; arrays same as scalars here   */
  ARG1  =  (int  *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (float    *) argv[2];

  result =  FTN_NAME(planck) (ARG1, ARG2, ARG3);

  return (result);
}
