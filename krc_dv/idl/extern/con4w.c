#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_RETURN con4 (int argc, void *argv[])
{
  int   *ARG1;   /* always preceed with *, no [] for arrays  */
  float *ARG2;
  float *ARG3;
 int    *ARG4;
 float  *ARG5;
 int    *ARG6;
 float  *ARG7;
 float  *ARG8;
 int    *ARG9;
extern int FTN_NAME(con4) (int *ARG1, float ARG2[], float ARG3[], 
       int *ARG4, float ARG5[], int *ARG6, float ARG7[], float ARG8[], int *ARG9);

  if (argc != 9) {
    (void) fprintf(stderr,
      "IDL caller of con4 did not pass the 9 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

  ARG1  =  (int    *) argv[0];
  ARG2  =  (float  *) argv[1];
  ARG3  =  (float  *) argv[2];
  ARG4  =  (int    *) argv[3];
  ARG5  =  (float  *) argv[4];
  ARG6  =  (int    *) argv[5];
  ARG7  =  (float  *) argv[6];
  ARG8  =  (float  *) argv[7];
  ARG9  =  (int    *) argv[8];
  
  (void) FTN_NAME(con4) (ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8, ARG9 );
  return (0);
}
