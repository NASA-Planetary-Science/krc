#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/  */
    
IDL_RETURN mseas (int argc, void *argv[])

{
  float *ARG1; /* always preceed with *, no trailing [] for scalars or arrays */
  int   *ARG2;
  int   *ARG3;
  IDL_STRING   *ARG4;
  int   *ARG5;


/* Declare the function prototype;
         preceed scalars with * , append [] to arrays  */
extern int FTN_NAME(mseas) (float *ARG1, int *ARG2, int *ARG3,  char *ARG4,
                     int *ARG5, int ARG4_len );

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 5) {
    (void) fprintf(stderr,
      "IDL caller of mseas did not pass the 5 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (float *) argv[0];
  ARG2  =  (int   *) argv[1];
  ARG3  =  (int   *) argv[2];
  ARG4  =  (IDL_STRING   *) argv[3];
  ARG5  =  (int   *) argv[4];

/***********************************************************************
  Call the FORTRAN version
************************************************************************/
/*  s_len = strlen(IDL_STRBUF(ARG4)); */
  (void) FTN_NAME(mseas) (ARG1, ARG2, ARG3, IDL_STRBUF(ARG4), ARG5,
                          strlen(IDL_STRBUF(ARG4)) );
  return (0);
}
