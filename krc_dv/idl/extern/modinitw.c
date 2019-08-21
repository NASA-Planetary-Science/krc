#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"
/* This interface routine modeled on  exroutinew.c  See the comments there.
       the Fortran source is at /home/hkieffer/tes/mod/    */

extern int f__init;

IDL_RETURN modinit (int argc, void *argv[])
{    
extern  int FTN_NAME(modinit) (void);

  FILE *tmp;
  FILE *t_stdin, *t_stdout, *t_stderr;


  if (argc != 0) {
    (void) fprintf(stderr,
      "IDL caller of modinit did not pass the 0 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }
/*
  tmp = fopen("/dev/null", "w");
  t_stderr = _IO_stderr;
  t_stdin = _IO_stdin;
  t_stdout = _IO_stdout;
  _IO_stderr = _IO_stdout = _IO_stdin = tmp;
  f_init();

  _IO_stderr = t_stderr;
  _IO_stdin = t_stdin;
  _IO_stdout = t_stdout;

*/
  (void) FTN_NAME(modinit) ();

  return (0);
}
