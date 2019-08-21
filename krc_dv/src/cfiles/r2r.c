#include <stdio.h>
#include <stdlib.h>
#include <string.h>                              
#include "binding.h"

void FTN_NAME(r2r) (void *ibuf, void *obuf, int *nvals)
{
  void CC_NAME(r2r) (void *ibuf, void *obuf, int nvals);
  (void) CC_NAME(r2r) (ibuf, obuf, *nvals);
  return;
}


void r2r(void *ibuf, void *obuf, int nvals)
/***********************************************************************
*_Title	r2r Transfers longs from one buffer to another (very fast)
*_Args	Type	Variable	    I/O Description
*_Parm  void   	ibuf[];          I   Input buffer
*_Parm  void   	obuf[];          O   Output buffer
*_Parm  int      nvals;          I  Number of values to transfer
*                                       nvals > 0: # values to move
*                                       nvals < 0: fill -nvals elements in
*                                                   obuf with first value
*                                                   in ibuf  
*
*_Desc	Transfers the contents of the source buffer (ibuf) to the
*	    destination buffer (obuf) for the number of int egers specified
*	    by nvals.   If nvals < 0, obuf is initialized with the first
*       value in ibuf (ibuf[0]).

*	    This routine also handles overlapping input and output buffers.

*_Hist  Aug 29 2003 Kris Becker USGS, Flagstaff Original Version
*_End
***********************************************************************/
{
  if (nvals > 0) {
    if (ibuf != obuf) {
      (void) memmove(obuf, ibuf, (size_t) nvals*sizeof(float));
    }
  }
  else if (nvals < 0) {
    if (sizeof(int) == 4) {
      int  n, i;
      int *ib, *ob;
      ib = (int *) ibuf;
      ob = (int *) obuf;
      for (i = 0, n = -nvals ; i < n ; i++) {
        ob[i] = ib[0];
      }
    }
    else if (sizeof(long) == 4) {
      int  n, i;
      long *ib, *ob;
      ib = (long *) ibuf;
      ob = (long *) obuf;
      for (i = 0, n = -nvals ; i < n ; i++) {
        ob[i] = ib[0];
      }
    }
    else {
      (void) fprintf(stderr, "r2r - Unable to determine 4-byte variable size!");
      abort();
    }
  }
  return;

}
