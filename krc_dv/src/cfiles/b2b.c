#include <stdio.h>
#include <string.h>                              
#include "binding.h"

void FTN_NAME(b2b) (void *ibuf, void *obuf, int *nvals)
{
  void CC_NAME(b2b) (void *ibuf, void *obuf, int nvals);
  (void) CC_NAME(b2b) (ibuf, obuf, *nvals);
  return;
}


void b2b(void *ibuf, void *obuf, int nvals)
/***********************************************************************
*_Title	b2b Transfers bytes from one buffer to another (very fast)
*_Args	Type	Variable	    I/O Description
*_Parm  void     ibuf[];         I   Input buffer
*_Parm  void   	 obuf[];         O   Output buffer
*_Parm  int      nvals;          I  Number of values to transfer
*                                       nvals > 0: # values to move
*                                       nvals < 0: fill -nvals elements in
*                                                   obuf with first value
*                                                   in ibuf  
*
*_Desc	Transfers the contents of the source buffer (ibuf) to the
*	    destination buffer (obuf) for the number of bytes specified
*	    by nvals.   If nvals < 0, obuf is initialized with the first
*       value in ibuf (ibuf[0]).

*	    This routine also handles overlapping input and output buffers.

*_Hist  Aug 29 2003 Kris Becker USGS, Flagstaff Original Version
*_End
***********************************************************************/
{
  if (nvals > 0) {
    if (ibuf != obuf) {
      (void) memmove(obuf, ibuf, (size_t) nvals);
    }
  }
  else if (nvals < 0) {
    int c;
    c = *((char *) ibuf); 
    (void) memset(obuf, c, (size_t) -nvals);
  }
  return;

}
