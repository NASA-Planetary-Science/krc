#include <stdio.h>
#include "binf5.h"
 

void u_swapn(int nelems, int esize, void *ibuf, void *obuf)
/***********************************************************************
*_Title u_swapn Swap n-byte elements
*_Args  Type    Name    I/O    Description
*_Parm  int     nelems;  I     Number of elements to swap
*_Parm  int     esize;   I     Element size.  Need not be even
*_Parm  void    *ibuf;   I     Input buffer containing elements to swap
*_Parm  void    *obuf;   O     Output buffer to receive swapped bytes

*_Descr u_swapn swaps bytes in arbitrarily sized elements.  It has
*       the unique features that do not require the elements to be
*       even-sized and are not required to be aligned on any boundary.

*       Note that "ibuf" and "obuf" can be the same buffer.  If they
*       are not then this routine will first move the input to the
*       output buffer and then swap in place.  This is the only way
*       to ensure that compiler optimizations do not cause this
*       operation to fail.

*       Note that because of this routines generality, it may be a
*       a bit slower than other alternatives.

*_Hist  Nov 30 1999 Kris Becker, USGS, Flagstaff Original Version
*       Mar 04 2002 KJB - Removed ISIS dependencies
*_End
************************************************************************/
{
  register unsigned char *op;           /* Output buffer pointer */
  register unsigned char tmp;           /* Temp variable */
  register int  i;                      /* Loop counter */
  register int  isize;                  /* Element size */

  register int  bot0, top0;             /* Initial top and bottom */
  register int  bot1, top1;             /* Secondary top and bottom */
  register int  nswp, iswp;             /* Swap counts */ 

/***************************************************************
  Set initial swapping parameters 
****************************************************************/
  if (ibuf != obuf) {
    int nbytes = nelems * esize;
    (void) u_move1(nbytes, ibuf, obuf);
  }

  op = (unsigned char *) obuf;
  nswp = (esize + 1) / 2;

/***************************************************************
  Do the conversion
****************************************************************/
  isize = esize;
  for (i=0, bot0=0, top0=isize-1 ; i++ < nelems ; bot0+=isize, top0+=isize) {
    for (iswp = 0, bot1 = bot0, top1 = top0; iswp++ < nswp ; bot1++, top1--) {
      tmp  = op[bot1];
      op[bot1] = op[top1];
      op[top1] = tmp;
    }
  }

  return;
}
