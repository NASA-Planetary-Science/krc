#include <stdio.h>
#include <string.h>
#include "isisdef.h"
#include "isistypes.h"
#include "u.h"


void u_move1(INT4 nbytes, void * ibuf, void *obuf)
/**********************************************************************
*_Title	u_move1 Transfers bytes from one buffer to another (very fast)
*_Args	Type	Variable	I/O	Description
*_Parm  INT4    nbytes;          I      Number of bytes to transfer
*_Parm  void	*ibuf;           I      Input buffer
*_Parm	void  	*obuf;           O      Output buffer
*
*_Desc	Transfers the contents of the source buffer (IBUF) to the
*	destination buffer (OBUF) for the number of bytes specified
*	by NBYTES.   If NBYTES <= 0, then this routine returns without
*	any action.  This routine also handles overlapping input and
*	output buffers.  See also u_move2 and u_move4.
*
*_Hist   Aug 13 1990 Kris Becker USGS, Flagstaff Original Macro Version
*	 July 16, 1991 Steve Wampler USGS, Flagstaff rewritten in C
*        Jan 11 1993 KJB - Ported to ANSI C ISIS system
*        Oct 28 1994 KJB - Updated for System 5 support
*	 Mar 10 1995 Trent Hare - changed to isistypes
*        Sep 08 1995 Kris Becker - changed argument types from CHAR to
*                                  void
*_End
************************************************************************/
{

  if (ibuf != obuf) {
    (void) memmove((void *) obuf, (void *) ibuf, (size_t) nbytes);
  }
  return;
}
