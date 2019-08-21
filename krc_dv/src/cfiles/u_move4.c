#include <stdio.h>
#include <string.h>
#include "isisdef.h"
#include "isistypes.h"
#include "u.h"


void u_move4(INT4 nlongs, INT4 ibuf[], INT4 obuf[])
/***********************************************************************
*_Title	u_move4 Transfers longs from one buffer to another (very fast)
*_Args	Type	Variable	I/O	Description
*_Parm  INT4    nlongs;          I      Number of longs to transfer
*_Parm  INT4  	ibuf[];          I      Input buffer
*_Parm  INT4  	obuf[];          O      Output buffer
*
*_Desc	Transfers the contents of the source buffer (IBUF) to the
*	destination buffer (OBUF) for the number of longs specified
*	by NLONGS.   If NLONGS <= 0, then this routine returns without
*	any action.  This routine also handles overlapping input and
*	output buffers.  See also u_move1 and u_move4.
*
*_Hist   Aug 13 1990 Kris Becker USGS, Flagstaff Original Macro Version
*	 July 16, 1991 Steve Wampler USGS, Flagstaff rewritten in C
*        Jan 11 1993 KJB - Ported to ANSI C ISIS system
*	 Mar 10 1995 Trent Hare - changed to isistypes
*_End
***********************************************************************/
{

  if (ibuf != obuf) {
    (void) memmove((void *) obuf, (void *) ibuf, (size_t) nlongs*sizeof(INT4));
  }
  return;

}
