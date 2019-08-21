#include <stdio.h>
#include <stdlib.h>
#include "isistypes.h"
#include "binding.h"


void b_free(CHAR *mem)
/*********************************************************************
*_Title b_free Frees memory allocated by b_alloc
*_Nobinding
*_Args  Type  Name       I/O   Description
*_Parm  CHAR  *mem;       I    Address of allocated memory

*_Desc  b_free frees memory allocated by the b_alloc routine.

*_Keys  MEMORY

*_Hist  Apr 14 1993 Kris Becker, USGS, Flagstaff Original Version
*	Apr 05 1995 Trent Hare - changed to isistypes
*_End
**********************************************************************/
{

/****************************************************************
  Free the memory and return to caller
*****************************************************************/
  if (mem != NULL) (void) free((void *) mem);
  return;
}
