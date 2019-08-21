#include <stdio.h>
#include <stdlib.h>
#include "isistypes.h"
#include "binding.h"


CHAR *b_alloc(INT4 nelem, INT4 esize)
/*********************************************************************
*_Title b_alloc Allocate memory for elements of a specified size
*_Nobinding
*_Args  Type  Name       I/O   Description
*_Parm  INT4  nelem;      I    Number of elements to allocate memory
*                              For
*_Parm  INT4  esize;      I    Size of each element in bytes
*_Parm  CHAR  *b_alloc;   O    Address of allocated memory

*_Desc  b_alloc returns the address of enough allocated memory to 
*       store nelem elements of esize bytes per element.  If this 
*       routine cannot allocate memory, a NULL pointer is returned.
*       b_alloc does not initialize the memory.

*_Keys  MEMORY

*_Hist  Apr 14 1993 Kris Becker, USGS, Flagstaff Original Version
*	Apr 5 1995 Trent Hare - changed to isistypes
*_End
**********************************************************************/
{

/****************************************************************
  Allocate the memory
*****************************************************************/
  return ((CHAR *) malloc(B_MAX(B_MINIMUM_MEMORY, (nelem * esize))));
}
