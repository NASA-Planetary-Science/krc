#include <stdio.h>
#include "isisdef.h"
#include "isistypes.h"
#include "u.h"


INT4 u_new_id(void)
/********************************************************************
*_Title u_new_id Returns a unique identifier
*_Args  Type	Name	   I/O   Description
*_Parm  INT4    u_new_id;   O    A unique identifier

*_Desc  u_new_id will return unique identifiers that can be used
*       for reference to elements within the system.

*_Keys  MEMORY

*_Hist  Nov 12 1996 Kris Becker USGS Flagstaff Original Version
*_End
***********************************************************************/
{
  static INT4 new_ids = 1000;
  return (new_ids++);
}
