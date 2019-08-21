#include <stdio.h>
#include <string.h>
#include "isistypes.h"
#include "isisdef.h"
#include "isislogio.h"
#include "u.h"

INT4 u_write_msg (INT4 dest, CHAR msg[])
/************************************************************************
*
*		-------------------------------
*		|    U _ W R I T E _ M S G    |
*		-------------------------------
*
*_TITLE	U_WRITE_MSG Write message to terminal and/or session log file
*
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_PARM  INT4       dest;	 I	Destination for the message:
*       				  1 - controlling terminal
*       				  2 - ISIS session log file
*       				  3 - both terminal and session log
*_PARM	CHAR       msg[];        I	The message to be output - maximum
*					  length is 132 characters
*
*_DESC	This routine will output the given text string to the controlling
*	terminal and/or the ISIS session log file.  Trailing spaces in the
*	MSG string will not be output.  If any line is longer than 132
*       characters, all characters after 132 will be truncated.
*       Each line may be separated by a newline ('\n') in "C" and a
*       line feed (char(10)) in "FORTRAN".  Also note that all carriage
*       return characters are ignored.
*
*	NOTE: this routine assumes that the session log file has been
*	opened and the proper connection to the controlling terminal has
*	been made.  Normally, these things will be done by a call to the
*	u_std_init routine.
*
*_KEYS	SESSION_LOG
*
*_HIST	Aug 21 1989 Jim Torson, USGS, Flagstaff - Original Build 2 specs
*       Oct 03 1989 Kris Becker USGS, Flagstaff Original Version
*	Jul 18 1991 KJB - Ported to UNIX system
*       Jun 01 1993 Tracie Sucharski - Converted to C
*       Aug 29 1994 Kris Becker - Reworked for new user interface
*	Mar 13 1995 Trent Hare - changed to isistypes
*_END
****************************************************************************/
{

/************************************************************
*  Determine the output destination and call the appropriate
*  routines
************************************************************/
   if (dest == 1 || dest == 3)  (void) u_write_term(msg);
/*   if (dest == 2 || dest == 3)  (void) u_write_log(msg); */
   return(0);
}
