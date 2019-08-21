#include <stdio.h>
#include <string.h>
#include "isistypes.h"
#include "isisdef.h"
#include "isis_limits.h"
#include "isislogio.h"
#include "u.h"

#define MAX_LINE_LENGTH 132

extern BOOLEAN current_term_on; /* maintained in uu_report_stream */

INT4 u_write_term (CHAR msg[])
/***********************************************************************
*		---------------------------------
*		|    U _ W R I T E _ T E R M    |
*		---------------------------------
*_TITLE	u_write_term Write message to controlling terminal
*
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_PARM  CHAR       msg[];        I	The message to be output - maximum
*					  length of each line is 132 
*                                         characters
*
*_DESC	This routine will output the given text string to the controlling
*	terminal.  Trailing spaces in the MSG string will not be output.
*       If any line is longer than 132 characters, all characters after 
*       132 will be truncated.  Each line may be separated by a newline ('\n')
*       in "C" and a line feed (char(10)) in "FORTRAN".  Also note that all
*       carriage return characters are ignored.
*
*	NOTE: this routine assumes that the proper connection to the
*	controlling terminal has been made.  Normally, this will be done
*	by a call to the u_std_init routine.
*
*_KEYS	TERMINAL
*
*_HIST	Aug 21 1989 Jim Torson, USGS, Flagstaff - Original Build 2 specs
*       Nov 01 1989 Kris Becker, USGS, Flagstaff Original Version
*	Jul 18 1991 KJB - Ported to UNIX system
*       May 30 1993 Tracie Sucharski - Converted to C for UNIX port
*       Aug 29 1994 Kris Becker - Reworked for new user interface
*       Dec 29 1994 KJB - Added carriage return to end a line of text
*       Feb 14 1995 Trent Hare - added ISIS_LOG_REPORT check and ISIS types
*       Apr 04 1996 Jim Mathews, USGS, Flagstaff Changed Boolean to BOOLEAN
*       Nov 04 1999 Kris Becker - GNU libc (Linux libc6) has forced us to
*                                 protect ourselves against a NULL file
*                                 pointer.  See isislogio.h for details.
*_END
****************************************************************************/
{
  INT4     i;
  INT4     n;
  
  BOOLEAN force_line;

  CHAR    tbuf[MAX_LINE_LENGTH+10];

/**********************************************************************
  If external variable current_term is ON continue, return if OFF
  vairiable originated from uu_report_stream 
**********************************************************************/
/*  if (current_term_on == FALSE )
     return(0); */

/**********************************************************************
  Check to ensure that tlun0 is defined.  If not define it here.
***********************************************************************/
/*  if (tlun0 == (FILE *) NULL) tlun0 = stdout; */

/**********************************************************************
*  Parse throught the message one character at a time
**********************************************************************/
  if ((strlen(msg)) > 0) {
   for (i = 0, n = 0, force_line = TRUE; msg[i] != EOS; i++) {
    
    switch (msg[i]) {

/********************************************************************
*  Ignore carriage returns 
*********************************************************************/
      case CARRIAGE_RETURN:
        break;
      
/*********************************************************************
*  A line terminator has been encountered-write out accumulated line 
**********************************************************************/
        case NEWLINE:
        tbuf[n++] = CARRIAGE_RETURN;
        tbuf[n++] = NEWLINE;
	tbuf[n++] = EOS;
/*	(void) fprintf(tlun0, "%s", tbuf); */

	n = 0;
        force_line = FALSE;
        break;

/*********************************************************************
*  Add character if it does not exceed the line limit
**********************************************************************/
      default:
	if (n < MAX_LINE_LENGTH) tbuf[n++] = msg[i];
	force_line = TRUE;
	break;
    }
  }

  if (force_line == TRUE) {
    tbuf[n++] = CARRIAGE_RETURN;
    tbuf[n++] = NEWLINE;
  }
  tbuf[n] = EOS;

/***********************************************************
  Write the string the the terminal
 ***********************************************************/
/*    (void) fprintf(tlun0,"%s", tbuf); */
  }
  else
/*    (void) fprintf(tlun0,"\r\n"); */


  return (0);

}

