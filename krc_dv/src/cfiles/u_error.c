#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "isistypes.h"
#include "isisdef.h"
#include "isislogio.h"
#include "u.h"
/* #include "h.h" */
#include "uidef.h"

BOOLEAN term_ever_off = FALSE; /* Maintained in uu_report_stream */
BOOLEAN log_ever_off = FALSE;  /* Maintained in uu_report_stream */

static BOOLEAN log_error_reporting = TRUE;  /* Log error reporting */
static BOOLEAN term_error_reporting = TRUE; /* Terminal error reporting */

INT4 u_error (CHAR errkey[], CHAR errmes[], INT4 errval, INT4 action)
/*****************************************************************************
*_TITLE  U_ERROR  Error message handling facility
*
*_ARGS  TYPE      VARIABLE   I/O  DESCRIPTION
*_PARM  CHAR      errkey[];   I   Error key, 17 characters or less in length
*_PARM  CHAR      errmes[];   I   Error message
*_PARM  INT4      errval;     I   Error value
*_PARM  INT4      action;     I   Action code
*                                  ACTION=1, return after logging error message
*                                  ACTION=2, terminate program after logging 
*                                            error
*
*_DESC
*     U_ERROR is called when a program is to log an error message
*     and optionally terminate execution. The arguments passed to U_ERROR 
*     conform to the TAE message logging requirements. These arguments are 
*     described in detail below.
*
*                 Detailed description of Arguments
*                 ---------------------------------
*
*     ERRKEY - A 17 character or smaller string indicating the facility 
*     and message identification. It has the form "facility-msgid". 
*     "Facility" indicates the name of the program or subroutine issuing 
*     the error message. "Msgid" uniquely identifies the message within 
*     the facility. For example, if program XYZ could not open a file 
*     because it did not exisit, then ERRKEY might have the form: 
*     "XYZ-NOFILE".
*     
*     ERRMSG - This character string contains the error message to be 
*     issued by U_ERROR. For example, if a program could not find an
*     input file because it did not exist, as in the example above, then
*     ERRMSG might have the form: "Input cube file AVIRIS.CUB not found"
*     Together, with ERRKEY and ERRMSG, the complete error message would 
*     look like:  "XYZ-NOFILE Input cube file AVIRIS.CUB not found"
*     The ERRMSG string can be any length.  If the ERRMSG string will not
*     fit on a single output line, then U_ERROR will break the message into 
*     several lines.
*     
*     ERRVAL - INTEGER*4, This value is the unique number associated 
*     with the error. 
*
*     ACTION - INTEGER*4, This argument specifies the action to be taken
*     by U_ERROR. If ACTION=1, then U_ERROR will log the error message
*     and return to the calling routine.  If ACTION=2, then U_ERROR will
*     log the error message and terminate the program.
*
*_KEYS STRING, SESSION_LOG, TERMINAL
*
*_HIST 16-Nov-87 Eric Eliason, USGS, Internal documentation (specs)
*      29-May-88 Bob Mehlman  UCLA/IGPP  Dummy interim version: writes ERRKEY,
*	ERRMES and ERRVAL to SYS$OUTPUT, and STOPs or RETURNs acc. to ACTION.
*	Jul 31 1990 Kris Becker Updated documentation and code to adhere to 
*	                        Build 2 ISIS specifications; NOT all 
*			        functionality of this routine implemented
*				as of yet.  
*	Sep 16 1990 KJB - Added check in ERRKEY for length, modified
*	                  description for ACTION parameter
*	Jul 18 1991 KJB - Ported to UNIX system
*	Jul 22 1991 KJB - Fixed error in write for UNIX FORTRAN
*       Jun 03 1993 Tracie Sucharski - Converted to C for UNIX port
*       Mar 22 1994 KJB - Added TAE abnormal termination notification and exit
*       Aug 29 1994 KJB - Reworked for new user interface
*       Jan 24 1995 KJB - Fixed bug in call to ui_error and added prototype
*                         include file for user interface (uidef.h)
*       Feb 10 1995 Trent Hare - Added history dump checks for TERM and LOG
*                                  and ISIS types.
*       Apr 04 1996 Jim Mathews, USGS, Flagstaff Changed Boolean to BOOLEAN
*       Sep 05 1996 Jim Mathews, USGS, Flagstaff Added capability to turn
*                         error reporting on and off.
*       Apr 22 1997 Tracie Sucharski, Changed the way the parsing is
*                         done to handle long phrases with no spaces.
*
*_END
***************************************************************************/
{

  CHAR errbuf[512];
  CHAR tbuf[512];

  INT4 start;
  INT4 end;
  INT4 nchars;
  INT4 outcode;
  static BOOLEAN dumped = FALSE; /* Static variable for u_dump_history */

/**********************************************************************
  Check error reporting variables and set the appropriate output number 
**********************************************************************/
  outcode = 1;
  if (action == 2) { /* Fatal error occurred, report to log and term */
    outcode = 3;
  }
  else { /* action equals 1 */
    if (term_error_reporting == TRUE) {
      if (log_error_reporting == TRUE) {
	outcode = 3;
      }
      else { /* term reporting is TRUE and log reporting is FALSE */
	outcode = 1;
      }
    }
    else { /* term error reporting equals FALSE */
      if (log_error_reporting == TRUE) {
	outcode = 2;
      }
      else { /* both log and term error reporting equal FALSE - return */
	return(0);
      }
    }
  }

#if 0
/**********************************************************************
  Always turn on report streams 
**********************************************************************/
  (void) u_set_current_stream(ALL_STREAMS, REPORT_ON);

/**********************************************************************
  See if history has been dumped 
**********************************************************************/
  if (dumped == FALSE) {
     if (term_ever_off == TRUE)
        (void) h_dump_history(TERM_STREAM);
     if (log_ever_off == TRUE)
        (void)h_dump_history(LOG_STREAM);
     dumped = TRUE;
  }
#endif

/***********************************************************************
  Write error key, value and message to both terminal and log file
************************************************************************/
  (void) sprintf(tbuf," [%s] (%ld) %s",errkey,(long) errval,errmes);

  start = 0;
  end = 75;
  nchars = (INT4) strlen(tbuf);
  errbuf[0] = EOS;

  while (end < nchars) {
    while (tbuf[end] != SPACE && tbuf[end] != EOS && end != start)
      end--;
    if (end == start) {
      end = start+80;
      while (tbuf[end] != '/' && tbuf[end] != '-' && tbuf[end] != EOS)
        end--;

      if (end == start) end = start+80;
      (void) strcat(errbuf, &tbuf[start]);
      end++;
      start = end;
      errbuf[end++] = '\n';
      errbuf[end] = '\0';
      end = end + 80;
    }
    else {
      (void) strcat(errbuf, &tbuf[start]);
      errbuf[end++] = '\n';
      errbuf[end] = '\0';
      start = end;
      end = end + 80;
    }
  }

  (void) strcat(errbuf, &tbuf[start]);
  (void) u_write_msg(outcode, errbuf);


/* Allow user interface to deal with the error appropriately.  ui_error
   should abort if (action == 2) however, we will ensure a program exit */
  (void) ui_error(errkey,errmes,errval,action);
  if (action == 2) exit(errval);  /* Ensures a program exit */

/**********************************************************************
  Return streams to their defaults
**********************************************************************/
#if 0
  (void) u_set_current_stream(ALL_STREAMS, REPORT_DEFAULT);
#endif
  return(0);

}
 

INT4 u_set_erract(INT4 stream, INT4 action)
/*****************************************************************************
*_TITLE  U_SET_ERRACT  Set the error handling action
*
*_ARGS  TYPE      VARIABLE   I/O  DESCRIPTION
*_PARM  INT4      stream;     I   describes what stream to turn on.
*                                 choices:
*                                  1 (TERM_STREAM)
*                                  2 (LOG_STREAM)
*                                  3 (ALL_STREAMS) - both TERM and LOG
*_PARM  INT4      action;     I   do what with stream, choices:
*                                  1 (REPORT_ON)
*                                  2 (REPORT_DEFAULT) - progr's defaults
*                                  3 (REPORT_OFF) 
*
*_Parm INT4 u_set_erract;     O   Return values:
*                                0 - OK
*                               -1 - 1 (REPORT_ON) or 2 (REPORT_DEFAULT)
*                                    or (REPORT_OFF) for TERM_STREAM or 
*                                    ALL_STREAMS not sent
*                               -2 - 1 (REPORT_ON) or 2 (REPORT_DEFAULT)
*                                    or (REPORT_OFF) for TERM_STREAM or 
*                                    ALL_STREAMS not sent
*                               -3 - 3 (ALL_STREAMS), 1 (TERM_STREAM),
*                                    2 (LOG STREAM) not sent
*
*_DESC
*     U_SET_ERRACT will set the error handling options for the u_error 
      routine.  You can turn on or off reporting the error to the terminal,
      the log file, or both.
*
*     NOTE the defines for REPORT_ON, REPORT_DEFAULT, REPORT_OFF, 
*     ALL_STREAMS, TERM_STREAM, and LOG_STREAM are initialized in 
*     isisdef.inc and isisdef.h
*
*_KEYS STRING, SESSION_LOG, TERMINAL
*
*_HIST Sep 05 1996 Jim Mathews, USGS, Flagstaff - Original version
*      Feb 17 1997 James M Anderson - fixed sprintf format
*
*_END
***************************************************************************/
{
  static BOOLEAN default_term_state = TRUE;
  static BOOLEAN default_log_state = TRUE;

  BOOLEAN temp_term_state;  /* temporary terminal state */
  BOOLEAN temp_log_state;   /* temporary log state */
  CHAR errbuf[126];         /* error buffer */

  if ((stream == LOG_STREAM) || (stream == ALL_STREAMS)) { 

    switch (action) {

      case (REPORT_ON):
	default_log_state = log_error_reporting;
	log_error_reporting = TRUE;
	break;

      case (REPORT_DEFAULT):
	temp_log_state = log_error_reporting;
	log_error_reporting = TRUE;
	default_log_state = temp_log_state;
	break;

      case (REPORT_OFF):
	default_log_state = log_error_reporting;
	log_error_reporting = FALSE;
	break;

      default:
         (void) sprintf(errbuf,
      "Value %ld sent, needed 1 (REPORT_ON) or 2 (REPORT_DEFAULT) or 3 (REPORT_OFF)",
                        (long) action);
	 (void) u_error("USETERRACT-BADREP", errbuf, -1, 1);
	 return(-1);
    }
  }

  if ((stream == TERM_STREAM) || (stream == ALL_STREAMS)) { 

    switch (action) {

      case (REPORT_ON):
	default_term_state = term_error_reporting;
	term_error_reporting = TRUE;
	break;

      case (REPORT_DEFAULT):
	temp_term_state = term_error_reporting;
	term_error_reporting = TRUE;
	default_term_state = temp_term_state;
	break;

      case (REPORT_OFF):
	default_term_state = term_error_reporting;
	term_error_reporting = FALSE;
	break;

      default:
         (void) sprintf(errbuf,
      "Value %ld sent, needed 1 (REPORT_ON) or 2 (REPORT_DEFAULT) "
	 "or 3 (REPORT_OFF)", (long) action);
	 (void) u_error("USETERRACT-BADREP", errbuf, -2, 1);
	 return(-2);
    }
  }

/**************************************************************************
  IF TERM_STREAM, LOG_STREAM or ALL_STREAMS not sent return error
  **************************************************************************/
  if ((stream != TERM_STREAM) && (stream != LOG_STREAM) &&
                                 (stream != ALL_STREAMS)) {
    (void) sprintf(errbuf, 
      "Value %ld sent, needed 3 (ALL_STREAMS),1 (TERM_STREAM), 2(LOG_STREAM)",
	    (long) stream);
    (void) u_error("USETERACT-BADSTRM", errbuf, -3, 1);
    return(-3);
  }

  return(0);
   
}
