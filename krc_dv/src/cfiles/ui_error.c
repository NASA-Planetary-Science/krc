#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "u.h"

INT4 ui_error (CHAR errkey[], CHAR errmes[], INT4 errval, INT4 action)
/*****************************************************************************
*_Title  ui_error  Error message handling facility
*
*_ARGS  TYPE      VARIABLE   I/O  DESCRIPTION
*_PARM  CHAR      errkey[];   I   Error key, 17 characters or less in length
*_PARM  CHAR      errmes[];   I   Error message
*_PARM  INT4      errval;     I   Error value
*_PARM  INT4      action;     I   Action code
*                                  ACTION=1, return after logging error message
*                                  ACTION=2, terminate program after logging 
*                                            error
*_PARM  INT4      ui_error;   O   Returns 0 - if successful
*
*_DESC ui_error is the TAE interface for generating error messages to TAE.
*      Since TAE passes back to the environment a pointer to terminal,
*      all we need to do here is terminate when an ABORT action is
*      sensed.

*_Hist Oct 28 1994 Kris Becker, USGS, Flagstaff Original Version
*      Apr 07 1995 Trent Hare - changed to isistypes
*      Jul 19 1995 Trent Hare - fixed some documentation
*      Feb 16 1997 James M Anderson - added declarations for TAE routines
*      Feb 25 1997 James M Anderson - changed to use TAE types
*      Jul 28 1998 Kris Becker - Added SHELL ISIS support
*_END
***************************************************************************/
{

/* Abnormally terminate with error code */
  if (action == 2) {
      (void) fprintf(stdout,
       "[ISIS-ABTERM] Abnormal termination forced by application program\n");
      exit(errval);
  }

  return(0);
}
 
