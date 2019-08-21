/************************************************************************
*_TITLE	ISISLOGIO Definitions and symbols for session logging and terminal I/O
*
*_DESC	This file contains the logical names of the I/O devices associated
*	with session logging and the logical unit numbers associated with 
*       them.  
*      
*       INIT_IO must be defined only once and should be defined in the
*       initialization routine.  This would normally be done in the 
*       " u_std_init" routine.
*/

#ifndef ISISLOGIO_H
#define ISISLOGIO_H
#include <stdio.h>
#ifndef INIT_IO

extern CHAR   sysout[];             /* System output environment name */
extern CHAR   isislog[];            /* Session log file environment name */
extern CHAR   sessiondef[];         /* Default session logging file */
extern FILE   *tlun0;               /* File pointer for terminal */
extern FILE   *llun0;               /* File pointer for session log */

#else

CHAR     sysout[10] = {"/dev/tty"};
CHAR     isislog[14] = {"ISIS_LOG_FILE"};
CHAR     sessiondef[256] = {"print.prt"};
FILE     *tlun0 = {(FILE *) NULL};
FILE     *llun0 = {(FILE *) NULL};

#endif
#endif
/*	
*
*_KEYS  SESSION_LOG, UNIX
*
*_HIST	Jul 18 1990 Kris Becker USGS, Flagstaff Original Version
*	Jul 18 1991 KJB - Ported to UNIX system
*       Jul 01 1992 KJB - Added LOG_FILE to COMMON
*       May 31 1993 Tracie Stoewe - Converted to C
*	Mar 13 Trent Hare - changed to isistypes
*       Aug 15 1996 Kris Becker - Changed ISISLOG_FILE to ISIS_LOG_FILE
*                                 to be consistant with other ISIS
*                                 environment variables
*       Nov 04 1999 Kris Becker - GNU libc (Linux libc6) does not allow
*                                 initialization of standard file pointers
*                                 stdout, stdin, etc..., so tlun0 is
*                                 initialized to NULL.  It is defined
*                                 in u_std_init as the first thing anyway.
*_END
**************************************************************************/
