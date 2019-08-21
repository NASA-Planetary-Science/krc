/*********************************************************************
*_Title uidef.h Include file for user interface system

*_Desc  Declares user interface prototypes.
*_Keys  INCLUDE
*_Hist  Aug 31 1994 Kris Becker USGS, Flagstaff Original Version
*       Mar 10 1995 Trent Hare - changed to isistypes
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*       Nov 26 1996 Kris Becker - added new ui_getdblprm and ui_setdblprm
*                                 prototypes
*       Jul 29 1998 KJB - Added new ui_record_args prototype
*_End
*********************************************************************/
#ifndef UIDEF_H
#define UIDEF_H
#include <stdio.h>
#include <string.h>
#include "isistypes.h"

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

extern FILE *ui_init (CHAR pgmnam[], INT4 *ret);
extern INT4 ui_getfileprm(CHAR name[], INT4 maxcount, CHAR val[], INT4 val_len, 
                         INT4 length[], INT4 *count);
extern INT4 ui_getintprm(CHAR name[], INT4 maxcount, INT4 val[], INT4 *count);
extern INT4 ui_getrealprm (CHAR name[], INT4 maxcount, FLOAT4 val[],
                           INT4 *count);
extern INT4 ui_getdblprm (CHAR name[], INT4 maxcount, FLOAT8 val[], INT4 *count);
extern INT4 ui_getstrprm(CHAR name[], INT4 maxcount, CHAR val[], INT4 val_len, 
                        INT4 length[], INT4 *count);
extern INT4 ui_record_args(int argc, char *argv[], INT4 lang);
extern INT4 ui_setintprm(CHAR name[], INT4 val[], INT4 count);
extern INT4 ui_setrealprm(CHAR name[], FLOAT4 val[], INT4 count);
extern INT4 ui_setdblprm(CHAR name[], FLOAT8 val[], INT4 count);
extern INT4 ui_setstrprm(CHAR name[], CHAR val[], INT4 val_len, INT4 count);
extern INT4 ui_error (CHAR errkey[], CHAR errmes[], INT4 errval, INT4 action);
extern INT4 ui_exit (INT4 status);

#ifdef __cplusplus
}
#endif  /*  __cplusplus  */
#endif
