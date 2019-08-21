/********************************************************************
*_Title primio.h Prototype include file for I/O routines
*_Args NONE
*/

#ifndef PRIMIO_H
#define PRIMIO_H

#include "isisdef.h"
#include "isistypes.h"

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

extern INT4 pio_in(INT4 *fid, CHAR flspec[], INT4 *nbytes, INT4 mode,
                   INT4 *ret);
extern INT4 pio_cl(INT4 fid, INT4 idele, INT4 *ret);
extern INT4 pio_rd(INT4 fid, INT4 ibyte, INT4 nbytes, void *buf, INT4 *ret);
extern INT4 pio_wt(INT4 fid, INT4 ibyte, INT4 nbytes, void *buf, INT4 *ret);
extern INT4 pio_ap(INT4 fid, INT4 nbytes, INT4 *ntbytes, INT4 *ret);
extern INT4 pio_reset_access(INT4 fid, INT4 access, INT4 *ret);
extern INT4 pio_truncate(INT4 fid, INT4 nbytes, INT4 *ret);
#ifdef __cplusplus
}
#endif  /*  __cplusplus  */
#endif

/*
*_Hist Dec 02 1993 Kris Becker USGS, Flagstaff Original Version
*      Nov 04 1994 Trent Hare USGS, Flagstaff added pio_reop_rw 
*      Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*      Nov 25 1996 KJB - Added pio_truncate prototype
*_End
*********************************************************************/
