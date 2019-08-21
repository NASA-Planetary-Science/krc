/* Prototypes for the H routines */

#ifndef H_H
#define H_H
#include "isistypes.h"
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

extern INT4 h_copy (INT4 from, INT4 to, INT4 indent, INT4 *ret);
extern INT4 h_disable(void);
extern INT4 h_enable(void);
extern void h_end_parm (void);
extern INT4 h_get_line (INT4 fileid, CHAR line[], INT4 llen, INT4 *ret);
extern INT4 h_indent (INT4 fid, INT4 *ret);
extern INT4 h_init_buf (CHAR progname[]);
extern INT4 h_init_object (INT4 fileid, INT4 iflag, INT4 *nbytes, INT4 *ret);
extern INT4 h_put (INT4 fileid, INT4 *ret);
extern INT4 h_reset_line (INT4 fileid, INT4 *ret);
extern INT4 h_size (INT4 fileid, INT4 *arecs, INT4 *obytes, INT4 *ret);
extern INT4 h_write_line(CHAR sbuf[], INT4 *ret);
extern INT4 h_dump_history (INT4 destin);


#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif
/*
*_Hist          Kris Becker, USGS, Flagstaff - Original version
*       Nov 21, 1994 - Jim Torson - Changed to use ISIS types (INT2, INT4,
*               FLOAT4, FLOAT8, CHAR, etc.)
*       Feb 21, 1995 - Trent Hare - Added h_dump_history
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*       Aug 29 2000 KJB - Added h_enable prototype
*_End
*/

