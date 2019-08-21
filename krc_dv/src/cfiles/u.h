/* Prototypes for the U routines */

#ifndef U_H
#define U_H
#include <stdio.h>
#include "isisdef.h"
#include "isistypes.h"

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */


extern void u_actime (INT4 opt, INT4 *ret);
extern INT4 u_actinf(INT4 *con_tsec, INT4 *con_tms, INT4 *cpu_tsec, 
			INT4 *cpu_tms, INT4 *dir_io, INT4 *pg_flts, 
			INT4 *proc_swaps, INT4 *ret);
extern INT4 u_error (CHAR errkey[], CHAR errmes[], INT4 errval,
			INT4 action);
extern void u_free(void *mbuf);
extern void u_move1 (INT4 nbytes, void *ibuf, void *obuf);
extern void u_move2 (INT4 shorts, INT2 ibuf[], INT2 obuf[]);
extern void u_move4 (INT4 nints, INT4 ibuf[], INT4 obuf[]);
extern INT4 u_open_log(FILE **logptr, INT4 *ret);
extern INT4 u_set_erract (INT4 stream, INT4 action);
extern void u_swap2(INT4 nwords, void *ibuf, void *obuf);
extern void u_swap4(INT4 nlongs, void *ibuf, void *obuf);
extern INT4 u_new_id();
extern INT4 ui_error (CHAR errkey[], CHAR errmes[], INT4 errval, INT4 action);
extern INT4 u_write_msg (INT4 dest, CHAR msg[]);
extern INT4 u_write_term (CHAR msg[]);


#ifdef __cplusplus
}
#endif  /*  __cplusplus  */
#endif

/*
*_Hist 		Kris Becker, USGS, Flagstaff - Original version
*	Nov 21, 1994 - Jim Torson - Changed to use ISIS types (INT2, INT4,
*		FLOAT4, FLOAT8, CHAR, etc.)  
*	Dec 13, 1994 - JMT - added u_sat_minmax1 & u_sat_minmax2
*       Dec 19, 1994 Kris Becker - Added u_file_exist prototype
*	Feb 11, 1995 JMT - added u_pix_cvf
*	Feb 22, 1995 Trent Hare - added u_check_env, u_find_substring,
*		u_set_current_stream, u_set_report_stream, u_report_parm
*	Mar 16, 1995 Trent Hare - changed all void [] to void *
*       Jul 31 1995 Kris Becker - Added u_swap2 and u_swap4 prototypes
*       Nov 21 1995 KJB - Added u_compstat prototype
*       Jan 26 1996 KJB - added ret parameter to u_fourn prototype
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++;
*                         changed references of Boolean to BOOLEAN
*       Mar 21 1996 KJB - Added u_mvbits prototype
*       Apr 19 1996 KJB - Added u_range1 prototype
*       Apr 26 1996 KJB - Changed occurances of "inline" to "iline" as this
*                         conflicts with C++.
*       May 06 1996 Kris Becker - added prototype for u_bitm
*       Jul 09 1996 Jeff Anderson - added prototypes for u_roi routines
*       Jul 19 1996 Jim Mathews - added prototypes for u_roi and u_inq 
*                                 routines.
*	Oct 11 1996 JMT - Added u_specpr_open, u_specpr_close, u_specpr_rentry,
*			and u_specpr_rnext
*       Oct 24 1996 Jim Mathews - added prototype for u_set_erract 
*       Jan 07 1997 Tracie Sucharski - added prototype for u_set_nologfile
*       Feb 17 1997 James M Anderson - added u_system
*       Nov 26 1996 Kris Becker - Added u_new_id, u_get_dbl_parm,
*                                 u_record_dbl_parm, u_put_dbl_key and
*                                  u_get_objdef prototypes
*       Dec 03 1996 KJB - Added u_decompose_path, u_find_file_delimiter,
*                         and u_compose_path prototypes
*       Jan 14 1996 KJB - Added u_define_object prototype
*       Jul 27 1998 Kris Becker - Added u_nchr2d, u_nchr2i, u_nchr2r and
*                                 u_next_token prototypes
*       Jul 27 1998 Kris Becker - Added u_token_buffer prototype
*       Mar 05 1999 Kris Becker - Added u_set_dbl_parm prototype
*       Dec 01 2000 Kris Becker - Added u_vectorize_image prototype
*       Jan 03 2001 Kris Becker - Added u_get_line prototype
*	Feb 21 2001 Jim Torson - Added u_roi_setline
*       Nov 13 2001 Kris Becker - Added u_get_geomsys prototype
*       May 05 2005 Kris Becker - Added u_isub_exist prototype
*_Ver   $Id: u.h,v 1.2 2005/05/05 19:27:11 kbecker Exp $
*_End
*/
