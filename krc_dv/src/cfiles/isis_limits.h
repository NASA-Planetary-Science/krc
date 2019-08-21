/***********************************************************************
*_TITLE	ISIS_LIMITS Definition of current ISIS system limits
*_VARS	SYMBOL  	VALUE      DESCRIPTION                         */

#ifndef ISIS_LIMITS_H
#define ISIS_LIMITS_H

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

#define MAX_FILES 20		/* Maximum number open files */

#define	MAX_AXES  	   6	/* Maximum number of supported qube axes */
#define MAX_SUFFIX_AXIS    3    /* Maximum axis allowed to have suffixes */

#define	MAX_SAMPS       1024	/* Maximum number of cube file samples */
#define MAX_LINES       1024	/* Maximum number of cube file lines   */
#define MAX_BANDS       1024	/* Maximum number of cube file bands   */
#define MAX_CDIM        1024	/* Maximum dimensions of core axes */

#define MAX_SIDEPLANES	 128	/* Maximum number of cube sideplanes  */
#define MAX_BOTTOMPLANES 128	/* Maximum number of cube bottomplanes */
#define MAX_BACKPLANES	 512	/* Maximum number of cube backplanes   */
#define MAX_SDIM         512	/* Maximum number of suffixes allowed */

#define MAX_CACHE        20	/* Maximum number of i/o caches allowed */
				/* Index of 0 to MAX_CACHE-1 on q_use_cache */

#define MAX_HIST_SIZE   10240    /* Maximum size of history entry buffer*/

/* Expected ISIS SFDU value */
#define SFDU_LABEL   "CCSD3ZF0000100000001NJPL3IF0PDS200000001"

/* Default numbers for LABEL records */
#define DEFAULT_LABEL_RECORDS   30
#define EXTRA_LABEL_RECORDS     15

/* Default numbers for HISTORY records */
#define DEFAULT_HISTORY_RECORDS 50
#define EXTRA_HISTORY_RECORDS   25

/* Maximum number objects, groups and keywords */
#define MAX_OBJECTS               20		
#define MAX_GROUPS                20
#define MAX_BAND_BIN_KEYWORDS     50

/************************************************************************
  ROW and COLUMN file limits and ASCII line limits
*************************************************************************/
#define MAX_COLUMNS              300
#define R_ASCII_LINE_MIN          40
#define R_ASCII_LINE_MAX        1024

#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif

/*
*_DESC	This INCLUDE file defines PARAMETER values that provide
*	symbolic names for various ISIS system limits.  Many of these
*	can be modified to "tailor" your own ISIS system.  Note that
*	a FORTRAN equivalent .INC include file is supplied for FORTRAN
*	programmers.  Any change that is made to this file MUST also
*	be changed in the ISIS_LIMITS.INC file!!!

*_KEYS  DOCUMENTATION

*_HIST	Mar 21 1990 Kris Becker, USGS, Flagstaff - Original C version
* 	Oct  2 1989 Jim Torson, USGS, Flagstaff - Original version
*	Aug 04 1990 KJB - Added MAX_CDIM, MAX_SDIM parameter
*       Mar 19 1992 KJB - Added MAX_COLUMNS
*       Apr 08 1992 KJB - Added R_ASCII_LINE_MIN and R_ASCII_LINE_MAX
*	Jul 12 1991 KJB - Modified to support UNIX system
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*	May 21 1996 Jeff Anderson - Added multi-cache support
*       Jul 17 2003 Janet Barrett - Increased MAX_CACHE to 20
*_END
************************************************************************/
