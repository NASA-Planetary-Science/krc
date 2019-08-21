/******************************************************************
*_Title binding.h Binding include file
*_Args NONE

*_Desc  This include file should be included in all binding routines
*       as it defines and declares some symbols and constants needed
*       to generate bindings
*/

#ifndef BINDING_H
#define BINDING_H
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */


#define CC_NAME(a)                                   a
#define FTN_NAME(a)                               a##_
#define FTN_COMMON(a)                             a##_


#define B_MAXIMUM_SIZE                           65535
#define B_MINIMUM_MEMORY                            80
#define B_STRING_MAX(slen) ((((long) slen) < 0) ? 0 : ((long) slen))

#define B_MAX(x, y)          (((x) > (y)) ? (x) : (y))
#define B_MIN(x, y)          (((x) < (y)) ? (x) : (y))

#define B_EOS                                     '\0'
#define B_FORTRAN_PAD_CHAR                         ' '

#define B_RETAIN_PAD                                 1
#define B_DISCARD_PAD                                2

#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif

/*
*_Keys   INCLUDE

*_Hist  Apr 14 1993 Kris Becker, USGS, Flagstaff Original Version
*       Oct 11 1994 KJB - Added B_MAXIMUM_SIZE, the largest size of a
*                         string element
*       Dec 20 1994 KJB - Modified B_STRING_MAX to return 0 as minimum
*                         size of string rather than 1
*       Jul 29 1995 KJB - Added complete function prototypes
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*       Mar 20 1997 KJB - Added FTN_COMMON macro
*_End
********************************************************************/
