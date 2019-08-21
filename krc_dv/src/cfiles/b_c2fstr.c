#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "isistypes.h"
#include "binding.h"


CHAR *b_c2fstr(INT4 nstr, CHAR cstr[], INT4 istrlen, CHAR fstr[],
               INT4 ostrlen, INT4 keep_pad)
/*********************************************************************
*_Title b_c2fstr Reformat C strings to FORTRAN strings
*_Nobinding
*_Args  Type  Name                 I/O  Description
*_Parm  INT4  nstr;                 I   Number of C strings to copy
*_Parm  CHAR  cstr[nstr][istrlen];  I   Array of C strings.  If this
*                                       is a NULL pointer then no 
*                                       conversion of strings is
*                                       performed.
*_Parm  INT4  istrlen;              I   Length of each C string
*                                       in the "cstr" array
*_Parm  CHAR  fstr[nstr][ostrlen];  O   Address of FORTRAN string array
*                                       where to copy strings to.  If 
*                                       NULL, then "nstr" strings each
*                                       with length "ostrlen" will be
*                                       allocated to accomodate the
*                                       C strings
*_Parm  INT4  ostrlen;              I   Length of each reformatted
*                                       FORTRAN type string
*_Parm  INT4  keep_pad;             I   Specifies FORTRAN padding.
*                                       (Included for consistancy - 
*                                        not used at present)
*_Parm  CHAR  *b_c2fstr;            O   Address of memory region that
*                                       contains the reformated FORTRAN
*                                       strings.  If cstr == NULL, then 
*                                       the strings are initialized to 
*                                       the pad character


*_Desc  b_c2fstr will reformat and (if requested) copy C strings to
*       FORTRAN strings.  This routine is multi-purpose in that it may
*       or may not allocate memory or reformat C strings to FORTRAN strings.

*       If cstr == NULL, then no C strings are copied to the
*       output array, fstr.  If fstr == NULL on input, the memory
*       is allocated to store "nstr" strings each of length "ostrlen".

*       The pointer returned by this routine will be either to the
*       "fstr" array (if fstr != NULL) or to the newly allocated
*       memory region allocated by this routine (if fstr == NULL).
*       If cstr == NULL, then each FORTRAN string will be padded with
*       the FORTRAN pad character to length ostrlen.

*_Keys  MEMORY

*_Hist  Apr 14 1993 Kris Becker, USGS, Flagstaff Original Version
*       Oct 11 1994 KJB - Added check for maximum FORTRAN string size;
*                         also changed exit to abort to get core dump
*       Oct 12 1994 KJB - Added insurance for string lengths being less
*                         zero
*	Apr 05 1995 Trent Hare - changed to isistypes
*_End
**********************************************************************/
{
  register CHAR *mem;         /* Memory pointer */
  register CHAR *fp, *cp;     /* FORTRAN and C string pointers, respectively */
  register INT4 i, j;         /* Loop counters */
  register INT4 len;          /* String lengths */

  CHAR errbuf[256];           /* Error buffer */

/*****************************************************************
  Check for invalid string size
******************************************************************/
  if (istrlen > (B_MAXIMUM_SIZE+1) || ostrlen > B_MAXIMUM_SIZE) {
    (void) sprintf(errbuf,"%%BC2FSTR-INVSIZ - invalid string length (%ld, %ld)",
                   (long) istrlen, (long) ostrlen);
    errno = EFAULT;
    (void) perror(errbuf);
    abort();
  }

/****************************************************************
  Determine if we need to allocate memory.  If no strings are
  to be allocated, return a NULL pointer.
*****************************************************************/
  nstr = B_MAX(1, nstr);
  ostrlen = B_MAX(1, ostrlen);
  istrlen = B_MAX(0, istrlen);
  mem = (fstr == NULL) ? (char *) b_alloc(nstr, ostrlen) : fstr;
  if (mem == NULL) {
    (void) sprintf(errbuf,
                 "%%BC2FSTR-MEMFAIL - cannot allocate %ld strings, length %ld",
                   (long) nstr, (long) ostrlen);
    (void) perror(errbuf);
    abort();
  }

/***************************************************************
  If the C string array is given, reformat the strings
  otherwise, initialize the output memory section
****************************************************************/
  *mem = B_FORTRAN_PAD_CHAR;              /* A little insurance */
  len = B_MIN(istrlen, ostrlen);
  if (cstr != NULL) {
    for (cp=cstr, fp=mem, i=0 ; i < nstr ; i++, cp+=istrlen, fp+=ostrlen) { 
      for (j = 0 ; (cp[j] != B_EOS) && (j < len) ; j++) fp[j] = cp[j];
      for ( ; j < ostrlen ; j++) fp[j] = B_FORTRAN_PAD_CHAR;
    }
  }
  else {
/************************************************************************
  C string array is not provided - initialize the output FORTRAN string
*************************************************************************/
    for (fp=mem, i=0 ; i < nstr ; i++) { 
      for (j = 0 ; j < len ; j++) *fp++ = B_FORTRAN_PAD_CHAR;
    }
  }

/*******************************************************************
  All done...return the address to caller
********************************************************************/
  return (mem);
}
