#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "isistypes.h"
#include "binding.h"


CHAR *b_f2cstr(INT4 nstr, CHAR fstr[], INT4 istrlen, INT4 keep_pad,
               CHAR cstr[], INT4 ostrlen)
/*********************************************************************
*_Title b_f2cstr Reformat FORTRAN strings to C strings
*_Nobinding
*_Args  Type  Name                 I/O  Description
*_Parm  INT4  nstr;                 I   Number of FORTRAN strings to
*                                       copy
*_Parm  CHAR  fstr[nstr][istrlen];  I   Array of FORTRAN strings.  If
*                                       this is a NULL pointer then
*                                       no conversion of strings is
*                                       performed.
*_Parm  INT4  istrlen;              I   Length of each FORTRAN string
*                                       in the "fstr" array
*_Parm  INT4  keep_pad;             I   Specifies to maintain FORTRAN
*                                       pad character or to discard
*                                       pad character before NULL C
*                                       string termination:
*                                           1 - keep pad characters
*                                           2 - discard pad characters
*_Parm  CHAR  cstr[nstr][ostrlen];  O   Address of C string array where
*                                       to copy strings to.  If NULL,
*                                       then "nstr" strings each with
*                                       length "ostrlen" will be
*                                       allocated to accomodate the
*                                       C strings
*_Parm  INT4  ostrlen;              I   Length of each reformatted C
*                                       type string including the extra
*                                       NULL character required to 
*                                       terminate each string
*_Parm  CHAR  *b_f2cstr;            O   Address of memory region that
*                                       contains the reformated C strings.
*                                       If fstr == NULL, then no strings
*                                       are contained in the memory region
*                                       and each C string in the output
*                                       array is set to length 0.


*_Desc  b_f2cstr will reformat and (if requested) copy FORTRAN strings
*       to C strings.  This routine is multi-purpose in that it may or
*       may not allocate memory or reformat FORTRAN strings to C strings.

*       If fstr == NULL, then no FORTRAN strings are copied to the
*       output array, cstr.  If cstr == NULL on input, the memory
*       is allocated to store "nstr" strings each of length "ostrlen".

*       The pointer returned by this routine will be either to the
*       "cstr" array (if cstr != NULL) or to the newly allocated
*       memory region allocated by this routine (if cstr == NULL).
*       If fstr == NULL, the each C string will be set to length
*       0.  If keep_pad == 1, the each C string is padded to the
*       istrlen length prior to C string termination.

*_Keys  MEMORY

*_Hist  Apr 14 1993 Kris Becker, USGS, Flagstaff Original Version
*       Oct 11 1994 KJB - Added check for maximum FORTRAN string size;
*                         also changed exit to abort to get core dump
*       Oct 12 1994 KJB - Added insurance for string lengths being less
*                         zero
*	Apr 05 1995 Trent Hare - changed to isistypes
*	May 12 1995 Trent Hare - included string.h
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
  if (istrlen > B_MAXIMUM_SIZE) {
    (void) sprintf(errbuf, "%%BF2CSTR-INVSIZ - invalid string length (%ld)",
                   (long) istrlen);
    errno = EFAULT;
    (void) perror(errbuf);
    abort();
  }

/****************************************************************
  Determine if we need to allocate memory.  If no strings are
  ro be allocated, return NULL pointer.
*****************************************************************/
  istrlen = B_MAX(0, istrlen);
  ostrlen = B_MAX(1, ostrlen);
  if (nstr <= 0) {
    return (NULL);
  }
  else {
    mem = (cstr == NULL) ? (char *) b_alloc(nstr, ostrlen) : cstr;
    if (mem == NULL) {
      (void) sprintf(errbuf,
                 "%%BF2CSTR-MEMFAIL - cannot allocate %ld strings, length %ld",
                    (long) nstr, (long) ostrlen);
      (void) perror(errbuf);
      abort();
    }
  }

/***************************************************************
  If the FORTRAN string array is given, reformat the strings
  otherwise, initialize the output memory section
****************************************************************/
  *mem = B_EOS;              /* A little insurance */
  len = B_MAX(0, B_MIN(istrlen, (ostrlen-1)));
  if (fstr != NULL) {

/* If requested to keep padding, use this loop */
    if (keep_pad == 1) {
      for (fp=fstr, cp=mem, i=0 ; i < nstr ; i++, fp+=istrlen, cp+=ostrlen) { 
        (void) strncpy(cp, fp, len);
        cp[len] = B_EOS;
      }
    }
    else {

/* Copy without string padding retained */
      for (fp=fstr, cp=mem, i=0 ; i < nstr ; i++, fp+=istrlen, cp+=ostrlen) { 
        for (j = len ; j > 0 ; j--) if (fp[j-1] != B_FORTRAN_PAD_CHAR) break;
        (void) strncpy(cp, fp, j);
        cp[j] = B_EOS;
      }
    }
  }
  else {
/************************************************************************
  FORTRAN string array is not provided - initialize the output C string
*************************************************************************/
    if (keep_pad == 1) {  /* Pad with FORTRAN pad character */
      for (cp=mem, i=0 ; i < nstr ; i++, cp+=ostrlen) { 
        for (j = 0 ; j < len ; j++) cp[j] = B_FORTRAN_PAD_CHAR;
        cp[len] = B_EOS;
      }
    }
    else {

/* Initialize without string padding retained */
      for (cp=mem, i=0 ; i < nstr ; i++, cp+=ostrlen) cp[0] = B_EOS;
    }
  }

/*******************************************************************
  All done...return the address to caller
********************************************************************/
  return (mem);
}

