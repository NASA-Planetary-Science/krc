/************************************************************************
*_Title isisdef.h Establishes definitions and structures for ISIS system
*_Descr
*/

#ifndef ISISDEF_H
#define ISISDEF_H

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

#if defined(TRUE)
#undef TRUE
#endif
#define TRUE                      1

#if defined(FALSE)
#undef FALSE
#endif
#define FALSE                     0


/* Expected ISIS SFDU value */
#define SFDU_LABEL   "CCSD3ZF0000100000001NJPL3IF0PDS200000001"

#if defined(ABS)
#undef ABS
#endif
#define ABS(A)   (((A) < 0.0) ? -(A) : (A))

#if defined(MIN)
#undef MIN
#endif
#define MIN(A, B)    (((A) < (B)) ? (A) : (B))

#if defined(MAX)
#undef MAX
#endif
#define MAX(A, B)    (((A) > (B)) ? (A) : (B))

#if defined(UPPER)
#undef UPPER
#endif
#define UPPER(C)     (((C) >= 'a' && (C) <= 'z') ? (C) - 'a' + 'A' : (C))

#if defined(LOWER)
#undef LOWER
#endif
#define LOWER(C)     (((C) >= 'A' && (C) <= 'Z') ? (C) - 'A' + 'a' : (C))

#if defined(ADJNEG)
#undef ADJNEG
#endif
#define ADJNEG(X)    (((X) <= 0.0) ? ((X) - 1.0) : (X))

#define FREE_MEMORY               1
#define RETAIN_MEMORY             2

#define UNDEFINED                 0
#define READ_ONLY                 1
#define READ_WRITE                2
#define CREATE_NEW                3
#define CREATE_TEMPORARY          4
#define CREATE_DETACHED           5

#define PIO_READ_ONLY             0
#define PIO_READ_WRITE            1
#define PIO_CREATE_NEW            2
#define PIO_CREATE_TEMPORARY      4
#define PIO_CREATE_NEW_OVRWRT     5

#define PIO_SAVE                  0
#define PIO_DELETE                1

#if defined(READ) 
#undef READ
#endif
#define READ                      1

#if defined(WRITE) 
#undef WRITE
#endif
#define WRITE                     2

#define KEEP                      0
#define DELETE                    1

#define KEEP_FILE                 1
#define DELETE_FILE               2
#define FLUSH_KEEP                3

#define MAXFILNAME              128
#define MAXKEYNAME               40
#define MAXGRPNAME               32
#define MAXOBJNAME               32
#define MAXLITLEN                32

#define MAX_LABEL_LINE_LENGTH    80
#define MAX_HISTORY_LINE_LENGTH  75

#define FILE_RECORD_BYTES       512
#define SUFFIX_ITEM_BYTES         4

#define DEFAULT_REAL_FMT          2
#define DEFAULT_REAL_DIGITS       7

#if defined(SPACE)
#undef SPACE
#endif
#define SPACE                    ' '

#if defined(EOS)
#undef EOS
#endif
#define EOS                     '\0'

#if defined(TAB)
#undef TAB
#endif
#define TAB                     '\t'

#if defined(CARRIAGE_RETURN)
#undef CARRIAGE_RETURN
#endif
#define CARRIAGE_RETURN         '\r'

#if defined(NEWLINE)
#undef NEWLINE
#endif
#define NEWLINE                 '\n'

#define  LOGICAL_ORDER            1
#define  PHYSICAL_ORDER           2

#define  BSQ_STORAGE              1
#define  BIL_STORAGE              2
#define  BIP_STORAGE              3

/*** Start of STREAM defines *********/
#define  TERM_STREAM              1
#define  LOG_STREAM               2
#define  ALL_STREAMS              3

#define  REPORT_ON                1
#define  REPORT_DEFAULT           2
#define  REPORT_OFF               3
/*** End of STREAM defines *********/

/* Object definitions */
#define ATTACHED_OBJECT    1      /* Specifies attached data object */
#define DETACHED_OBJECT    2      /* Specifies detached data object */
#define DATALESS_OBJECT    3      /* Specifies dataless object */


/******************************************************
  Define language tags 
*******************************************************/
#define UNKNOWN_LANG          0
#define C_LANG                1
#define FORTRAN_LANG          2

#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif
/*
*_Hist  Nov 27 1992 Kris Becker - USGS Flagstaff Original Version
*       Feb 21 1995 Trent Hare, USGS, Flagstaff - Added stream section
*       Jul 12 1995 Kris Becker - Added PIO_CREATE_NEW_OVRWRT macro
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*       Nov 22 1996 KJB - Added CREATE_DETACHED definition
*       Jul 29 1998 KJB - Added language tags
*_End
***********************************************************************/
