/************************************************************************
*_Title isistypes.h Establishes ISIS system-wide internal type definitions
*_Descr
*/

#ifndef ISISTYPES_H
#define ISISTYPES_H
#include "isisarch.h"
#include "isisdef.h"

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

/***************************************************************
  Define native variable types
****************************************************************/
typedef NATIVE_4BYTE_INT           BOOLEAN;
typedef NATIVE_BYTE                INT1;
typedef NATIVE_UNSIGNED_BYTE       UINT1;

typedef NATIVE_2BYTE_INT           INT2;
typedef NATIVE_4BYTE_INT           INT4;
typedef NATIVE_2BYTE_UNSIGNED_INT  UINT2;
typedef NATIVE_4BYTE_UNSIGNED_INT  UINT4;

typedef NATIVE_4BYTE_FLOAT         FLOAT4;
typedef NATIVE_8BYTE_FLOAT         FLOAT8;

typedef NATIVE_BYTE                CHAR;

/************************************************************
  Internal object structure
*************************************************************/
typedef struct isis_object {
  CHAR name[MAXOBJNAME+1];          /* Object name */
  INT4 status;                      /* Object status:
                                         ATTACHED_OBJECT
                                         DETACHED_OBJECT
                                         DATALESS_OBJECT */
  CHAR file[MAXFILNAME+1];          /* External object file name */
  INT4 io_mode;                     /* I/O mode for file
                                         0 - set up no access
                                         1 - READ_ONLY
                                         2 - READ_WRITE
                                         3 - CREATE_NEW
                                         4 - CREATE_TEMPORARY */
  INT4 nrecs;                       /* Number records in object */
  INT4 sbyte;                       /* Starting byte of data */
  INT4 nbytes;                      /* Number bytes in data object */
} ISIS_OBJECT;

#ifdef __cplusplus
}
#endif  /*  __cplusplus  */
#endif
/*
*_Hist  Nov 27 1992 Kris Becker - USGS Flagstaff Original Version
        Nov 17 1994 Trent Hare  - USGS Flagstaff, typedefs updated
                                  added all NATIVE defines
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++;
*                         changed type Boolean to BOOLEAN to avoid collision
*                         with X/Motif
*       Nov 25 1996 KJB - Added object definitions
*_End
***********************************************************************/
