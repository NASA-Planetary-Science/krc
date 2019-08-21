/************************************************************************
*_Title isissys.h Define system specific symbols and definitions
*_Descr
*/

#ifndef ISISSYS_H
#define ISISSYS_H

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

#define IBM_SYSTEM                   1
#define IEEE_SYSTEM                  2
#define LSB_SYSTEM                   3
#define MAC_SYSTEM                   4
#define MSB_SYSTEM                   5
#define PC_SYSTEM                    6
#define SUN_SYSTEM                   7
#define VAX_SYSTEM                   8
#define VAXG_SYSTEM                  9
#define DGUX_SYSTEM                 10
#define HP_SYSTEM                   11
#define SGI_SYSTEM                  12
#define DECSTN_SYSTEM               13
#define ALPHA_SYSTEM                14

#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif
/*
*_Hist  Apr 15 1993 Kris Becker - USGS Flagstaff Original Version
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*_End
***********************************************************************/
