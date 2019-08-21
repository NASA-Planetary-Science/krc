/**************************************************************************
*_Title binf5.h - Prototypes for binf5 stuff
*_Args NONE
*/

#ifndef BINF5_H
#define BINF5_H
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

extern void binf5(char action[], char filename[], char header[],
                  int header_size, int id[], unsigned char data[], int *ret);
extern void u_move1(int nbytes, void* ibuf, void* obuf);
extern void u_move4(int nlongs, int ibuf[], int obuf[]);
extern void u_swapn(int nelems, int esize, void *ibuf, void *obuf);

#ifdef __cplusplus
}
#endif  /*  __cplusplus  */
#endif

/*
*_Hist Mar 24 2000 Kris Becker, USGS, Flagstaff Original Version
*_End
***************************************************************************/
