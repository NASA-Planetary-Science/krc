/************************************************************************
*_Title isisarch.h Determines compilation for system architecures
*_Args NONE
*/

#ifndef ISISARCH_H
#define ISISARCH_H
#include "isissys.h"

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */

/* The following two #defines select which operating system to compile	*/
/* for.  They are automatically set by predefined preprocessor symbols.	*/

#ifdef vms
#define UNIX_OS	0
#define VMS_OS	1
#else
#define UNIX_OS	1
#define VMS_OS	0
#endif

/* The following #defines select the architecture of the system you are	*/
/* on, i.e. a subdivision of the UNIX_OS and VMS_OS defines.  These are	*/
/* used almost exclusively to set the various feature #defines at the	*/
/* bottom of this file.  Do not use a _ARCH directly in an #if in	*/
/* program code.  Rather, abstract the thing you want into a feature	*/
/* #define, set below, that is based on the architecture.  This makes	*/
/* porting to new machines much easier.  The _ARCH symbols are set	*/
/* automatically by predefined preprocessor symbols.			*/

#define ALPHA_ARCH	0
#define VAX_ARCH	0
#define SUN3_ARCH	0
#define SUN4_ARCH	0
#define SUN_ARCH        0
#define ALLIANT_ARCH	0
#define DGUX_ARCH       0
#define DECSTATION_ARCH	0
#define CRAY_ARCH	0
#define MAC_ARCH        0
#define MAC_AUX_ARCH	0
#define MAC_MPW_ARCH	0
#define MAC_DARWIN_ARCH 0
#define SGI_ARCH	0
#define TEK_ARCH	0
#define HP_ARCH         0
#define HP700_ARCH	0
#define LINUX_IX86_ARCH 0 
#define LINUX_PPC_ARCH  0 

#ifdef __alpha 
#undef	ALPHA_ARCH
#define	ALPHA_ARCH	1
#endif

#ifdef vms
#undef	VAX_ARCH
#define	VAX_ARCH	1
#endif

#ifdef ultrix
#undef	DECSTATION_ARCH
#define	DECSTATION_ARCH	1
#endif

#ifdef sun
#undef  SUN_ARCH
#define SUN_ARCH        1
#ifdef sparc
#undef	SUN4_ARCH
#define	SUN4_ARCH	1
#else
#undef	SUN3_ARCH
#define	SUN3_ARCH	1
#endif
#endif

#ifdef _DGUX_
#undef  DGUX_ARCH
#define DGUX_ARCH       1
#endif

#ifdef alliant
#undef	ALLIANT_ARCH
#define	ALLIANT_ARCH	1
#endif

#ifdef CRAY
#undef	CRAY_ARCH
#define	CRAY_ARCH	1
#endif

#ifdef _AUX_SOURCE
#undef  MAC_ARCH
#define MAC_ARCH       1
#undef	MAC_AUX_ARCH
#define	MAC_AUX_ARCH	1
#endif

#ifdef applec
#undef  MAC_ARCH
#define MAC_ARCH       1
#undef	MAC_MPW_ARCH
#define MAC_MPW_ARCH	1
#endif

#if defined(__APPLE__)
/* Mac OS X using Apple's gcc derivative */
#undef  MAC_ARCH
#define MAC_ARCH       1
#undef	MAC_DARWIN_ARCH
#define MAC_DARWIN_ARCH	1
#endif

#ifdef sgi
#undef	SGI_ARCH
#define	SGI_ARCH	1
#endif

#if defined(m88k) && defined(ghs)	/* need better conditions! */
#undef	TEK_ARCH
#define	TEK_ARCH	1
#endif

/* On the 700 series, the 700 symbol is defined in the _cc_ command,	*/
/* *NOT* the _cpp_ command!!!  This means imake won't find the proper	*/
/* predefined symbol.  So, for now, use the 800 series symbol (which is	*/
/* defined in cpp on the 700) as well.					*/
#if defined(__hp9000s700) || defined(__hp9000s800)	/* Fix this!! */
#undef HP_ARCH
#define HP_ARCH         1
#undef HP700_ARCH
#define HP700_ARCH	1
#endif


/* The Linux Operating System is available on many different hardware   */
/* types.  Find out which one we are using from the supported types.    */
#if defined(Linux) || defined(__linux__)
#ifndef USE_STANDARD_F77
#define USE_STANDARD_F77 1
#endif
#if defined(__i386__) || defined(x86_64)
#undef LINUX_IX86_ARCH
#define LINUX_IX86_ARCH 1
#endif
#ifdef __PPC__
#undef LINUX_PPC_ARCH
#define LINUX_PPC_ARCH 1
#endif
#endif

/* Error check */

#if ALPHA_ARCH+VAX_ARCH+SUN3_ARCH+SUN4_ARCH+ALLIANT_ARCH+DECSTATION_ARCH+CRAY_ARCH+MAC_AUX_ARCH+MAC_MPW_ARCH+MAC_DARWIN_ARCH+SGI_ARCH+TEK_ARCH+HP700_ARCH+DGUX_ARCH+LINUX_IX86_ARCH+LINUX_PPC_ARCH != 1
/* ANSI C Error reporting */
/*#error "*_ARCH either not defined or not defined correctly!!!!!!!!!!!"*/
Compile Error: "*_ARCH either not defined or not defined correctly!!!!!!!!!!!"
#endif

/* The following #defines select optional pieces of the RTL.  1 means	*/
/* use the piece, 0 means do not use it.				*/
/* The TAE major version (4 or 5) should be set based on what is	*/
/* available on your platform.  It is needed only by builds (imake etc).*/

#define RTL_USE_TAE	1	/* use TAE subroutine package */
#if HP700_ARCH+SUN4_ARCH+DGUX_ARCH+ALPHA_ARCH+LINUX_IX86_ARCH+LINUX_PPC_ARCH+MAC_DARWIN_ARCH
#define TAE_VERSION	5
#else
#define TAE_VERSION	4
#endif


/************************************************************************/
/* The following macros do NOT need to be redefined when compiling on a	*/
/* different system.  Changing the macros above will control what is	*/
/* selected below.							*/
/************************************************************************/

/************************************************************************/
/* Data-type defines.  The following sets up the defaults for the data	*/
/* type system label items.						*/
/* NOTE:  If you change or add an entry, make sure you update the table	*/
/* in xvhost.c!								*/
/************************************************************************/

/************************************************************************/
/* Please redefine NATIVE'S as they are learned for each machine        */
/************************************************************************/

#if ALPHA_ARCH
#define NATIVE_SYSTEM              ALPHA_SYSTEM
#define NATIVE_HOST_LABEL	   "ALPHA/OSF1"
#define NATIVE_INTFMT		   "LSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if VAX_ARCH
#define NATIVE_SYSTEM              VAX_SYSTEM
#define NATIVE_HOST_LABEL	   "VAX-VMS"
#define NATIVE_INTFMT		   "LSB"
#define NATIVE_REALFMT		   "VAX"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if SUN3_ARCH
#define NATIVE_SYSTEM              SUN_SYSTEM
#define NATIVE_HOST_LABEL	   "SUN-3"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if SUN4_ARCH
#define NATIVE_SYSTEM              SUN_SYSTEM
#define NATIVE_HOST_LABEL	   "SUN-4"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if ALLIANT_ARCH
#define NATIVE_SYSTEM              SUN_SYSTEM
#define NATIVE_HOST_LABEL	   "ALLIANT"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if DECSTATION_ARCH
#define NATIVE_SYSTEM              DECSTN_SYSTEM
#define NATIVE_HOST_LABEL	   "DECSTATN"
#define NATIVE_INTFMT		   "LSB"
#define NATIVE_REALFMT		   "RIEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if CRAY_ARCH
#define NATIVE_SYSTEM              CRAY_SYSTEM
#define NATIVE_HOST_LABEL	   "CRAY"
#define NATIVE_INTFMT		   "???"
#define NATIVE_REALFMT		   "???"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if MAC_AUX_ARCH
#define NATIVE_SYSTEM              MAC_SYSTEM
#define NATIVE_HOST_LABEL	   "MAC-AUX"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if MAC_MPW_ARCH
#define NATIVE_SYSTEM              MAC_SYSTEM
#define NATIVE_HOST_LABEL	   "MAC-MPW"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if MAC_DARWIN_ARCH
#define NATIVE_SYSTEM              MAC_SYSTEM
#define NATIVE_HOST_LABEL          "MAC-DARWIN"
#define NATIVE_INTFMT              "MSB"
#define NATIVE_REALFMT             "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short 
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short 
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if SGI_ARCH
#define NATIVE_SYSTEM              SGI_SYSTEM
#define NATIVE_HOST_LABEL	   "SGI"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if TEK_ARCH
#define NATIVE_SYSTEM              TEK_SYSTEM
#define NATIVE_HOST_LABEL	   "TEK"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if HP700_ARCH
#define NATIVE_SYSTEM              HP_SYSTEM
#define NATIVE_HOST_LABEL	   "HP-700"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if DGUX_ARCH
#define NATIVE_SYSTEM              DGUX_SYSTEM
#define NATIVE_HOST_LABEL	   "DG/UX"
#define NATIVE_INTFMT		   "MSB"
#define NATIVE_REALFMT		   "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif

#if LINUX_IX86_ARCH
#define NATIVE_SYSTEM              ALPHA_SYSTEM
#define NATIVE_HOST_LABEL          "LINUX-ix86"
#define NATIVE_INTFMT              "LSB"
#define NATIVE_REALFMT             "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif


#if LINUX_PPC_ARCH
#define NATIVE_SYSTEM              MAC_SYSTEM
#define NATIVE_HOST_LABEL          "LINUX-ppc"
#define NATIVE_INTFMT              "MSB"
#define NATIVE_REALFMT             "IEEE"
#define NATIVE_BYTE                char
#define NATIVE_UNSIGNED_BYTE       unsigned char
#define NATIVE_2BYTE_INT           short
#define NATIVE_4BYTE_INT           int
#define NATIVE_2BYTE_UNSIGNED_INT  unsigned short
#define NATIVE_4BYTE_UNSIGNED_INT  unsigned int
#define NATIVE_4BYTE_FLOAT         float
#define NATIVE_8BYTE_FLOAT         double
#endif




/************************************************************************/
/* OS-related defines							*/
/************************************************************************/

/* Set up mechanisms for handling global variables.  The difference is	*/
/* needed because under VMS, all "globaldef" variables go in a single	*/
/* psect, while all "extern" variables go in individual psects.  Since	*/
/* the data psects must be included in the linker options file in order	*/
/* for the shareable image to be linked, it is easier to deal with one	*/
/* psect than dozens of them.						*/

#if UNIX_OS

#define PUBLICDEF
#define PUBLICREF extern

#endif /* UNIX_OS */

#if VMS_OS

#define PUBLICDEF globaldef
#define PUBLICREF globalref

#endif /* VMS_OS */

/************************************************************************/
/* Architechture-related defines					*/
/************************************************************************/

/* If the actual number of arguments are available, set NARGS_AVAIL_OS	*/
/* to 1 and define the macro "va_count(nargs)" to set the integer nargs.*/
/* The varargs environment is assumed to be set up already.		*/

#if VAX_ARCH + ALPHA_ARCH
#define NARGS_AVAIL_OS 1
/* va_count is system-defined */
#endif

#if SUN3_ARCH + SUN4_ARCH + DECSTATION_ARCH + CRAY_ARCH + SGI_ARCH + TEK_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH
#define NARGS_AVAIL_OS 0
#endif

#if MAC_AUX_ARCH + MAC_MPW_ARCH + HP700_ARCH
#define NARGS_AVAIL_OS 0
#endif

#if ALLIANT_ARCH
#define NARGS_AVAIL_OS 1
#define va_count(nargs) nargs = *(((short int *)(&va_alist))-1)
#endif

/************************************************************************/
/* If fstat() returns the optimal blocksize, set FSTAT_BLKSIZE_OS to 1.	*/
/* If not, set it to 0.							*/

#if SUN3_ARCH + SUN4_ARCH + DECSTATION_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH
#define FSTAT_BLKSIZE_OS 1
#endif

#if VAX_ARCH + CRAY_ARCH + MAC_MPW_ARCH + SGI_ARCH + TEK_ARCH + HP700_ARCH
#define FSTAT_BLKSIZE_OS 0
#endif

/************************************************************************/
/* If fstat() is even available, set FSTAT_AVAIL_OS to 1.  If not,	*/
/* set it to 0.  Note that if fstat() is not available, FSTAT_BLKSIZE_OS*/
/* should also be set to 0.						*/

#if VAX_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if SUN3_ARCH + SUN4_ARCH + DECSTATION_ARCH + ALLIANT_ARCH + CRAY_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH + MAC_DARWIN_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if SGI_ARCH + TEK_ARCH + HP700_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if MAC_AUX_ARCH
#define FSTAT_AVAIL_OS 1
#endif

#if MAC_MPW_ARCH
#define FSTAT_AVAIL_OS 0
#endif

/************************************************************************/
/* If mmap() is available (for array I/O), then set MMAP_AVAIL_OS to 1.	*/
/* If not, set it to 0.							*/

#if VAX_ARCH
#define MMAP_AVAIL_OS	0		/* doesn't matter for VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + SGI_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH + MAC_DARWIN_ARCH
#define MMAP_AVAIL_OS	1
#endif

#if CRAY_ARCH
#define MMAP_AVAIL_OS	???		/* check on this */
#endif

#if ALLIANT_ARCH + MAC_MPW_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + TEK_ARCH
#define MMAP_AVAIL_OS	0
#endif

#if HP700_ARCH
#define MMAP_AVAIL_OS	0
#endif

/************************************************************************/
/* If the on_exit() routine is available (sets up an exit handler),	*/
/* then set ON_EXIT_AVAIL_OS to 1.  If not, set it to 0.		*/

#if VAX_ARCH
#define ON_EXIT_AVAIL_OS	0	/* doesn't matter under VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH + MAC_DARWIN_ARCH
#define ON_EXIT_AVAIL_OS	1
#endif

#if DECSTATION_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + SGI_ARCH + TEK_ARCH + ALPHA_ARCH
#define ON_EXIT_AVAIL_OS	0
#endif

#if HP700_ARCH
#define ON_EXIT_AVAIL_OS	0
#endif

#if CRAY_ARCH
#define ON_EXIT_AVAIL_OS	???		/* check on this */
#endif

#if MAC_MPW_ARCH	/* actually exists as atexit() if needed */
#define ON_EXIT_AVAIL_OS	0		/*????*/
#endif

/************************************************************************/
/* Since there is little standardization on the definitions for the	*/
/* lseek() call, set up the include to use and define SEEK_SET and	*/
/* SEEK_END if they aren't already.					*/
/* Note:  If you include seek_include, you must include <sys/types.h>	*/
/* first for MAC_AUX_ARCH!						*/

#if VAX_ARCH
#define seek_include	<stdio.h>
#endif

#if SUN3_ARCH + SUN4_ARCH + DECSTATION_ARCH + SGI_ARCH + TEK_ARCH + HP700_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH
#define seek_include	<unistd.h>
#endif

#if ALLIANT_ARCH
#define seek_include	<sys/file.h>
#define SEEK_SET	L_SET
#define SEEK_END	L_XTND
#endif

#if CRAY_ARCH
#define seek_include	????
#endif

#if MAC_AUX_ARCH
#define seek_include	<stdio.h>
#endif

#if MAC_MPW_ARCH
#define seek_include	<fcntl.h>
#endif

/************************************************************************/
/* Some implementations of open() don't allow the Unix-style protection	*/
/* flags as the third argument.						*/

#if VAX_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if SUN3_ARCH + SUN4_ARCH + ALLIANT_ARCH + DECSTATION_ARCH + SGI_ARCH + TEK_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if CRAY_ARCH + MAC_AUX_ARCH + HP700_ARCH
#define OPEN_PROTECT_OS	1
#endif

#if MAC_MPW_ARCH
#define OPEN_PROTECT_OS	0
#endif

/************************************************************************/
/* Set FTRUNCATE_AVAIL_OS if the ftruncate() function is available.	*/
/* Note that it does not hurt if ftruncate() isn't there, files just	*/
/* won't be pre-allocated or truncated on close.			*/

#if VAX_ARCH
#define FTRUNCATE_AVAIL_OS	0
#endif

#if SUN3_ARCH + SUN4_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + SGI_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH + MAC_DARWIN_ARCH
#define FTRUNCATE_AVAIL_OS	1
#endif

#if HP700_ARCH
#define FTRUNCATE_AVAIL_OS	1
#endif

#if ALLIANT_ARCH + MAC_MPW_ARCH + TEK_ARCH
#define FTRUNCATE_AVAIL_OS	0
#endif

#if CRAY_ARCH
#define FTRUNCATE_AVAIL_OS	???
#endif

/************************************************************************/
/* Set HOSTMSG_UNIX_OS if the host error messages can be obtained via	*/
/* sys_nerr and sys_errlist[].  If not, set HOSTMSG_UNIX_OS to 0 and	*/
/* define HOSTMSG_FUNC_OS to be a function that returns a pointer to	*/
/* the error string given the error number as an argument.		*/

#if VAX_ARCH
#define HOSTMSG_UNIX_OS	0	/* HOSTMSG_FUNC_OS doesn't matter for VMS */
#endif

#if SUN3_ARCH + SUN4_ARCH + ALLIANT_ARCH + MAC_AUX_ARCH + DECSTATION_ARCH + ALPHA_ARCH + LINUX_IX86_ARCH + LINUX_PPC_ARCH
#define HOSTMSG_UNIX_OS	1
#endif

#if SGI_ARCH + TEK_ARCH + HP700_ARCH
#define HOSTMSG_UNIX_OS	1
#endif

#if MAC_MPW_ARCH
#define HOSTMSG_UNIX_OS	0
#define HOSTMSG_FUNC_OS	strerror
#endif

#if CRAY_ARCH
#define HOSTMSG_UNIX_OS	???
#endif

/***********************************************************************/
/*  Include strategies for use of the memory allocation routines,      */
/*  malloc() and free(), etc..., are not the same for all OSes. Ones   */
/*  we know about are added here with the default assumptions using    */
/*  "<malloc.h>.                                                       */
#if MAC_DARWIN_ARCH
#define malloc_include <stdlib.h>
#else
#define malloc_include <malloc.h>
#endif


/***********************************************************************/
/*  This is added to support 64-bit architecture.  Note that this may  */
/*  not be fully supported in its entirety throughout the ISIS system. */
/*  By this I mean that this indicates that at a minimum, pointers are */
/*  64 bits in size.  Support for files > 32 bit limits may or may not */
/*  be supported (at the time of this addition, 64 bit file addressing */
/*  was not supported). */
#if defined(HAS_64BIT_SUPPORT) || (__arch64__ == 1) || (ALPHA_ARCH == 1)
#define ISIS_64_BIT 1
#else
#define ISIS_64_BIT 0
#endif


#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif

/*
*_Keys  SYSTEM

*_Hist  Apr 15 1993 Kris Becker, USGS, Flagstaff Original Version
*       Nov 16 1993 Kris Becker - Added NATIVE_4BYTE_INT definition
*	Mar 16 1995 Kris Becker/Trent Hare - Added ALPHA_ARCH definitions
*       Mar 20 1996 KJB - Added elements so this file can be ingested by C++
*       Feb 15 1997 KJB/James M Anderson - Added LINUX_IX86_ARCH definitions
*       Nov 03 1999 Kris Becker - Added LINUX_PPC_ARCH definitions
*       Feb 19 2004 Kris Becker - Added MAC_DARWIN_ARCH definitions for 
*                                 MAC OS X; added include strategy for memory
*                                 allocation (malloc and free) since Darwin
*                                 does not have /usr/include/malloc.h; 
*                                 added support for 64 bit architectures
*       May 25 2005 Kris Becker - Reworked how the Linux architecture is 
*                                 determined.  Do not depend on user
*                                 defined preprocessor definitions
*_Ver  $Id: isisarch.h,v 1.2 2005/05/26 22:51:43 kbecker Exp $
*_End
*************************************************************************/
