#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#if !defined(RUSAGE_SELF) || (defined(Solaris) && !defined(__GNUC__))
#include <unistd.h>
#include <fcntl.h>
#include <sys/signal.h>
#include <sys/procfs.h>
#endif
#include "isisdef.h"
#include "isistypes.h"
#include "u.h"


INT4 u_actinf(INT4 *con_tsec, INT4 *con_tms, INT4 *cpu_tsec, INT4 *cpu_tms,
             INT4 *dir_io, INT4 *pg_flts, INT4 *proc_swaps, INT4 *ret)
/***********************************************************************
*_Title u_actinf Return resource useage information
*_Args  Type    Name         I/O        Description
*_Parm  INT4    *con_tsec;    O         Wall clock connect time in 
*                                       seconds
*_Parm  INT4    *con_tms;     O         Number of microseconds past the
*                                       second of connect seconds
*                                       (con_tsec)
*_Parm  INT4    *cpu_tsec;    O         Cpu usage in seconds
*_Parm  INT4    *cpu_tms;     O         Number of microseconds past the
*                                       second of cpu seconds (cpu_tsec)
*_Parm  INT4    *dir_io;      O         Number of direct I/Os the 
*                                       processes used
*_Parm  INT4    *pg_flts;     O         Number page faults the process
*                                       had
*_Parm  INT4    *proc_swaps;  O         Number of times the process
*                                       swapped in and out of memory
*_Parm  INT4    *ret;         O         Return code
*                                          0 - ok
*                                         -1 - could not get process id
*                                         -2 - could not get process info
        
*_Desc  U_ACTINF will collect and return information about the calling
*       process.  This does not include the child processes of the 
*       calling process.

*_Hist  Aug 09 1991 Kris Becker, USGS, Flagstaff Original C Version
*       Jul 24 1992 Kris Becker - Modified computation of microseconds
*       Aug 11 1993 KJB - Ported to ANSI compliant ISIS system
*       Aug 25 1994 KJB - Added system 5 implementation
*	Feb 28 1995 Trent Hare - changed to isistypes
*       Jun 09 1997 Kris Becker - Fixed conditional for resource query
*                                 that SUN's Solaris upgrade from5.4 to 5.5
*                                 broke
*       Dec 08 2003 KJB - Port to Solaris 9 using GNU compilers requires
*                         no compilation of previous Solaris specific
*                         junk namely unistd.h, fcntl.h, etc...
*_End
************************************************************************/
{
  CHAR errbuf[256];                         /* Error message buffer */

/*****************************************************
  This implementation is for system 5 POSIX support
******************************************************/
#ifdef PIOCUSAGE
  INT4 fid;
  CHAR proc[80];
  prusage_t  process;
  time_t     connect;

/* Open the process file */
  (void) sprintf(proc,"/proc/%ld", (long) getpid());
  if ((fid = open(proc, O_RDONLY)) == -1) {
    (void) sprintf(errbuf,"Error getting process id");
    (void) u_error("UACTINF-PIDERR", errbuf, -1, 1);
    *ret = -1;
    return (-1);
  }

/* Get the process information */
  if (ioctl(fid, PIOCUSAGE, &process) == -1) {
    (void) sprintf(errbuf,"Error getting process resource information");
    (void) u_error("UACTINF-RSCERR", errbuf, -2, 1);
    *ret = -2;
    return (-2);
  }

  (void) close(fid);

/* Extract the requested information */
  *con_tsec = (INT4) process.pr_tstamp.tv_sec;
  *con_tms  = process.pr_tstamp.tv_nsec / 1000;

  *cpu_tsec = (INT4) (process.pr_utime.tv_sec + process.pr_stime.tv_sec); 
  *cpu_tms  = (process.pr_utime.tv_nsec + process.pr_stime.tv_nsec) / 1000; 

  *dir_io   = (INT4) (process.pr_inblk + process.pr_oublk);
  *pg_flts  = (INT4) (process.pr_minf + process.pr_majf);
  *proc_swaps = (INT4) process.pr_nswap;

/*******************************************************
  This implementation is for BSD-based systems
********************************************************/
#else
  struct rusage process;      /* Resource information structure */
  struct timeval connect;     /* Connect time structure */


  (void) getrusage(RUSAGE_SELF, &process);
  (void) gettimeofday(&connect, NULL);

   
  *con_tsec = connect.tv_sec;
  *con_tms  = connect.tv_usec;

  *cpu_tsec = process.ru_utime.tv_sec + process.ru_stime.tv_sec;
  *cpu_tms = process.ru_utime.tv_usec + process.ru_stime.tv_usec;

  *dir_io = process.ru_inblock + process.ru_oublock;
  *pg_flts = process.ru_majflt;
  *proc_swaps = process.ru_nswap;
#endif

  *ret = 0;
  return (0);
} 
