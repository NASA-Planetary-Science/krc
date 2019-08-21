/* Unix system include files */
#include	<stdio.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include        <fcntl.h>
#include        <unistd.h>
#include        <stdlib.h>
#include        <errno.h>
#include        "primio.h"
#include	"isisdef.h"
#include        "isistypes.h"
#include        "u.h"

#if defined(USE_OLD_ERRNO)
/* Errno external variables */
#if (defined(NEED_SYSERR_DEFINED) || defined(sun))
extern int sys_nerr;
extern char *sys_errlist[];
#endif
#endif

#define UNLINK_AVAIL_OS
#if defined(UNLINK_AVAIL_OS) && defined(VAX)
#define unlink     delete
#endif

/**********************************************************************
  This is the basic structure that contains information about each file
  opened by the primio file package.
***********************************************************************/
typedef struct pio_fcb {
   INT4 fid;			/* File descriptor */
   INT4 access;			/* Access mode of file */
   INT4 mode;			/* I/O mode for the file */
   INT4 file_handle;		/* Handle for file */
   CHAR filnam[256];		/* Name of file */
   INT4 nbytes;			/* Number of bytes in file */
   INT4 tbytes;			/* Total bytes in file */
   INT4 disposition;		/* Status of file */
  } PRIMIO_FCB;


/* Some constants */
#define		FCB_LEN	  			512
#define         MAX_FILES  	 		 20
#define         NO_FCB          (PRIMIO_FCB *) NULL
#define         TRUE              		  1
#define         FALSE             		  0
#define         SAVE              		  0
#define         KEEP              		  0
#define         DELETE            		  1
#define         CREATE            		  0
#define         PRIMIO_READ_WRITE         	  1
#define         PRIMIO_READ_ONLY         	  3
#define         TEMPORARY         		  4


/* These variables are the control blocks for PRIMIO package */
static PRIMIO_FCB *file_fcbs[MAX_FILES];	/* Pointers to file blocks */
static INT4 fcb_init = FALSE;		/* Specifies initialization of arrays */
#if defined(NO_ISIS)
  INT4 new_fids = 1000;
#define u_new_id()      new_fids++
#endif

#if defined(vms)
#define NULL_FILE      "NLA0:"
#else
#define NULL_FILE      "/dev/zero"
#endif


/* Declare internal functions */
static PRIMIO_FCB *pio_get_fcb(INT4 fileid);
static INT4 pio_error(CHAR routine[], CHAR errmsg[], INT4 errnum, 
                      INT4 syserrno);



INT4 pio_in(INT4 *fid, CHAR flspec[], INT4 *nbytes, INT4 mode, INT4 *ret)
/****************************************************************************
*_Title pio_in Create a new file or open an existing file
*_Args   Type  Name        I/O      Description
*_Param  INT4  *fid;        O       File identifier of the created or opened
*                                   file.
*_Param  CHAR  flspec[];    I       Name of the file to create or open.
*_Param  INT4  *nbytes;     B       For access mode CREATE or TEMPORARY, nbytes
*                                   specifies the size of the file in bytes.
*                                   For access mode READ_ONLY or READ_WRITE,
*                                   nbytes will return the total number of
*                                   bytes in the file.
*_Param  INT4  mode;        I       Specifies the access mode for the file.
*                                      0 - Open an existing file READ_ONLY
*                                      1 - Open an existing file READ_WRITE
*                                      2 - CREATE a new file for read/write
*                                      3 - Open an existing file READ_WRITE
*                                          if it exists, otherwise CREATE a
*                                          new file.
*                                      4 - Create a TEMPORARY file for
*                                          read/write access.
*                                      5 - CREATE a new file for read/write
*                                          allowing open of existing file
*                                      6 - create READ_WRITE access to a
*                                          null file
*_Param  INT4   *ret;       O       Return code:
*					 0 - ok
*					-1 - invalid access requested
*					-2 - file open/create failed

*_Desc  PIO_IN will open an existing file or create a new file for access
*       by the PRIMIO low level routines.  This routine needs to be 
*       called before any PRIMIO routines can perform I/O operations.

*       The fid is an integer value used internally by these routines 
*       to uniquely associate each file to an internal structure known
*       as the FCB (File Control Block) which holds descriptive
*       information about the opened file.  For the PRIMIO routines,
*       the information is meant to be for internal use only.  PIO_IN
*       creates the FCB dynamically and associates it with the file
*       'flspec'.  pio_in will completely initialize the FCB.  Subsequent
*       references to a file by other PRIMIO routines will be via the fid.

*       Mode == 6 option will create access to a null file.  This option
*       will emulate file access and will allow reading and writing to
*       a null device.  For this option, the caller must supply the size
*       of the psuedo file.  Read operations will return 0 filled buffers.
*       Write operations will result in the data being thrown away.  Note
*       that this implies that write operations are not preserved and 
*       subsequent reads of data that has been written will always return
*       0s in the buffer.  Note that the name of the file may/should
*       be specified by the caller but has no significance when using
*       this access mode.

*_KEYS  FILE_I/O

*_Hist  Oct 10 1992 Kris Becker USGS, Flagstaff Original C Version
*       Mar 23 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Mar 10 1995 KJB - Conditionalized creation of 512 byte
*                         block-bound size files
*       Jul 12 1995 KJB - Added open option that will allow file overwrite
*                         for creation mode.
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 22 1996 KJB - Use ISIS id routine; ensure error number is
*                         preserved
*       Nov 27 1996 KJB - Added access to null file
*       Oct 21 1999 KJB - Conditionalized the declaration of sys_errlist
*                         and sys_nerr in header portion of this file
*                         since it caused problems with glibc2.  Also
*                         correct a macro that refines the u_new_id
*                         as internal usage..was misspelled as u_new_fid.
*       Oct 27 1999 KJB - Sun compilers don't declare sys_errlist and
*                         sys_nerr so had to add it to the conditional
*       Dec 01 2003 KJB - Modified how system error strings are resolved.
*                          Now uses strerror as opposed to direct use of
*                          sys_nerr and sys_errlist.  Definition of
*                          USE_OLD_ERRNO macro will employ old implementation
*_End
**************************************************************************/
{
  register PRIMIO_FCB  *fb;
  INT4 perr;                   
  register INT4 nblocks;
  INT4 tbytes;
  INT4 file_pos;		/* Position of file pointer */
  INT4 file_no;

  register INT4 i;
  INT4 omode, flags, exclusive_open;		
  CHAR *open_mode;

#if ( FSTAT_AVAIL_OS == 1 )
  struct stat sb;
#endif

  INT4 status;
  CHAR errbuf[256];

/***********************************************************************
  Determine if the fcb array has been initialized - if not, initial it
************************************************************************/
  if (fcb_init == FALSE) {
     for (i = 0 ; i < MAX_FILES ; i++) 
       file_fcbs[i] = NO_FCB;
     fcb_init = TRUE;
  }

/***********************************************************************
  If the file is to be created, calculate the total number of blocks
  to allocate to the file.  Create the open mode flags for the proper
  open call.
************************************************************************/
  exclusive_open = 0;
  switch (mode) {

/* Set up to access a file PRIMIO_READ_ONLY */
          case 0:
                  flags = O_RDONLY;
                  omode = 0;
                  open_mode = "open (PRIMIO_READ_ONLY)";
                  break;
 
/* Set up for PRIMIO_READ_WRITE access */
          case 1:
          case 3:
                  flags = O_RDWR;
                  omode = 0;
                  open_mode = "open (PRIMIO_READ_WRITE)";
                  break;

/* Create a new file for read/write access */
          case 2:
          case 4: /* This will result in failure if the file exists */
                   exclusive_open = O_EXCL;
          case 5:
                  flags = O_RDWR | O_CREAT | exclusive_open;
                  omode = 0664;
                  nblocks = (*nbytes + 511) / 512;
#if defined(BLOCK_BOUND_OS)
                  tbytes = nblocks * 512;
#else
                  tbytes = *nbytes;
#endif
                  open_mode = "create";
                  break;

/* Generate a null file */
          case 6:
                  flags = O_RDWR;
                  nblocks = (*nbytes + 511) / 512;
#if defined(BLOCK_BOUND_OS)
                  tbytes = nblocks * 512;
#else
                  tbytes = *nbytes;
#endif
                  open_mode = "create";
                  break;

/* Invalid access option specified */
          default:
                  (void) sprintf(errbuf,
                                  "Invalid file access mode requested: %ld",
                                 (long) mode);
                  (void) pio_error("PIOIN-INVACC", errbuf, -1, 0);
                  *ret = -1;
                  return (-1);
                  break;
  }


/*********************************************************************
  Open the file with the requested access mode
**********************************************************************/        
  errno = 0;
  if (mode == 6) file_no = open( NULL_FILE, flags, omode );
  else           file_no = open( flspec, flags, omode );
  perr = errno;

/* Check for failure under access mode 3.  If it fails for this case,
   the file didn't exist so try to create it.  */
  if (file_no < 0 && mode == 3) { 
    flags = O_RDWR | O_CREAT | O_EXCL;
    omode = 0744;
    nblocks = (*nbytes + 511) / 512;
#if defined(BLOCK_BOUND_OS)
    tbytes = nblocks * 512;
#else
    tbytes = *nbytes;
#endif
    errno = 0;
    file_no = open( flspec, flags, omode);
    perr = errno;
  }

/* Now we can check to determine if the file was opened successfully */
  if (file_no < 0) {
    (void) sprintf(errbuf,"Unable to %s the file %s", open_mode, flspec);
    (void) pio_error("PIOIN-CREFAI", errbuf, -2, perr);
    *ret = -2;
    return (-2);
  }

/***********************************************************************
  Determine the size of the file
************************************************************************/
  if (flags & O_CREAT) {
    file_pos = tbytes;
#if (FTRUNCATE_AVAIL_OS == 1 )
    errno = 0;
    status = ftruncate(file_no, (off_t) tbytes);
    perr = errno;
    if (status < 0) {
      (void) sprintf(errbuf,"Error allocating %ld bytes to file %s",
                     (long) tbytes, flspec);
      (void) pio_error("PIOIN-FILERR", errbuf, -3, perr);
      *ret = -3;
      return (-3);
    }
#endif
  }
  else if (mode != 6) { 
    errno = 0;
#if (FSTAT_AVAIL_OS == 1 )
    status = fstat( file_no, &sb );
    perr = errno;
    tbytes = (INT4) sb.st_size;
#else
    tbytes = (INT4) lseek(file_no , (off_t) 0, SEEK_END);
    perr = errno;
    if (tbytes < 0) status = -1;
    else status = 0;
#endif
    if (status < 0) {
      (void) sprintf(errbuf, "Error determining size of file %s",
                     flspec);
      (void) pio_error("PIOIN-FILERR", errbuf, -3, perr);
      *ret = -3;
      return (-3);
    }  
    *nbytes = tbytes;
    nblocks = (tbytes + 511) / 512;
  }
        
/************************************************************************
  Allocate an internal PRIMIO FCB.
*************************************************************************/
  fb = (PRIMIO_FCB *) malloc(sizeof(PRIMIO_FCB));
  if (fb == NO_FCB) { 
    (void) sprintf(errbuf,"Unable to allocate FCB for file %s", flspec);
    (void) pio_error("PIOIN-ALLFAI", errbuf, -4, 0);
    *ret = -4;
    return (-4);
  }
 
/*************************************************************************
  Set up the internal FCB for the file
**************************************************************************/
  *fid = fb->fid = u_new_id();
  fb->access = ((flags & O_RDWR) ? PRIMIO_READ_WRITE : PRIMIO_READ_ONLY);
  fb->mode = mode;
  fb->file_handle = file_no;
  strcpy( fb->filnam, flspec);
  fb->tbytes = tbytes;
  fb->nbytes = *nbytes;
  fb->disposition = ((mode == 4) ? DELETE : KEEP);

/*************************************************************************
  Insert the FCB into the PRIMIO internal file table
**************************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++) {
    if (file_fcbs[i] == NO_FCB) {
       file_fcbs[i] = fb;
       break;
    }
  }
        
/* Ensure that the FCB has been added to the open file list */
  if (i >= MAX_FILES) {
    (void) sprintf(errbuf, "Maximum number files exceeded (%ld) for file %s", 
                   (long) MAX_FILES, flspec);
    (void) pio_error("PIOIN-MAXOVR", errbuf, -5, 0);
    *ret = -5;
    return (-5);
  }


  *ret = 0;
  return (0);
}



INT4 pio_cl(INT4 fid, INT4 idele, INT4 *ret)
/**************************************************************************
*_Title pio_cl Close an opened file
*_Args  Type  Name            I/O	Description
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of the opened file
*_Parm  INT4  idele;           I	Specifies to KEEP or DELETE the
*                                       file upon closure.
*                                         0 - Don't delete the file upon close
*                                         1 - Delete the file upon close
*_Parm  INT4  *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed
*                                        -2 - error deleting file

*_Desc  pio_cl closes a file that has been opened by pio_in.  The caller
*       may optionally request to delete the file upon closure of the
*       file. 

*_Keys  FILE_I/O

*_Hist  Oct 21 1992 Kris Becker, U.S.G.S., Flagstaff Original C Version
*       Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 12 1996 KJB - Ensure error number is preserved
*_End
**************************************************************************/
{
  register INT4 i, status;
  INT4 perr;                   
  CHAR errbuf[256];

/************************************************************************
  Find the specified file
*************************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++ )
    if (file_fcbs[i] != NO_FCB) 
      if (file_fcbs[i]->fid == fid) break;
       
/***********************************************************************
  Close the file and optionally delete it
************************************************************************/
  if (i < MAX_FILES) {
    errno = 0;
    status = close( file_fcbs[i]->file_handle );
    if (status != 0) {
      perr = errno;
      (void) sprintf(errbuf,"Error closing file %s associated with file id %ld",
                     file_fcbs[i]->filnam, (long) fid);
      (void) pio_error("PIOCL-CLSERR", errbuf, -1, perr);
      *ret = -1;
      return (-1);
    }

#if defined(UNLINK_AVAIL_OS)
/*  Now check if the file is to be deleted */
    if (idele == DELETE || file_fcbs[i]->disposition == DELETE) {
      errno = 0;
      status = unlink(file_fcbs[i]->filnam);
      if (status < 0) {
        perr = errno;
        (void) sprintf(errbuf,
                       "Error deleting file %s associated to file id %ld",
                       file_fcbs[i]->filnam, (long) fid);
        (void) pio_error("PIOCL-CLSERR", errbuf, -2, perr);
        *ret = -2;
        return (-2);
      }
    }
#endif

/* Free the PRIMIO FCB memory */
    (void) free((void *) file_fcbs[i]);
    file_fcbs[i] = NO_FCB;
  }

/****************************************************************
   All done...return to caller
*****************************************************************/
  *ret = 0;
  return (0); 
}



INT4 pio_rd(INT4 fid, INT4 ibyte, INT4 nbytes, void *buf, INT4 *ret)
/************************************************************************
*_Title pio_rd Read data from a file
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of the opened file
*_Parm  INT4  ibyte;           I	Starting byte of the file at which
*                                       to read the data from.  (ibyte=1
*                                       specifies the first byte in the
*                                       file.)
*_Parm  INT4  nbytes;          I        Number of bytes to read from the
*                                       file.  
*_Parm  void  *buf;            O        Buffer that will contain the data read
*                                       from the file upon return to the caller.
*_Parm  INT4  *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_rd reads data from a disk file.  A file is considered to be
*       a contiguous stream of bytes.  ibyte specifies the starting byte
*       position in the file to begin reading data from.  The first byte
*       position in the file is 1.  nbytes specifies the number of bytes
*       to read.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*       Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 12 1996 KJB - Ensure error number is preserved
*	Nov 22 1997 James M Anderson  --fix sprintf args
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  INT4 perr;                   
  INT4 n;

  CHAR errbuf[256];

  /* Declare internal functions here */
  PRIMIO_FCB *pio_get_fcb(INT4);

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,"Invalid file identifier (%ld) - file not found",
                   (long) fid);
    (void) pio_error("PIORD-INVFID", errbuf, -1, 0);
    *ret = -1;
    return (-1);
  }

/**********************************************************************
  Verify the requested byte addresses for the I/O operation
***********************************************************************/
  if (ibyte < 1 || ibyte > fb->nbytes) {
    (void) sprintf(errbuf, "Invalid byte address (%ld) for read in file %s", 
                   (long) ibyte, fb->filnam);
    (void) pio_error("PIORD-INVADR", errbuf, -2, 0);
    *ret = -2;
    return (-2);
  }
       
  if (nbytes < 1 || (ibyte + nbytes -1) > fb->nbytes) {
    (void) sprintf(errbuf,"Invalid number bytes (%ld) for read in file %s", 
                   (long) nbytes, fb->filnam);
    (void) pio_error("PIORD-INVADR", errbuf, -3, 0);
    *ret = -3;
    return (-3);
  }

/***********************************************************************
  Position the file pointer to the proper requested file byte address
  and read the data.
************************************************************************/
  errno = 0;
  n = lseek( fb->file_handle,  ibyte-1, SEEK_SET );
  perr = errno;
  if (n != (ibyte -1)) {
    (void) sprintf(errbuf,"Error position file pointer at byte %ld in file %s",
                   (long) ibyte, fb->filnam);
    (void) pio_error("PIORD-POSERR", errbuf, -4, perr);
    *ret = -4;
    return (-4);
  }

/* Read the data to the file */
  errno = 0;
  n = read( fb->file_handle, buf, nbytes );
  perr = errno;
  if (n != nbytes) {
    (void) sprintf(errbuf,"Error reading %ld bytes at byte %ld from file %s", 
                   (long) nbytes, (long) ibyte, fb->filnam);
    (void) pio_error("PIORD-IOERR", errbuf, -5, perr);
    *ret = -5;
    return (-5);
  }

  *ret = 0;
  return (0);
}



INT4  pio_wt(INT4 fid, INT4 ibyte, INT4 nbytes, void *buf, INT4 *ret)
/************************************************************************
*_Title pio_wt Write data to a file
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of the opened file
*_Parm  INT4  ibyte;           I	Starting byte of the file at which
*                                       to write the data from.  (ibyte=1
*                                       specifies the first byte in the
*                                       file.)
*_Parm  INT4  nbytes;          I        Number of bytes to write to the
*                                       file.  
*_Parm  void  *buf;            I        Buffer that contains the data to write
*                                       the file.
*_Parm  INT4  *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_wt writes data to a disk file.  A file is considered to be
*       a contiguous stream of bytes.  ibyte specifies the starting byte
*       position in the file to begin writing data to.  The first byte
*       position in the file is 1.  nbytes specifies the number of bytes
*       to write.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*       Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 12 1996 KJB - Ensure error number is preserved
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  INT4 perr;                   
  INT4 n;

  CHAR errbuf[256];

  /* Declare internal functions here */
  PRIMIO_FCB *pio_get_fcb(INT4);

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,"Invalid file identifier (%ld) - file not found",
                   (long) fid);
    (void) pio_error("PIOWT-INVFID", errbuf, -1, 0);
    *ret = -1;
    return (-1);
  }

/*********************************************************************
  Ensure we have write access to the file
**********************************************************************/
  if (fb->access != PRIMIO_READ_WRITE) {
    (void) sprintf(errbuf,"File %s does not have write access",
                   fb->filnam);
    (void) pio_error("PIOWT-INVACC", errbuf, -2, 0);
    *ret = -2;
    return (-2);
  }
       
/**********************************************************************
  Verify the requested byte addresses for the I/O operation
***********************************************************************/
  if (ibyte < 1 || ibyte > fb->nbytes) {
    (void) sprintf(errbuf,"Invalid byte address (%ld) for write in file %s", 
                   (long) ibyte, fb->filnam);
    (void) pio_error("PIOWT-INVARD", errbuf, -3, 0);
    *ret = -3;
    return (-3);
  }
       
/***********************************************************************
  Position the file pointer to the proper requested file byte address
  and write the data.
************************************************************************/
  errno = 0;
  n = lseek( fb->file_handle,  ibyte-1, SEEK_SET );
  perr = errno;
  if (n != (ibyte - 1)) {
    (void) sprintf(errbuf,"Error position file pointer at byte %ld in file %s",
                   (long) ibyte, fb->filnam);
    (void) pio_error("PIOWT-POSERR", errbuf, -4, perr);
    *ret = -4;
    return (-4);
  }

/* Write the data to the file */
  errno = 0;
  n = write( fb->file_handle, buf, nbytes );
  perr = errno;
  if (n != nbytes) {
    (void) sprintf(errbuf,"Error writing %ld bytes at byte %ld to file %s", 
                   (long) nbytes, (long) ibyte, fb->filnam);
    (void) pio_error("PIOWT-IOERR", errbuf, -1, perr);
    *ret = -5;
    return (-5);
  }

/* Determine if the write extends the file and update the internal FCB */
  if ((ibyte+nbytes-1) > fb->nbytes) {
    fb->nbytes = ibyte + nbytes - 1;
    fb->tbytes = MAX(fb->nbytes, fb->tbytes);
  }

  *ret = 0;
  return (0);
}


INT4 pio_ap(INT4 fid, INT4 nbytes, INT4 *ntbytes, INT4 *ret)
/************************************************************************
*_Title pio_ap Extend a file size by a specified number of bytes
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of an open file
*_Parm  INT4  nbytes;          I        Number of bytes to extend the file by.  
*_Parm  INT4  ntbytes;         O        Total number of bytes in the file
*                                       after the extension.
*_Parm  INT4  *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_ap will extend a file by nbytes bytes.  These bytes are added
*       to the end of the file.  Upon return, ntbytes will contain the
*       total number of bytes in the file.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*       Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 12 1996 KJB - Ensure error number is preserved
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  INT4 perr;                   
  UINT1 dummy = 0;
  
  INT4 status;
  CHAR errbuf[256];

/* Declare internal functions here */
  PRIMIO_FCB *pio_get_fcb(INT4);

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf, "Invalid file identifier (%ld) - file not found",
                 (long) fid);
    (void) pio_error("PIOAP-INVFID", errbuf, -1, 0);
    *ret = -1;
    return (-1);
  }

/*********************************************************************
  Ensure we have write access to the file so that the file may be
  extended.  Then extend the file to the requested size
**********************************************************************/
  if (nbytes > 0) {
    if (fb->access != PRIMIO_READ_WRITE) {
      (void) sprintf(errbuf,"File %s does not have write access", fb->filnam);
      (void) pio_error("PIOAP-INVACC", errbuf, -2, 0);
      *ret = -2;
      return (-2);
    }

/* Now update the FCB */
    fb->nbytes += nbytes;
    fb->tbytes = MAX(fb->nbytes, fb->tbytes);

/* Now physically extend the file */
#if (FTRUNCATE_AVAIL_OS == 1 )
    errno = 0;
    status = ftruncate(fb->file_handle, (off_t) fb->nbytes);
    perr = errno;
#else
    (void) pio_wt(fid, fb->nbytes, 1, (void *) &dummy, &status);
     perr = 0;
#endif

/* Check for status of append operation */
    if (status < 0) {
      (void) sprintf(errbuf,"Error extending %ld bytes in file %s",
                     (long) nbytes, fb->filnam);
      (void) pio_error("PIOAP-EXTERR", errbuf, -3, perr);
      *ret = -3;
      return (-3);
    }
  }

  *ntbytes = fb->nbytes;
  *ret = 0;
  return (0);
}


static PRIMIO_FCB *pio_get_fcb(INT4 fid)
/************************************************************************
*_Title pio_get_fcb Return the PRIMIO file control block for file id
*_Nobind
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of the opened file
*_Desc  pio_get_fcb returns the address of a PRIMIO file control block
*       structure that is associated with fid.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*_Hist  Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*_End
**************************************************************************/
{
  register INT4 i;

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++ )
    if (file_fcbs[i] != NO_FCB)
      if (file_fcbs[i]->fid == fid) 
        return (file_fcbs[i]);

/* If we reach here, it doesn't exist */
  return (NO_FCB);
}     



INT4 pio_reset_access(INT4 fid, INT4 access, INT4 *ret)
/**************************************************************************
*_Title pio_reset_access Close file then reopen with appropriate access
*_Args  Type  Name            I/O	Description
*_Parm  INT4  fid;             I	File identifier of the opened file
*_Parm  INT4  access; 	       I	Access mode:
*				          1 - Read-only existing file
*				          2 - Read/write existing file
*_Parm  INT4   *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - Invalid file identifier
*                                        -2 - Error closing file
*                                        -3 - Error opening file READ-WRITE
*                                        -4 - Wrong access sent
*
*
*_Desc  pio_reset_access closes a file that has been opened by pio_in
*	then reopens the same file in READ-WRITE or READ-ONLY
*	depending on the access mode sent.
*
*_Keys  FILE_I/O
*
*_Hist  Nov 04 1994 Trent Hare, U.S.G.S., Flagstaff (from: pio_cl,pio_in).
*       Nov 18 1994 Trent Hare -- changed to use ISIS types (INT4 etc.)
*       Aug 19 1996 KJB - Reworked error reporting
*       Nov 12 1996 KJB - Ensure error number is preserved
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  INT4 perr;                   
  INT4 file_no;
  INT4 omode;
  INT4 flags;		
  CHAR *open_mode;

  register INT4 status;
  CHAR errbuf[256];

/* Declare internal functions here */
  PRIMIO_FCB *pio_get_fcb(INT4);

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,"Invalid file identifier (%ld) - file not found",
                   (long) fid);
    (void) pio_error("PIORESETACC-INVFID", errbuf, -1, 0);
    *ret = -1;
    return (-1);
  }

/***********************************************************************
  Check what access was sent
************************************************************************/
  if (access == 2) {
/* Close the file */
     errno = 0;
     status = close( fb->file_handle );
     perr = errno;

/* Now we can check to determine if the file was closed successfully */
     if (status != 0) {
       (void) sprintf(errbuf,
                      "Error closing file %s associated with file id %ld",
                      fb->filnam, (long) fid);
       (void) pio_error("PIORESETACC-CLSERR", errbuf, -2, perr);
       *ret = -2;
       return (-2);
     }

/* Set up for PRIMIO_READ_WRITE access */
     flags = O_RDWR;
     omode = 0;
     open_mode = "open (PRIMIO_READ_WRITE)";

     errno = 0;
     file_no = open( fb->filnam, flags, omode );
     perr = errno;

/* Now we can check to determine if the file was opened successfully */
     if (file_no < 0) {
       (void) sprintf(errbuf,"Unable to %s the file %s", open_mode, fb->filnam);
       (void) pio_error("PIORESETACC-OPNFAI", errbuf, -3, perr);
/* return a file_handle -1 so it will cause a fatal error */
       fb->file_handle = -1;
       *ret = -3;
       return (-3);
     }

/*************************************************************************
  Update the internal FCB for the file
**************************************************************************/
     fb->access = PRIMIO_READ_WRITE;
     fb->mode = PRIMIO_READ_WRITE;
     fb->file_handle = file_no;
  }
  else {
/*************************************************************************
  Wrong access sent set file_handle to -1 to cause a fatal error
**************************************************************************/
     (void) sprintf(errbuf, "Error: access mode %ld sent", (long) access);
     (void) pio_error("PIORESETACC-ACCESS", errbuf, -4, 0);
     *ret = -4;
     return (-4);
  }


  *ret = 0;
  return (0);
}


INT4 pio_truncate(INT4 fid, INT4 nbytes, INT4 *ret)
/**************************************************************************
*_Title pio_truncate Truncate a file to specified size
*_Args  Type  Name            I/O       Description
*_Parm  INT4  fid;             I        File identifier of the opened file
*_Parm  INT4  nbytes;          I        Total size of file in bytes to
*                                       truncate the file to.
*_Parm  INT4   *ret;            O       Return code:
*                                         1 - function not supported on this
*                                             platform
*                                         0 - ok
*                                        -1 - Invalid file identifier
*                                        -2 - file not opened for WRITE
*                                        -3 - Error truncating file
*
*
*_Desc  pio_truncate will truncate a file to the size specified by the
*       caller via nbytes.  Note that some systems may not support this
*       operation.
*
*_Keys  FILE_I/O
*
*_Hist  Nov 25 1996 Kris Beckerm U.S.G.S., Flagstaff
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  INT4 perr;
  INT4 file_no;
  INT4 omode;
  INT4 flags;
  CHAR *open_mode;
 
  register INT4 status;
  CHAR errbuf[256];
 
/* Declare internal functions here */
  PRIMIO_FCB *pio_get_fcb(INT4);
 
/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,"Invalid file identifier (%ld) - file not found",
                   (long) fid);
    (void) pio_error("PIOTRUNC-INVFID", errbuf, -1, 0);
    *ret = -1;
    return (-1);
  }
 
/***********************************************************************
  Truncate the file
************************************************************************/
  if (fb->access != PRIMIO_READ_WRITE) {
    (void) sprintf(errbuf,"File %s does not have write access", fb->filnam);
    (void) pio_error("PIOTRUNC-INVACC", errbuf, -2, 0);
    *ret = -2;
    return (-2);
  }
 
/* Now physically extend the file */
#if (FTRUNCATE_AVAIL_OS == 1 )
  errno = 0;
  status = ftruncate(fb->file_handle, (off_t) nbytes);
  perr = errno;
  if (status < 0) {
    (void) sprintf(errbuf,"Error truncating file %s to %ld bytes",
                   fb->filnam, (long) nbytes);
    (void) pio_error("PIOTRUNC-OPRFAI", errbuf, -3, perr);
    *ret = -3;
    return (-3);
  }

/* Now update the FCB */
  fb->nbytes = nbytes;
  fb->tbytes = nbytes;
 
/*  All done...return to caller */
  *ret = 0;
  return (0);

#else
/* Operation does not exist on this platform */
   *ret = 1;
   return (1);
#endif
}


static INT4 pio_error(CHAR routine[], CHAR errmsg[], INT4 errnum, INT4 sysno)
/**************************************************************************
*_Title pio_error Report PRIMIO error to appropriate output stream
*_Args  Type  Name            I/O	Description
*_Parm  CHAR  routine[];       I        Name of calling routine
*_Parm  CHAR  errmsg[];        I        Error message to report
*_Parm  INT4  errnum;          I        Error number 
*_Parm  INT4  sysno;           I        System error number 

*_Desc pio_error will report the error generated by the calling routine
*      to the appropriate stream.  sysno is assummed to be the value of
*      errno (the system error facility) and this routine will include
*      the appropriate system text message in the reporting of the error.

*_Keys  FILE_I/O

*_Hist  Aug 19 1996 Kris Becker Flagstaff Original Verison
*       Dec 01 2003 KJB - Modified how system error strings are resolved.
*                          Now uses strerror as opposed to direct use of
*                          sys_nerr and sys_errlist.  Definition of
*                          USE_OLD_ERRNO macro will employ old implementation
*_End
**************************************************************************/
{
   CHAR errbuf[2048];               /* Error buffer */

/*****************************************************************
  Write the error to the appropriate stream
******************************************************************/
#if defined(NO_ISIS)
    (void) sprintf(errbuf,"%%%s - (%ld) - %s", routine, (INT4) errnum, errmsg);
    (void) perror(errbuf);
#else
#if defined(USE_OLD_ERRNO)
    if (sysno > 0 && sysno <= sys_nerr) 
      (void) sprintf(errbuf,"%s: %s", errmsg, sys_errlist[sysno]);
    else 
      (void) sprintf(errbuf,"%s", errmsg);
#else
    (void) sprintf(errbuf,"%s: %s", errmsg, strerror(sysno));
#endif
    (void) u_error(routine, errbuf, errnum, 1);
#endif

/****************************************************************
  All done...
*****************************************************************/
  return (0);
}
