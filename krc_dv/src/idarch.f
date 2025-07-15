      FUNCTION IDARCH(ARCH)
      IMPLICIT NONE
C**********************************************************************
C_Title idarch Returns the running architecture as a string ident
C_Args
      CHARACTER*(*) ARCH             ! O   Returns a string identifying the
C                                          current running architecture.
C                                          Possible values are:
C                                             UNK    - Unknown/indetermined
C                                             SPARC  - SUN Solaris or SunOS
C                                             PCI    - Intel PC based system
C                                             ALPHA  - DEC Alpha
C                                             SGI    - Silicon Graphics
      INTEGER*4     IDARCH           ! O   Returns a numerical value that
C                                          corresponds to the current
C                                          system architecture.  Possible
C                                          values are:
C                                              0 - Unknown/indetermined
C                                              1 - SUN SPARC Solaris or SunOS
C                                              2 - PC w/Intel Inside
C                                              3 - DEC ALPHA w/Digital Unix
C                                              4 - SGI Irix
C                                              5 - X86_64 Architecture
      
C_Descr idarch will return the system architecture that this routine
C       is built on.  This implies that the architecture is determined
C       at the compilation phase by compiler defines.  This further
C       implies that this routine needs to be fed to the language
C       preprocessor.  On most Unix-based systems, this is automatic
C       if the source file ends in .F.

C_Keys SYSTEM

C_Hist May 12 1998 Kris Becker Flagstaff Original implementation
C 98may12  HHK modify returned values
C      Apr 01 2008 Kris Becker Added X86_64 architecture, string is
C                              increased in size to at least 6 chars
C_End
C**********************************************************************

C*******************************************************************
C  Let the preprocessor do the determination for us.
C*******************************************************************
#if defined(sparc) || defined(sun) 
      ARCH = 'sparc '
      IDARCH = 2
#elif defined(msdos) || defined(__i386__) || defined(__i486__)
      ARCH = 'x86   '
      IDARCH = 1
#elif defined(__osf__) 
      ARCH = 'alpha '
      IDARCH = 3
#elif defined(sgi) || defined(__sgi)
      ARCH = 'sgi   '
      IDARCH = 4
#elif defined(__x86_64__) || defined(__amd64__)
      ARCH = 'x86_64'
      IDARCH = 5
#else
      ARCH = 'unknow'
      IDARCH = 0
#endif
      RETURN
      END

