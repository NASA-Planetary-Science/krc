      SUBROUTINE CATIME (DATETIME)
C_TITL  CATIME  Returns current date & time as " yyyy mon dd hh:mm:ss "
      IMPLICIT NONE
C_ARGS
      CHARACTER*22 DATETIME   !out, date/time value
C_DESC  Uses Intrinsic functions to get date/time, rearranges bytes
C
C_CALLS  Intrinsic functions:  TIME  CTIME
C
C_HIST  98jan22  Hugh_Kieffer character version derived from  DATIME
C 2014mar02 HK Use  IMPLICIT NONE 
C 2016mar22 HK untabify
C_END
      INTEGER*4 J           ! returned by system function
      INTEGER*4 TIME        ! intrinsic function name
C       CHARACTER*(*) CTIME     ! intrinsic function name
      CHARACTER*24 STRING     ! working string

      J=TIME()            ! seconds since 1970.0
      STRING=CTIME(J)       ! convert to string

C     123456789012345678901234
C     www mon dd hh:mm:ss yyyy   from ctime
C     _yyyy mon dd hh:mm:ss_     desired output
      DATETIME(1:1)=' '
      DATETIME(2:5)=STRING(21:24) ! year
      DATETIME(6:22)=STRING(4:20) ! rest

      RETURN
      END
