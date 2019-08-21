      SUBROUTINE ERRDEL (KODE)
C_Titl  ERRDEL: report total or changes to error count
      IMPLICIT NONE
C_Vars
      INCLUDE 'errcom.inc'
C_Args
      INTEGER KODE ! in. logical unit for report (absolute value)
C  0=reset all counts to zero  
C  +=report changes since the last call (and reset saved values to current
C  -=report total counts
C_Hist  97sep10  Hugh_Kieffer  98jun29 change name  __Elev to  __Pres
C       99nov17  Hugh_Kieffer  Added initalization of units in DATA statement
C 2010apr13  HK  Add debug more extensive change report. Add save Keep
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
      DATA IDB1/-1/, IDB2/-1/, IDB3/-1/, IDB4/-1/, IOERR/0/
      INTEGER IOP,I,J,K,J1,J2

      INTEGER*4 KEEP(MERR)      ! save the values now in common
      CHARACTER*6 NAME(MERR) ! short names
      DATA NAME /'LoHour','HiHour',' LoLat',' HiLat','LoPres','HiPres'
     &,'LoOpac','HiOpac','  Lo T','  Hi T'
     &,'ELoLat','EHiLat','LoLong','HiLong','All 1T'/
      INTEGER NJ /10/ ! number of items on one output line
      SAVE KEEP
      
      IOP = ABS(KODE)           ! logical unit to use
      IF (KODE) 10,20,30        ! totals | set to zero | report changes
      
C  Report total values for all items
 10   WRITE (IOP,*)'Total counts in errcom'
      DO J=1,MERR,NJ            ! do one line_pair of output
        J1=J                    ! first item
        J2=MIN(J-1+NJ,MERR)     ! last item
        WRITE(IOP,12) (NAME(I),I=J1,J2) ! row of names
        WRITE(IOP,13) (KERR(I),I=J1,J2) ! row of counts
      ENDDO
 12   FORMAT(10(2X,A6))
 13   FORMAT (10I8)
      GOTO 9

C  Rest all counts to zero
 20   DO J=1,MERR               ! reset all counts and save values to zero
        KERR(J)=0
        KEEP(J)=0
      ENDDO
      GOTO 9

C  Report only the changes
 30   K=0                       ! number of items that have cahnges
      DO J=1,MERR
        I=KERR(J)-KEEP(J)       ! change since last call
        IF (I.NE.0) THEN        ! this item changed
C          WRITE (IOP,31)J,NAME(J),I
          WRITE (IOP,32)J,NAME(J),I  ,KEEP(J),KERR(J)
 31       FORMAT ('Error>',I3,1X,A8, ' changed by ',I8)
 32       FORMAT ('Error>',I3,1X,A8, ' changed by ',I8,'  Was:is',2I8)
          KEEP(J)=KERR(J)       ! save the new value
          K=K+1                 !increment count of items that have cahnged
        ENDIF
      ENDDO
      IF(K.EQ.0) WRITE (IOP,*) 'ERRDEL: no changes' ! specific null response
      
 9    RETURN
      END
