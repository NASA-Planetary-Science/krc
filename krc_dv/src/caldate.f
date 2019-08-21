      SUBROUTINE CALDATE(DJIN, IYEAR,IMON,IDAY,FDAY,IDATE,MONTH,DAY)
C_Titl  CALDATE convert MJD or julian date to year, month, day, day-of-week/year
      IMPLICIT NONE
C_Args
      REAL*4 DJIN       !i. julian date, rel. to 2000.0 or full
      INTEGER*4 IYEAR           !o. year
      INTEGER*4 IMON            !o. month of year, 1 through 12
      INTEGER*4 IDAY            !o. day of month, 1 through 31
      REAL*4 FDAY               !o. fraction of  GMT (civil) day.
      INTEGER*4 IDATE           !o. day-of-year
      CHARACTER*3 MONTH         !o. the month
      CHARACTER*3 DAY           !o. the weekday
C_Desc
C Works on any day in Gregorian calander
C Civil days are offset from  JD by 12 hours.
C_Calls: Numerical Recipes:  CALDAT
C_Hist
C 2012feb29  HK Simplification of JDATE by use of  NumRec  caldat
C 2014jun09 HK  WARNING: Order of last 3 argumemnts changed.  Use J2000 base,
C               Untabify, replace use of integers for strings
C_End

      REAL*8 DJBASE /2451545.D0/    ! J2000 system base day
      REAL*8 FULLJD             ! full Julian date
      REAL*8 FDDAY              ! double precision fractional  GMT
      INTEGER*4 JULDAY          ! function
      INTEGER*4 JD0,IW,JULIAN         
      
      CHARACTER*3 WDAY(7),MON(12)
      DATA WDAY /'SUN','MON','TUE','WED','THU','FRI','SAT'/
      DATA MON /'JAN','FEB','MAR','APR','MAY','JUN',
     &      'JUL','AUG','SEP','OCT','NOV','DEC'/

      FULLJD=DJIN
      IF (DJIN.LT.1.22D6) FULLJD=DJBASE+DJIN
C now have full Julian date, virtually certain to be positive

      JULIAN=IDINT(FULLJD)       ! get floor integer
      FDDAY=DMOD(FULLJD,1.D0)+0.5d0 ! double precision needed
      IF (FDDAY.GE. 1.D0) THEN  ! ensure fractional day <1.
        FDDAY=FDDAY-1.D0
        JULIAN=JULIAN+1
      ENDIF
      FDAY=SNGL(FDDAY)          ! from double to single precision
      
      CALL CALDAT(JULIAN, IMON,IDAY,IYEAR) ! NumRec algorithm
      MONTH=MON(IMON) ;         ! month as 3-characters
      JD0=JULDAY(1,1,IYEAR)     ! first day of the year
      IDATE=JULIAN-JD0+1        ! day of year
      
      IW=MOD(JULIAN-1,7)          ! day of week,0=Sunday
      DAY=WDAY(IW+1)            ! day of week as 3 characters

      RETURN
      END
