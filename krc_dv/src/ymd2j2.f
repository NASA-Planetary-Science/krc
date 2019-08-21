       SUBROUTINE YMD2J2 (IYEAR,IMON,IDAY, DJ)
C_TITLE  YMD2J2  Convert year, month, day to Julian date offset from J2000
       IMPLICIT NONE
C_Args
       INTEGER IYEAR          ! in. 4-digit year, e.g. 1985
       INTEGER IMON           ! in. month count, 1 thru 12
       INTEGER IDAY           ! in. day of month
       INTEGER DJ             ! out. Julian day, offset from J2000=2451545
C_DESC Julian days are integral at noon UTC
C In Gregorian calendar since 1582: 
C if year is divisible by 400 then
C    is_leap_year
C else if year is divisible by 100 then
C    not_leap_year
C else if year is divisible by 4 then
C    is_leap_year
C else
C    not_leap_year
C_Lims Correct for at least 1601 to 2399
C Tested against  NumRec JULDAY for 6th day of every month of every year 
C from 1601 to 2399
C_HISTORY 2013feb07 Hugh Kieffer Adopt from YMD2JD 
C 2016mar22 HK untabify   2016may23 HK Comment/remove unused variables
C_Calls 0
C_END
       INTEGER ID0(12) ! days before start of month on non-leapyear
       DATA ID0 /0,31,59,90,120,151,181,212,243,273,304,334/
C       INTEGER IFEB29 /60/ 
       INTEGER I1YEAR /365/   ! days in a non-leap year
       INTEGER I4YEAR /1461/  !  4*I1YEAR +1
       INTEGER ICEN /36524/   !  24*I4YEAR-1
       INTEGER I4CEN /146097/ !  4*ICEN +1
       INTEGER ILEAP /60/     ! Feb 29 on a leapyear in 4-yr block  
       INTEGER DJ0 /-1/       ! overall offset after -4 centuries

       INTEGER J,JC,N4YEAR,NCEN,N4CEN       ! local variables

       J = IYEAR - 1600       ! number of years after 1600
       N4CEN=J/400            ! number of whole 4centuries
       J=J-400*N4CEN          ! number of years into a 4century
       NCEN=J/100             ! number of whole centuries
       JC=J-100*NCEN          ! number of years into a century
       N4YEAR = JC/4          ! number of 4-year blocks
       J = JC - 4*N4YEAR      ! remaining number of years
       DJ = J*I1YEAR + ID0(IMON) + IDAY       ! remaining number of days
C 2/29 and 3/1 are the same DOY in non-leap, so they will yield the same JD

C Test if a leap day before remaining days
       IF (DJ.GT.ILEAP .OR. (DJ.EQ.ILEAP .AND. IMON.EQ.3)) DJ=DJ+1 

C The following found necessary for this algorithm. There is probably some 
C less obvious algorithm that does not require this exception for the
C first 2 months of a century but not a 4century.
       IF (NCEN.GE.1 .AND. JC.EQ.0 .AND. IMON.LT.3) DJ=DJ+1 ! 

       DJ = DJ +N4YEAR*I4YEAR +NCEN*ICEN +(N4CEN-1)*I4CEN +DJ0 ! Julian date offset from 2451545
       RETURN
       END

