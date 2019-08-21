        SUBROUTINE  PORB08
C_Titl PORB08 Planetary orbit. Read pre-computed matrices; minimal for KRC  DP
C_HIST  97jan30  HHK revised from  PORB1
C 2005dec28 HK Change to use of IMPLICIT NONE
C 2013jul24 HK Change to V2 PORB system
C 2014mar11 HK Make  REAL*8  version
C 2016may12 HK Update include name
C_END
        INCLUDE 'porbc8m.f' ! contains IMPLICIT NONE
        INCLUDE 'unic8m.f'

        REAL*8 P(30)            ! block of 30 floats to read
        EQUIVALENCE (P,PLANUM)  ! correspond to start of common
        LOGICAL LOP             ! debug print

        LOP=IDB1.GT.3
        IF (LOP) WRITE(IOSP,*)'PORB0 1'
        READ(IOIN,33)   ! skip the line of PORB runtime
        IF (LOP) WRITE(IOSP,*)'PORB0 2'
        READ(IOIN,33) P
        IF (LOP) WRITE(IOSP,33) P
        IF (LOP) WRITE(IOSP,*)'PORB0 3'
33      FORMAT (5G15.7) 
        PICON=3.141592653589793D0 ! ensure value in porb commom is set.
        R2D=180.D0/PICON

        IF (LOP) WRITE(IOSP,*)'PORB0 4'
        RETURN
        END
