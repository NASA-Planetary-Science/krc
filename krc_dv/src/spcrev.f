      SUBROUTINE SPCREV (PQ,START,DAY, REV)
C_Titl  SPCREV  Returns spacecraft revolution number for Viking or uniform period
C_args
      real*4 pq    !in  >0 = orbit period
C                        0 = none, routine returns 0.
C                       <0 = - spacecraft index: -1,-2 =  Viking  Orbiter 1,2
      real*4 start !in. Date at periapsis of orbit zero; ignored unless pq>0.
                       ! must be in J2000 system for Viking
      real*4 day   !in. = request date; offset from J2000.0 (2000 jan 01 noon UT)
      real*4 rev   !out. real orbit number. Orbits start at apoapsis.
C_Lims
      PARAMETER (MAXK=2) ! number of spaceraft stored
C_Desc:
C 98sep07  Source of these data tables unknown.
C  Snyder  JGR 84 p 7924 is table of  VO revs, but does not have as much 
C detail as contained herein. Some values here disagree with that article.
C  VO-2 Insertion 1976jun19, died 1978jul25 0601  UT  JD=2443715  Rev706
C  VO-1 Insertion 1976aug07, died 1980aug07  JD=2444459  Rev~1501 
C 2016mar22 HK untabify
C 2019dec06 HK Convert PQ date for Viking from JD=244000 to J2000 (JD=2451545)
C_End
      DIMENSION KO(MAXK)
      DATA KO/1,13/  !  index of first entry in table for this spacecraft
C data tables for each spacecraft hold info. for start of interval of 
C    constant orbital period. Last entry must have large date to terminate
C    a search.
C  lr = rev number at periapsis (rev starts at preceeding apoapsis)
C  pj = Date of periapsis relative to JD 2440000 (KRC version 1)
C  p  = orbit period in hours
      real*4 dmj  ! j200 date
      DIMENSION PJ(22),P(22),LR(22)
      DATA LR/3,83,97,214,264,279,380,899
     &,1062,1121,1227,9999
     &,4,19,52,124,190,236,405,418,432,9999/
      DATA P/24.59,22.17,24.54,22.98,22.02,23.51,23.98,24.85
     &,24.99,24.79,24.0,9999.
     &,26.75,24.71,26.80,26.45,24.53,22.74,24.31,24.22,23.98,99999./
      DATA PJ/2952.25,3034.21,3047.14,3166.76,3214.63,3228.39
     &,3327.33,3845.85,4014.62,4076.06,4185.55,99999.
     &,3002.52,3019.28,3053.26,3133.66,3206.41
     &,3253.43,3413.59,3426.76,3440.89,99999./
C
      REV=0.
      IF (PQ.EQ.0.) RETURN   ! input period sero, return sero 
      IF (PQ.GT.0.) THEN     ! assume uniform orbit period
            REV=(DAY-START)/PQ
            RETURN
          ENDIF
C pq was negative spacecraft number. Expect 1 or 2
      K= -PQ + 0.5
      IF (K.GT.MAXK) RETURN
      I = KO(K)
C search forward for interval containing the request time
      dmj=day+11545.0 ! convert from J2000 to JD-244000 of the tables
20    IF (DMJ.GT.PJ(I+1)) THEN
            I=I+1
            GOTO 20
          ENDIF
      REV = (DMJ-PJ(I))/(P(I)/24.0)+LR(I)+0.5
      RETURN
      END
