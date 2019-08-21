      SUBROUTINE PORBIT (KODE, TMJD,LSUBS, SDEC,DAU )
C_TITLE  PORBIT  Converts between date and Ls. Also returns DAU and Sdec  R*8
C_VARS
      INCLUDE 'porbc8m.f'      ! has IMPLICIT NONE
C_ARGS  
      INTEGER*4 KODE  ! in.  1= T to Ls.  2= Ls to T
      REAL*8 TMJD   ! in/out. request time in days relative to J2000.0          
      REAL*8 LSUBS  ! out/in. planetocentric longitude of the Sun, degree       
      REAL*8 SDEC   ! out. Sub-solar latitude, degree
      REAL*8 DAU    ! out. Heliocentric distance, A.U.
C_DESCR conic orbit; uses solution to Keplers equation
C Uses rotation matric from orbital to seasons, from common
C Responds to  PBUG  in  PORBCM: bit-encoded
C +1 Show Print output
C_CALLS  ORBIT8  ROTVEC  VNEG  COCOCS
C_HIST  circa 1970  Hugh Kieffer ORIGINAL VERSION
C    delete intermediate history
C 2013jul24 HK Revise from PORB to KRC version 2 system
C 2014mar10 HK Make  REAL*8  version    jun6: rename variables to match porbc8m
C_END
C
      INTEGER*4 I,K
      REAL*8 T,P,Q,R,TAR           ! scalars
      REAL*8 ANOM,EA,CTA,STA         ! scalars for inverse
      REAL*8 HPFXX(3)           ! vectors

      IF (KODE.EQ.1) THEN        ! T to Ls -----------------------

         T = TMJD-TJP           ! time from periapsis
         CALL ORBIT8 (SJA,OPERIOD,XECC,T, R,PHFXX) ! get vector from sun to planet
         CALL VNEG (PHFXX, HPFXX) ! get to Sun from planet
         TAR=DATAN2(HPFXX(2),HPFXX(1)) ! true anomaly of Sun, radians
         LSUBS=(TAR-TAV)*R2D      ! planetocentric system longitude of the Sun
         IF (LSUBS.LT.  0.D0) LSUBS=LSUBS+360.D0 ! put on 0:360 range
         IF (LSUBS.GT.360.D0) LSUBS=LSUBS-360.D0 ! put on 0:360 range

      ELSE                      !  Ls to T  -----------------------

         TAR= (LSUBS-180.D0)/R2D +TAV    ! true anomaly of Planet; radians
         STA=DSIN(TAR)
         CTA=DCOS(TAR)
         P= DSQRT(1.D0-XECC**2)*STA ! sin E * 1+ecosE
         R= XECC+CTA             ! cos E * 1+ecosE
         EA=DATAN2(P,R)          ! Eccentric anomaly
         ANOM=EA-XECC* DSIN(EA)   ! Keplers equation to get mean anomaly
         T=OPERIOD*(R2D*ANOM/360.) ! days from periapsis
         TMJD=T+TJP
         R=SJA*(1.D0-XECC**2)/(1.D0+XECC * CTA) ! radial distance
         HPFXX(1)= -R*CTA       ! X part of neg of PHfxx
         HPFXX(2)= -R*STA       ! Y part of neg of PHfxx
         HPFXX(3)= 0.           ! Z part

      ENDIF                     ! -----------------------
 
      CALL ROTVEC (BFRM,HPFXX, HPBXX) ! rotate into Body vernal equinox system
      CALL COCOCS (HPBXX, P,Q,DAU) ! spherical angles in radians. Get DAU
      SDEC=90.D0-R2D*P            ! sub-solar latitude in degree

C Debug section
      K=IDINT(PBUG)
      I=MOD(K,2)                ! +1
      IF (I.EQ.1) THEN
         print *,'KODE,TMJD,LSUBS=',KODE,TMJD,LSUBS
         print *,'TAR,P,Q,R=',TAR,P,Q,R
         print *,'SDEC,DAU=',SDEC,DAU
      ENDIF

      RETURN
      END
