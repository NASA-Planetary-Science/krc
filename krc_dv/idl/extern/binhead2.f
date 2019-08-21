C     +----+-----------------------------------------------------------+
      SUBROUTINE BINHEAD2 
     &           ( ALBEDO, C_ARR, DELDATE, H, INERTIA,
     &             JDATE, NUMLAT, NUMTOD, OPACITY, PRESSURE )
C     +----+-----------------------------------------------------------+
C_Tit BINHEAD2 Read header of Mike Mellon thermal models
C     Created: 26 Sep 1997, Mike Mellon, University of Colorado
C
C     MOD: 22 May 1998, Mike Mellon,
C                       Adjusted format for new lookup table headers
C
C     This routine is for aquiring needed information from the header 
C     of the binary files created by makebin.pro (IDL) from the 
C     TESTHERM model output files. 
C     The header is passed as a character array C_ARR to this routine
C     and the appropriate variable are extracted and returned.
C     +----+-----------------------------------------------------------+
      IMPLICIT NONE
C     +----+-----------------------------------------------------------+
      CHARACTER ATXT*10     ! ascii albedo, ALGND
      CHARACTER BTXT*10     ! ascii number of latitudes, JO
      CHARACTER C_ARR(7104) ! array of header ascii characters
      CHARACTER ITXT*10     ! ascii inertia, TI
      CHARACTER LTXT*10     ! ascii starting Ls
      CHARACTER NTXT*10     ! ascii number of time steps per day, NT
      CHARACTER OTXT*10     ! ascii opacity, TAUDUST
      CHARACTER PTXT*10     ! ascii pressure, PSL
      CHARACTER QTXT*10     ! ascii output step interval, PSTEP
      CHARACTER STXT*10     ! ascii number of seasons, IDAY

      REAL*4    ALBEDO      ! ground surface albedo 
      REAL*4    DELDATE     ! season time step in Earth days
      REAL*4    H           ! pressure scale height (not available)
      REAL*4    INERTIA     ! ground thermal inertia [J/m^2 S^1/2 K]
      REAL*4    JDATE       ! julian date of first season
      REAL*4    LS0         ! Ls of first season
      REAL*4    OPACITY     ! visible dust opacity 
      REAL*4    PRESSURE    ! surface pressure [mb]
      INTEGER   I           ! index
      INTEGER   IDAY        ! number of seasons per year in file
      INTEGER   NT          ! number of time steps per day
      INTEGER   NUMLAT      ! number of latitudes in file
      INTEGER   NUMTOD      ! number of times of day in file
      INTEGER   PSTEP       ! number of time steps between model output

C     +----+-----------------------------------------------------------+
C...  Extract numeric ascii information from the header array of 
C...  characters and into each subsequent text variable.
C     +----+-----------------------------------------------------------+

      DO I = 1,10               
        ATXT(I:I) = C_ARR(124+I)  ! ground albedo
        ITXT(I:I) = C_ARR(168+I)  ! ground thermal inertia
        OTXT(I:I) = C_ARR(222+I)  ! visible dust opacity
        PTXT(I:I) = C_ARR(257+I)  ! surface pressure
        LTXT(I:I) = C_ARR(319+I)  ! starting Ls
        STXT(I:I) = C_ARR(612+I)  ! iday
        BTXT(I:I) = C_ARR(671+I)  ! jo
        QTXT(I:I) = C_ARR(844+I)  ! pstep
        NTXT(I:I) = C_ARR(1496+I) ! nt
      END DO 

C     +----+-----------------------------------------------------------+
C...  Translate each text variable into a numeric floating point variable 
C...  using an internal read statement. Format is implicit.
C     +----+-----------------------------------------------------------+

      READ (ATXT,*) ALBEDO        ! ground albedo
      READ (ITXT,*) INERTIA       ! ground thermal inertia [MKS]
      READ (OTXT,*) OPACITY       ! visible dust opacity
      READ (PTXT,*) PRESSURE      ! surface pressure [mb]
      READ (LTXT,*) LS0           ! starting Ls, first season
      READ (STXT,*) IDAY          ! number of seasons output
      READ (BTXT,*) NUMLAT        ! number of latitudes (assume even spacing)
      READ (QTXT,*) PSTEP         ! time steps between output 
      READ (NTXT,*) NT            ! number of time steps per day

C     +----+-----------------------------------------------------------+
C...  Julian Date
C...  All model runs of TESTHERM from the University of Colorado begin 
C...  at a starting season of Ls=0 exactly. To tie this to a Julian date
C...  the date of the last Ls=0 is used from Tabel I, pg 27 of Kieffer et al, 
C...  The planet Mars: From antiquity to present, in Mars, Kieffer, 
C...  Jakosky, Snyder, Matthews, U of A Press, 1992. 
C...      26.9 August 1996........Julian Date 2450322.4
C...  This starting season is constant in all model runs it is not 
C...  calculated here but stated. Only a check to make sure
C...  the starting Ls=0 is employed.
C     +----+-----------------------------------------------------------+

      JDATE = -9999.9
      IF (LS0.EQ.0.0) JDATE =  2450322.4

C     +----+-----------------------------------------------------------+
C...  Delta Julian date (seasonal time step in Earth days)
C...  This is calculated from the number a fixed 8.0  Mars day interval 
C...  in the new lookup tables. To make sure we are reading a fixed 8 day
C...  lookup table, check that IDAY equals 84, the number of seasons. 
C...  If not return an error flag of -9999.9. 
C     +----+-----------------------------------------------------------+
 
      DELDATE = -9999.9
      IF (IDAY.EQ.84) DELDATE = (88775.*8)/86400.

C     +----+-----------------------------------------------------------+
C...  Pressure scale height
C...  In the TESTHERM model atmospheric temperatures on which the scale
C...  height depends are explicitly calculated and are continuously 
C...  changing. In addition the scale height is not needed, calculated 
C...  or output. H is set to -9999 to indicate it is unavailble.
C     +----+-----------------------------------------------------------+

      H = -9999.9

C     +----+-----------------------------------------------------------+
C...  number of output time steps per day
C...  Calculated from the number of temperature time steps, NT and the
C...  printing inteveral, PSTEP.
C     +----+-----------------------------------------------------------+

      NUMTOD = NT/PSTEP

C     +----+-----------------------------------------------------------+
      RETURN
      END
C     +----+-----------------------------------------------------------+
