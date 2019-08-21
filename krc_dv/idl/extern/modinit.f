      SUBROUTINE MODINIT
C_Title  MODINIT  initialize the values in common.
C_Vars
      INCLUDE 'modcom.inc'
C_Hist 97feb02  Hugh_Kieffer  USGS original version
C  98may06  HHK convert to  Pascal, make  Mellon values the default
C  98jun04  HHK convert from  BLOCK DATA, and  set to  Mellon large model set
C_End789012345678901234567890123456789012345678901234567890123456789012_4567890
      HOUR1 = 1.0               ! firsthour
      hourdel = 1.              ! delta hour
      ALAT1 = -90.              ! first  latitude
      ALATDEL = 5.              ! delta latitude
      VT(1) = .0                ! model opacity values
      VT(2) = .5 
      VT(3) =  1.0
      VP(1) = 300.             ! model pressure values in  Pascal
      VP(2) = 600.
      VP(3) = 1000.
      Va(1) = 0.15              ! model albedo values
      Va(2) = 0.25 
      Va(3) = 0.35
      VI(1) = 24.0               ! model inertia values
      VI(2) = 35.4
      VI(3) = 52.3
      VI(4) = 77.2
      VI(5) = 114.0 
      VI(6) = 168.4
      VI(7) = 248.6
      VI(8) = 367.0
      VI(9) = 541.9
      VI(10) = 800.0
C the values above  MUST agree with the inputs to  Mellon model runs.
C the values for  VT and  VP may be changed in  MSEAS for  Kieffer models
      ALBDEF  = .25             ! default albedo for night observations
      SCALEHM = 10.8            ! mean scale height in km  (MARS p 31.6=p 858.1)
      PMEAN   = 689.7           ! mean annual pressure at 0 km,  Pascal
C  Derived from  VL mean pressure values from  Tillman et al and elevations 
C  relative to the geoid as listed in  MARS p 32.7,
C   average of the 2  VL-1 values, this averaged with  VL-2.  ln=6.5363

      RETURN
        END
