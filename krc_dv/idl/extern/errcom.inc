C_Titl errcom.inc
	INTEGER*4 MERR
	PARAMETER (MERR=15)       ! number of error counters
	INTEGER*4 	KERR(MERR)	! error counters
	INTEGER*4 IOERR	! logical unit for error messages % 
	INTEGER*4 IDB1	! logical unit for debug print %
	INTEGER*4 IDB2	! logical unit for debug print %
	INTEGER*4 IDB3	! logical unit for debug print %
	INTEGER*4 IDB4	! logical unit for debug print %
C % = must be set in main program
	INTEGER*4 IRET	! error return, from routines that use it.
C			! 0=normal, -=fatal, +=warning
      COMMON/ERRCOM/IOERR,IDB1,IDB2,IDB3,IDB4,IRET,KERR
C
C  KEER assignments;
C tifov4.	1/2	invalid low/high hour
C 		3/4	invalid low/high latitude
C 		5/6	low/high elevation
C 		7/8	low/high opacity
C ertia.f	9/10	observed  T is below/above any model
C elevate.f	11/12	low/high  latitude
C 		13/14	low/high  longitude
C ertia.f	15	no t range amoung models
C............
C  IOERR must be set to a valid logical unit number  (LUN)
