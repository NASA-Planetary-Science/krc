C_Titl  filc8m.f  Common /FILCOM/ for file names and other strings
        CHARACTER*80 FINPUT  ! input file
     & ,FRUN     ! uniq part of output name, comes from print-file prompt
     & ,FOUT     ! spooled printer file
     & ,FDISK    ! type 5x data output
     & ,FDIRA    ! direct access data output
     & ,FFAR     ! far-field temperatures input (surface)
     & ,FFATM    ! far-field atmosphere temperatures
     & ,FVALB    ! Seasonal Albedo
     & ,FVTAU    ! Seasonal opacity
     & ,FZONE    ! Depth zone table
     & ,FTOUT    ! for  TOUT at one latitude
        CHARACTER*20 TITONE     ! title for each one-point line
        CHARACTER*12 VERSIN     ! version number
        COMMON /FILCOM/ FINPUT,FRUN,FOUT,FDISK,FDIRA,FFAR,FFATM 
     & ,FVALB,FVTAU,FZONE,FTOUT,TITONE,VERSIN
C_Hist   85oct14  Hugh_Kieffer  97feb12 increase string length
C 2002mar01 HK increase string length from 60
C 2006sep09 HK Add FVALB,FVTAU
C 2009may10 HK Add TITONE
C 2013jan30 HK Add VERSIN
C 2016feb11 HK Add FZONE,  FMOON (later changed to FTOUT) 
C 2016may12 HK Add FDIRA,FFAR  Rename from filc33.f
C 2018feb03 HK Add FRUN      
C 2018oct16 HK add FFATM
C_End___________________________________________________________________________
  
