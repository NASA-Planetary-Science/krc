      FUNCTION julday(mm,id,iyyy)
C_Titl  JULDAY Compute intger Julian day from month, day,year
C_Hist 2012nov22 Hugh Kieffer  Modified from NumRec to eliminate PAUSE
C_End

      INTEGER julday,id,iyyy,mm,IGREG
      PARAMETER (IGREG=15+31*(10+12*1582))
      INTEGER ja,jm,jy
      jy=iyyy
      if (jy.eq.0) then
        write(*,*) 'julday ERROR: there is no year zero: Assume 1'
        jy=1
      endif
      if (jy.lt.0) jy=jy+1
      if (mm.gt.2) then
        jm=mm+1
      else
        jy=jy-1
        jm=mm+13
      endif
      julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
      if (id+31*(mm+12*iyyy).ge.IGREG) then
        ja=int(0.01*jy)
        julday=julday+2-ja+int(0.25*ja)
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 53'3.
