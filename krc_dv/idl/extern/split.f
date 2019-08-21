      SUBROUTINE SPLIT(TT,FR)
C_Titl  SPLIT  BREAKS D.P. NUMBER INTO A D.P.INTEGER AND A D.P. FRACTIONAL PART.
      IMPLICIT NONE
C_Args
      DOUBLE PRECISION TT       ! in. D.P. NUMBER
      DOUBLE PRECISION FR(2)    ! out. 
C            FR(1) CONTAINS INTEGER PART
C            FR(2) CONTAINS FRACTIONAL PART
C
C            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
C            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
C
C_Hist  2006jan21  Code pulled from ssd.jpl.nasa.gov//pub/eph/export/DE405/
C   Original code by Myles Standish of JPL.    Extract routine from testeph.f
C 2006jan21 Hugh_H_Kieffer  Use IMPLICIT NONE, rearrange argument documentation
C 2014may21 HK  remove the  SAVE statement
C_End __________________________________________________________________________

C       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS

      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN

C       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER

      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0

      RETURN

      END
