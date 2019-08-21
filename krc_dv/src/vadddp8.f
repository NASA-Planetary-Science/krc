C Routines:  VADD  VCROSS  VDOT  VEQUAL  VMAG  VNEG  VNORM  
C     here:  VPRF  VPRINT  VSCALE  VSHOW  VSUB  VUNIT 
C  This is the top of a general  3-dimension vector geometry package.
C  There are four such files, single and double precision, 3 and  N dimensions
C This is the double precision, REAL*8, version
C  All routines similiar in all four files.
C    Argument order in all routines is:  INPUT _space_  OUTPUT
C  Output vector may validly be the same location as input vector for all.
C  VADDNDP was considered the primary version,  VPRINT added here
C -----------------------------------------------------------------------
C_Hist  75-----  Hugh_H_Kieffer  VECTSP.FOR.  Revised 1982Jan12 84May24 84Oct26
C   85Apr12  HK  Change argument order from algebraic 
C   85Oct17  HK  Change variable symbols; document to  NIMS standards
C   91dec27  HK  Add  VMAGN; treat  VADDNDP as primary file
C 2013jun16  HK  Modify only comment typos and format
C 2014mar10  HK Make  REAL*8  version.  All routines name the same as Single Prec.
C 2014jun09 HK Add IMPLICIT NONE to all routines. Add  VPRINT 

      SUBROUTINE VADD (U,V, W)  !  W =  U +  V
C_Title  VADD  Add two vectors of dimension 3.   Double precision
      IMPLICIT NONE
C_Args
      REAL*8 U(*)               ! [i] vector to be added.
      REAL*8 V(*)               ! [i] vector to be added.
      REAL*8 W(*)               ! [o] sum of two vectors; may overwrite input.
C_Keys  MATH  VECTOR
C_Desc  Add two vectors of dimension 3 resulting in a third vector.
C_Paus
      INTEGER I

      DO I=1,3
        W(I) = U(I) + V(I)
      ENDDO
      RETURN
      END


      SUBROUTINE VCROSS (U,V, W) !  W =  U X  V
C_Title  VCROSS  Cross product of two vectors of dimension 3
      IMPLICIT NONE
C_Args
      REAL*8  U(*)              ! [i] first vector.
      REAL*8  V(*)              ! [i] second vector.
      REAL*8  W(3)              ! [o] cross product; must not overwrite input.
C_Keys  MATH  VECTOR
C_Desc  Calculates the cross product of two vectors.
C_Hist  90dec27  Hugh Kieffer    see comments in first routine
C 2013jul11 HK complete recode for 3 dimesions
C 2014mar11  HK  use temporary vector in case caller  W in same location  U or  V
C_Paus
      REAL*8  Y(3)              ! temporary
C cross product into temporary    
      Y(1) = U(2)*V(3) - U(3)*V(2)
      Y(2) = U(3)*V(1) - U(1)*V(3)
      Y(3) = U(1)*V(2) - U(2)*V(1)
C move temporary to output      
      W(1) = Y(1)
      W(2) = Y(2)
      W(3) = Y(3)
      RETURN
      END


      SUBROUTINE VDOT (U,V, R)  !  R =  U .  V
C_Title  VDOT  Calculates the dot product of two vectors of dimension 3.
      IMPLICIT NONE
C_Args
      REAL*8  U(*)              ! [i] first vector.
      REAL*8  V(*)              ! [i] second vector.
      REAL*8  R                 ! [o] dot product of two vectors.
C_Keys  MATH  VECTOR
C_Desc  Produces the dot product of two vectors of dimension 3,  R=U.V
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I
      REAL*8 SUM

      SUM = 0.D0
      DO I=1,3
        SUM  = SUM + U(I)*V(I)
      ENDDO
      R = SUM
      RETURN
      END


      SUBROUTINE VEQUAL (V, W)  !  W =  V
C_Title  VEQUAL  Equate one vector of dimension 3 to another.
      IMPLICIT NONE
C_Args
      REAL*8  V(*)              ! [i] initial vector 
      REAL*8  W(*)              ! [o] vector equal to input
C_Keys  MATH  VECTOR
C_Desc  Equates a vector of dimension 3 to another vector.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I
      DO I=1,3
        W(I)=V(I)
      ENDDO
      RETURN
      END


      SUBROUTINE VMAG (V, R)    !  R =  |V|
C_Title VMAG   Get magnitude (length) of a vector of dimension 3.
      IMPLICIT NONE
C_Args
      REAL*8  V(3)              ! [I] Vector to be normalized
      REAL*8  R                 ! [O] Vector length
C_Keys  MATH VECTOR
C_Desc  Finds length of a vector of dimension 3.  R = |V|.
C_Call  0
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I
      R = 0.D0
      DO I=1,3
        R = R + V(I)**2
      ENDDO
      R = DSQRT(R)
      RETURN
      END


      SUBROUTINE VNEG (V, W)    !  W =  -V
C_Title  VNEG  Negate each element of a vector of dimension 3.
      IMPLICIT NONE
C_Args
      REAL*8  V(*)              ! [i] vector to be negated
      REAL*8  W(*)              ! [o] vector of negated elements; 
C                       may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Each element of the input vector is copied with a change of sign.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I

      DO I=1,3
        W(I)=-V(I)
      ENDDO
      RETURN
      END


      SUBROUTINE VNORM (V, W)   !  W =  V/|V|
C_Title  VNORM  Normalize a vector of dimension 3.
      IMPLICIT NONE
C_Args
      REAL*8  V(*)              ! [i] vector to be normalized
      REAL*8  W(*)              ! [o] vector of unit length; may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Normalizes a vector of dimension 3 to unit length.  W=V/|V|.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I
      REAL*8 SUM
      SUM = 0.D0
      DO I=1,3
        SUM = SUM +V(I)**2
      ENDDO
      SUM = 1.D0/DSQRT(SUM)
      DO I=1,3
        W(I) = SUM * V(I)
      ENDDO
      RETURN
      END

      SUBROUTINE VPRF (ID,V,IOP,FMT)
C_Title  VPRF  Print a vector of dimension 3 in user format.
      IMPLICIT NONE
C_Args
      CHARACTER*(*) ID          ! [i] character variable identification
      REAL*8 V(*)               ! [i] vector to print
      INTEGER  IOP              ! [i] logical unit for printer
      CHARACTER*(*) FMT         ![i] format to use for vector. this should be only
C                       the core of the format, e.g.   '8F8.3' 
C_Keys  MATH  VECTOR
C_Desc  Prints a vector of dimension 3 to the logical unit specified
C       and in the format specified (up to 30 characters).
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I
      CHARACTER*60 FMT2
      FMT2 = '(1X,A,''='',(/1X,' // FMT // '))'
      WRITE (IOP,FMT2) ID,(V(I),I=1,3)
      RETURN
      END


      SUBROUTINE VPRINT (ID,V,IOP)
C_Title  VPRINT print a vector of dimension 3. Many significant figures
      IMPLICIT NONE
C_Args
      CHARACTER*(*) ID          ! [i] character variable identification
      REAL*8 V(*)               ! [i] vector to print
      INTEGER  IOP              ! [i] logical unit for printer
C_Keys  MATH  VECTOR
C_Desc  Prints a vector of dimension 3 to the logical unit specified
C_Hist  90dec27  HHKieffer  U.S.G.S.,Flagstaff. see comments in first routine
C_Paus
      INTEGER I

      WRITE(IOP,34) ID,(V(I),I=1,3)
 34   FORMAT('0VPRINTN dump of: ',A,(/5X,3D22.13))
      RETURN
      END


      SUBROUTINE VSCALE (R,V, W) !  W =  R *  V
C_Title  VSCALE  Multiply a vector of dimension 3 by a constant.
      IMPLICIT NONE
C_Args
      REAL*8  R                 ! [i] constant multiplier.
      REAL*8  V(*)              ! [i] vector to be multiplied.
      REAL*8  W(*)              ! [o] product of a vector and a constant; 
C                       may overwrite input vector
C_Keys  MATH  VECTOR
C_Desc  Multiplies a one dimensional array of dimension 3 by a constant  R 
C producing a third one dimensional array w.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I

      DO I=1,3
        W(I)=R*V(I)
      ENDDO
      RETURN
      END


      SUBROUTINE VSHOW (ID,V,IOP)
C_Title  VSHOW  Print a vector of dimension 3 as cartesion and spherical angles
      IMPLICIT NONE
C_Args
      CHARACTER*(*) ID          ! [i] character variable identification
      REAL*8 V(3)               ! [i] vector to print
      INTEGER IOP               ! in. Logical unit. Negative to print col headings
C_Keys  MATH  VECTOR
C_Desc  Prints a vector of dimension 3 to logical unit specified
C_Hist  2013jun16  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I,J
      REAL*8  P      ! Phi = polar angle from Z axis (co-latitude); in radians.
      REAL*8  Q      ! Lambda = azimuthal angle, positive from X axis; in radians.
      REAL*8  R      ! Equatorial radius.
      REAL*8 H,PID2,QD,PD,RAD  ! Internal variables
      PARAMETER (PID2=1.57079632679489662D0) ! PI/2
      CHARACTER*60 FMT
      FMT='(3g13.5,2x,2f8.2,g12.5,1x,A)'

C       CALL COCOCS(V, P,Q,R)   !  colat and  East longitude in radians, length
C copy cococs.f here and convert to REAL*8
      H = V(1)**2 + V(2)**2     ! X-Y Radius squared
      IF (H .EQ. 0.D0) THEN
        Q = 0.D0                ! On polar axis, azimuth indeterminate
      ELSE
        Q = DATAN2 (V(2),V(1))
      ENDIF
      R = DSQRT (H + V(3)**2)   ! Radius
      IF (R .EQ. 0.D0) THEN
        P = 0.D0                ! Radius is zero, polar angle indeterminate
      ELSE
        P = PID2 - DATAN2 (V(3), DSQRT(H) )
      ENDIF
C^^^^^^^^^^^^^^^ end of converted cococs
      RAD=90.D0/PID2            ! a radian in degrees
      QD=Q*RAD                  ! convert longitude to degrees
      PD=90.d0 -P*RAD           ! convert co-lat to latitude degrees
      J=ABS(IOP)                ! logical unit to use
      IF (IOP.LT.0) WRITE(J,*)'C_END  X          Y           Z'
     &     ,'            Lat    E.Lon   Radius     What'
      WRITE (J,FMT) (V(I),I=1,3),QD,PD,R,ID
      RETURN
      END


      SUBROUTINE VSUB (U,V, W)  !  W =  U -  V
C_Title  VSUB   Find difference of two vectors of dimension 3
      IMPLICIT NONE
C_Args
      REAL*8  U(*)              ! [i] vector, initial
      REAL*8  V(*)              ! [i] vector to be subtracted
      REAL*8  W(*)              ! [o] difference of two input vectors; 
C     may overwrite input
C_Keys  MATH  VECTOR
C_Desc  Difference of two vectors of dimension 3.  W=U-V.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_Paus
      INTEGER I

      DO I=1,3
        W(I)=U(I)-V(I)
      ENDDO
      RETURN
      END

      SUBROUTINE VUNIT (IAX, U)
C_Title  VUNIT  Construct unit vector of dimension 3 along one axis 
      IMPLICIT NONE
C_Args
      INTEGER IAX               ! [i] axis along which vector will point.
      REAL*8  U(*)              ! [o] unit vector along axis.
C_Keys  MATH  VECTOR
C_Desc  Creates a unit vector of dimension 3 along a  Cartesian axis.
C     if  IAX is not within  1:N, then a zero vector is returned.
C_Hist  90dec27  Hugh Kieffer.  See comments in first routine
C_End
      INTEGER I

      DO I=1,3
        U(I)=0.D0
      ENDDO
      IF (IAX.GE.1 .AND. IAX.LE.3) U(IAX)=1.D0
      RETURN
      END
