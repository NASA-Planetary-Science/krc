C_TITLE   ROTMDP   General 3-dimension rotation matrix geometry package. Double
C_DESCRIPTION  
Contains:  MEQUAL  MPROD3  ROTAX  ROTCOL  ROTDIA  ROTEST  ROTEXM  ROTEXV  
C  ROTMAT  ROTORB  ROTRIP  ROTROW  ROTSHO  ROTV  ROTVEC  ROTZXM  TRANS3  VROTV 
C Argument order is: input _SPACE_ output,
C Angular arguments are in radians; except ROTMAT  ROTORB  ROTRIP use degrees
C Matrices are assumed stored as:  AA(I,J) = AA(row,column) = A(I+3(J-1)).
C   Commonly treat matrices as a 9-element vector
C Output argument may overwrite input argument for routine TRANS3.
C
C  AA,BB,CC and  A,B,C are 3X3 matrices;  U,V are 3-vectors;
C  R, are argument scalars;  E,F,G are internal scalars.
CC ** This is a double precision version made from rotmsp by
CC ** makeing the following changes throughout:
CC **  REAL*4  >  REAL*8
CC **  ASIN, ATAN2  >  DASIN, DTAN2
CC **  E0  >  D0  
C_Calls  VADDDP8 package
C_HIST  85apr17  Hugh_H._Kieffer  U.S.Geological_Survey
C 91apr17  HK reorder, use  ENDDO
C 91mar04  HK include  ROTEX_ routines
C 2013jun20 HK Include  ROTV.  Rename ROTATE to ROTVEC to avoid NumRec conflict
C 2013jun30 HK Use  IMPLICIT NONE  in  each routine
C 2014mar10 HK Make  REAL*8  version. All routines name the same as Single Prec
CC ** This is a double precision version made from rotmsp by
CC ** makeing the following changes throughout:
CC **  REAL*4  >  REAL*8
CC **  ASIN, ATAN2  >  DASIN, DTAN2
CC **  E0  >  D0  
C 2014jun06 HK Put routines in alphabetical order and add list of them
C 2016sep26 HK Untabify, adjust spacing and edit some comments. No algor. changes
C_Pause


      SUBROUTINE MEQUAL (B, A)        ! [A] = [B]
C_Title  MEQUAL  Equate one 3x3 matrix to another.
      IMPLICIT NONE
C_Arguments
      REAL*8 B(9)     ![I] Input matrix.
      REAL*8 A(9)     ![O] Output matrix.
C_Keys  
C_Description  Simple copy of inpout array.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I
      DO I=1,9
        A(I)=B(I)
      ENDDO
      RETURN
      END


      SUBROUTINE MPROD3 (BB,CC, AA)   ! [AA] = [BB] * [CC]
C_Title  MPROD3  Matrix product (hard-coded for size=3).
      IMPLICIT NONE
C_Arguments
      REAL*8 BB(3,3)  ![I] First matrix of product.
      REAL*8 CC(3,3)  ![I] Second matrix of product.
      REAL*8 AA(3,3)  ![O] Resulting matrix
C_Keys  
C_Desc  Straight-forward multiply and sum of elements.
C coded for size=3).  
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I,J
      DO I=1,3
        DO J=1,3
          AA(I,J)=BB(I,1)*CC(1,J)+BB(I,2)*CC(2,J)+BB(I,3)*CC(3,J)
          ENDDO
        ENDDO
      RETURN
      END


      SUBROUTINE ROTAX  (N,R, B)      ! [B] = [R*|N|]*[B]
C_Title  ROTAX  Change rotation matrix to include additional rotation 'R'
      IMPLICIT NONE
C_Arguments
      INTEGER N       ![I] Axis about which to rotate. 1=X,2=Y,3=Z
      REAL*8 R        ![I] Size of rotation, in radians.
      REAL*8 B(9)     ![B] Rotation matrix (treated internally as 9-vector).
C_Keys  
C_Desc  
C Multiply appropriate elements of input rotation matrix by the
c sine and cosine of additional angle.   e.g. If start with matrix  AB 
C (from  B to  A), output will be  A'B, where  A' is reached from  A by 
C rotation of angle  R around axis  N of the  A system.
C_Calls 0       
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I,I2,I3
      REAL*8 C,S,G
      C = COS(R)
      S = SIN(R)
      I2=MOD(N,3)
      I3=MOD(N+1,3)
      DO I=1,7,3
        G=B(I+I2)
        B(I+I2)=C*G       +S*B(I+I3)
        B(I+I3)=C*B(I+I3) -S*G
      ENDDO
      RETURN
      END


      SUBROUTINE ROTCOL (A,N, V)      ! Extract N'th column from [A]
C_Title  ROTROW  Extract  N'th column from a 3x3 matrix.
      IMPLICIT NONE
C_Arguments
      REAL*8 A(9)     ![I] Rotation (3x3) matrix.
      INTEGER N       ![I] Column to extract. 1 to 3
      REAL*8  V(3)    ![O] Extracted vector
C_Keys  
C_Description  Just move the 3 elements of the column into a vector.
C_Calls 0
C_History  2013jul10  H.Kieffer Original version
C_Pause 
      INTEGER I ! index of last element of vector

      I=3*N               ! index of last element in desired column
      V(1) = A(I-2)
      V(2) = A(I-1)
      V(3) = A(I)
      RETURN
      END

      SUBROUTINE ROTDIA (R, B)        ! [B] = R * [IDENTITY]
C_Title  ROTDIA  Form diagonal matrix of magnitude  R.
      IMPLICIT NONE
C_Arguments
      REAL*8 R    ![I] Magnitude of matrix.
      REAL*8 B(9) ![O] Diagonal 3x3 matrix (treated internally as !9-vector)
C_Keys  
C_Desc Zero the matrix, replace diagonl elements with  R.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I
      DO I=1,9
            B(I)=0.D0
            ENDDO
      DO I=1,9,4
            B(I)=R
            ENDDO
      RETURN
      END


      SUBROUTINE ROTEST (BB, DER,DEV) ! Tests deviation from a rotation matrix
C_Titl  ROTEST  Tests deviation of matrix from a rotation matrix
      IMPLICIT NONE
C_Arguments
      REAL*8 BB(9)          ! IN. Array to be tested
      REAL*8 DER  ! out   Deviation of determinant of matrix from 1.
      REAL*8 DEV  ! out.  Max. deviation of mag.^2 of rows and columns from 1.
C Desc
C Tests magnitude of each column and row, and the determinant (to insure
C handedness)
C_Use
C    CALL ROTEST(BB, DER,DEV)
C  IF (DEV.GT.TOL1  .OR.  DER.GT.TOL2) then "is not a rotation matrix"
C tol1 and tol2 should be generously larger than the arithmatic precision
C  e.g., 1.E-6 for single and 1.e-12 for double. 
C_Hist 2013jul11 Hugh Kieffer  Derive from rotest.pro
C_End
      REAL*8 R1,R2,R3,C1,C2,C3,YA,YB
      REAL*8 ONE /1.d0/             ! which precision to use

      R1=BB(1)**2+BB(4)**2+BB(7)**2 ! square of magnitude of vector
      R2=BB(2)**2+BB(5)**2+BB(8)**2 ! each row
      R3=BB(3)**2+BB(6)**2+BB(9)**2
      
      C1=BB(1)**2+BB(2)**2+BB(3)**2
      C2=BB(4)**2+BB(5)**2+BB(6)**2 ! each column
      C3=BB(7)**2+BB(8)**2+BB(9)**2

      YA=DMIN1(R1,R2,R3,C1,C2,C3) ! smallest
      YB=DMAX1(R1,R2,R3,C1,C2,C3) ! largest 
      
      R1=ONE-YA             ! smallest relative to 1
      R2=YB-ONE             ! largest relative to one
      DEV=DMAX1(R1,R2)            ! maximum deviation from perfect

C compute determinant    comments are normal matrix notation
      R3=BB(1)*(BB(5)*BB(9)-BB(8)*BB(6)) !M(1,1)*(M(2,2)*M(3,3)-M(2,3)*M(3,2))
     &    -BB(4)*(BB(2)*BB(9)-BB(8)*BB(3)) !M(1,2)*(M(2,1)*M(3,3)-M(2,3)*M(3,1))
     &    +BB(7)*(BB(2)*BB(6)-BB(5)*BB(3)) !M(1,3)*(M(2,1)*M(3,2)-M(2,2)*M(3,1))
      DER=DABS(R3-ONE)      ! deviation from perfect

C The above determinant test will pass the degenerate case of a matrix with all 
C elements equal sqrt(1/3)
      RETURN
      END


      SUBROUTINE ROTEXM (IX,IZ,U, V)  ! V= U: old axis IX->new X, old IZ ->new Z 
C_Title  ROTEXM  Modify 9 rotation matrix to new system with axes interchanged
      IMPLICIT NONE
C_Arguments
      INTEGER IX      ![i] axis in old system along which new x-axis falls;
C                   1=X  2=Y  3=Z, minus for negative axis.
      INTEGER IZ      ![i] axis in old system along which new z-axis falls.
      REAL*8 U(9)     ![i] original rotation matrix.
      REAL*8 V(9)     ![o] rotated matrix, must not be same array as  U.
C_Keys
C_Description  Determines direction and sign of new  Y axis, then 
C exchanges rows (with possible change of sign) to generate new matrix.
C_Calls 0
C_Lims does not check for valid values of  IX &  IZ, 
C_History       91jul29  Hugh_H_Kieffer  USGS_flagstaff original version
C_Pause
      INTEGER I,IY,J,K,NZ
      INTEGER NEW(3)  ! will contain  IX,IY,IZ.
      INTEGER NEA(3)  ! will contain absolute value of  NEW.

      NEW(1) = IX
      NEW(3) = IZ
      DO I=1,3,2
        NEA(I) = IABS(NEW(I))
CC      IF (NEA(I).LT.1 .OR. NEA(I).GT.3) GOTO 81  ! check for valid values
        ENDDO
CC      IF (NEA(1).EQ.NEA(2) GOTO 82          ! no axes may be same.

C find right-handed  Y axis.   there are 6*4=24 possibile right-handed systems
C sum of 3 axes must be 6; this allows easy determination of which axis is  Y
C then, 3 independant binary tests for sign of  Y axis
      NEA(2) = 6-NEA(1)-NEA(3)
      IY = NEA(2)
      NZ = MOD(NEA(3),3)        ! get  Z below  X
      IF (NEA(1).NE.NZ+1) IY=-IY  ! if  X follows  Z, then  Y must follow  X
      IF (IX.LT.0) IY=-IY       ! but reverse if  X or  Z is along neg axis.
      IF (IZ.LT.0) IY=-IY
      NEW(2) = IY

      DO I=1,3
        K = NEA(I)
        IF (NEW(I).GT.0) THEN
          DO J=0,2
            V(I+3*J) =  U(K+3*J)
            ENDDO
        ELSE
          DO J=0,2
            V(I+3*J) = -U(K+3*J)
            ENDDO
        ENDIF
        ENDDO
      RETURN
      END


      SUBROUTINE ROTEXV (IX,IZ,V1, V2) ! V2 = V1 with X=old IX and Z=old IZ
C_Title  ROTEXV  Rotate a vector to system with axes interchanged
      IMPLICIT NONE
C_Arguments
      INTEGER IX      ![i] axis in old system along which new x-axis falls;
C                   1=X  2=Y  3=Z, minus for negative axis.
      INTEGER IZ      ![i] axis in old system along which new z-axis falls.
      REAL*8 V1(3)    ![i] original vector.
      REAL*8 V2(3)    ![o] rotated vector, must not be same array as  V1.
C_Keys
C_Description  checks for valid values of  IX &  IZ, determines proper 
C exchanges of coordinates, and moves to output vector.
C_Calls 0
C_lims  Does not check for valid input values.
C_History       91jul29  Hugh_H_Kieffer USGS_Flagstaff original version
C_Pause
      INTEGER IY,KX,KY,KZ,NZ

      KX = IABS(IX)
      KZ = IABS(IZ)

C check for validity.
CC      IF (KX.LT.1 .OR. KZ.LT.1 .OR. KX.GT.3 .OR. KZ.GT.3) GOTO 81
CC      IF (KX.EQ.KZ) GOTO 82

C find right-handed  Y axis. there are 6*4=24 possibile right-handed systems
C sum of 3 axes must be 6; this allows easy determination of which axis is  Y
C then, 3 independant binary tests for sign of  Y axis
      KY = 6-KX-KZ
      IY = KY
      NZ = MOD(KZ,3)        ! get  Z below  X
      IF (KX.NE.NZ+1) IY=-IY  ! if  X follows  Z, then  Y must follow  X
      IF (IX.LT.0) IY=-IY     ! but reverse if  X or  Z  IS along neg axis.
      IF (IZ.LT.0) IY=-IY

      IF (IX.GT.0) THEN
        V2(1) =  V1(KX)
      ELSE
        V2(1) = -V1(KX)
      ENDIF
      IF (IY.GT.0) THEN
        V2(2) =  V1(KY)
      ELSE
        V2(2) = -V1(KY)
      ENDIF
      IF (IZ.GT.0) THEN
        V2(3) =  V1(KZ)
      ELSE
        V2(3) = -V1(KZ)
      ENDIF

      RETURN
      END


      SUBROUTINE ROTMAT (V, A) ! A= rotation matrix for V=(RA, DEC, TWIST) degrees
C_Title  ROTMAT  Derive rotation matrix from pointing triple.
      IMPLICIT NONE
C_Arguments
      REAL*8 V(3)     ![I] Pointing triple  (RA, DEC, TWIST) in degrees.
      REAL*8 A(9)     ![O] Corresponding 3x3 rotation matrix.
C_Keys  VECTOR  ROTATION  MATRIX
C_Desc  Begins with a diagonal matrix, then does two  Euler rotations to the
C pointing vector
C_Calls ROTDIA ROTAX
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      REAL*8 RPD
      PARAMETER (RPD=.01745329251994329577D0) ! RADIANS PER DEGREE

      CALL ROTDIA (1.D0, A)
      CALL ROTAX  (3,RPD*(V(1)+90.), A)
      CALL ROTAX  (1,RPD*(90.-V(2)), A)
      IF (V(3) .NE. 0.D0) CALL ROTAX (3,RPD*(V(3)), A)
      RETURN
      END


      SUBROUTINE ROTORB (ODE,CLIN,ARGP, A, LRAD) ! A= rot.mat, in= deg unless LRAD 
C_Titl  ROTORB  Construct rotation matrix from classic orbital elements 
      IMPLICIT NONE
C_Args
      REAL*8 ODE      ! in. node of orbit <degrees>
      REAL*8 CLIN     ! in. inclination of orbit <degrees>
      REAL*8 ARGP     ! in. argument of periapsis <degrees>
      REAL*8 A(9)     ! out. rotation matrix, from orbital to reference.
      Logical Lrad    ! in. If true, then input is in radians
C_Desc
C does 3  Euler rotations.   orbital elements input must be specified in the
C   cordinate system to be the "to" orientation of the rotation matrix.
C_Call  ROTDIA  ROTAX
C_Hist  87oct11  Hugh_H_Kieffer  U.S.G.S._Flagstaff
C 2013jul11 HK Add  LRAD argument
C_Pause
      REAL*8 RPD,FAC
      PARAMETER (RPD=.01745329251994329577D0) ! radians per degree

      FAC=RPD
      IF (LRAD) FAC=1.0D0       ! already in radians

      CALL ROTDIA (1.D0, A)     ! initialize as identity matrix
C the 3  Euler rotations required are:
CC A = (-node)Z * (-inclination)X * (-argument of periapsis)Z
      CALL ROTAX (3, -ARGP*FAC, A)
      CALL ROTAX (1, -CLIN*FAC, A)
      CALL ROTAX (3, -ODE*FAC, A)
      RETURN
      END


      SUBROUTINE ROTRIP (A, V)        ! V= (RA, Dec, twist) for input of rotMat A
C_Title  ROTRIP  Converts rotation matrix to pointing triple.
      IMPLICIT NONE
C_Arguments
      REAL*8 A(9)     ![I] 3x3 Rotation matrix.
      REAL*8 V(3)     ![O] Pointing triple in astronomic convention, 
                  !    in degrees.
C            (1) is Right Ascension  (2) is Declination  (3) is Twist.
C_Keys  
C_Desc  Takes trig functions of combinations of elements of the matrix, 
C and scales them to degrees.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      REAL*8 DPR
      PARAMETER (DPR=57.2957795130823208768D0)  ! DEGREES PER RADIAN

      V(1) = DPR * ATAN2 (A(6),A(3))  ! right ascension
      V(2) = DPR * ASIN  (A(9))       ! declination
      V(3) = DPR * ATAN2 (A(7),A(8))  ! twist
      RETURN
      END

      SUBROUTINE ROTROW (A,N, V)      ! Extract N'th row from [A]
C_Title  ROTROW  Extract  N'th row from a 3x3 matrix.
      IMPLICIT NONE
C_Arguments
      REAL*8 A(9)     ![I] Rotation (3x3) matrix.
      INTEGER N       ![I] Row to extract. 1 to 3
      REAL*8  V(3)    ![O] Row extracted.
C_Keys  
C_Description  Just move the 3 elements of the row into a vector.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      V(1) = A(N)
      V(2) = A(N+3)
      V(3) = A(N+6)
      RETURN
      END


      SUBROUTINE ROTSHO (ID,A,IOP)    ! Print 9-v matrix as 3x3  
C_Title  ROTSHO  Print a 3x3 rotation matrix with ID
      IMPLICIT NONE
C_Args
      CHARACTER*(*) ID      ![I] Character variable identification.
      REAL*8 A(9)           ![I] 3x3 array to print.
      INTEGER IOP           ![I] Logical unit for printer
C_Keys
C_Desc  Uses F format, expecting values up to 1.0
C_Calls 0
C_Hist  2013jun30  Hugh Kieffer Minor revison of MPRINTU
C_End
      INTEGER J
      WRITE(IOP,33) ID,(A(J),A(J+3),A(J+6),J=1,3)
33      FORMAT(' ROTSHO dump of [',A,']',3(/5X,3F10.6))
      RETURN
      END


      SUBROUTINE ROTV   (A,N,R, B)    ! B = [R_N]*A
C_Title  ROTV  Rotate a vector about a Cartesian axis
      IMPLICIT NONE
      REAL*8 A(3) ! in.  original vector
      INTEGER N   ! in.  Integer axis about which to rotate. 1=X, 2=Y, or 3=Z
      REAL*8 R    ! in.  Size of rotation, in radians.
      REAL*8 B(3) ! out. New vector
C_Desc  
C   Component along axis of rotation is not changed.
C   Other two components subject to simple rotation
C_Calls  none
C_Hist 2002dec11 Hugh Kieffer  IDL version
C 2012may31  HK  Fortran version
C_Pause
      INTEGER I,J,K
      REAL*8 C,S,XIN,YIN

      C = COS(R)
      S = SIN(R)
!     Get indices as if rotation was about Z! which reduces this to rotation 
!     effectively in XY plane.  Must keep all MOD positive
      I=  MOD(N+3,3)+1        ! 0-based location of effective X
      J=  MOD(N+1,3)+1        ! " " Y
      K=  MOD(N+2,3)+1        ! rotation axis 
      XIN=A(I)              ! effective X input
      YIN=A(J)              !  " Y "
      B(I)= C*XIN - S*YIN       ! do the rotation, put X in proper location
      B(J)= S*XIN + C*YIN       ! " Y "
      B(K)=A(K)             ! component along rotation axis is unchanged
      RETURN
      END


      SUBROUTINE ROTVEC (B,V, U)      !  U =  B * V
C_Title  ROTVEC  Rotate a vector      !  U =  B *  V
      IMPLICIT NONE
C_Arguments
      REAL*8 B(9)     ![I] 3x3 Rotation matrix.
      REAL*8 V(3)     ![I] Original vector.
      REAL*8 U(3)     ![O] Rotated vector, must not be same array as V.
C_Keys  
C_Desc  straight-forward product of rotation matrix and original vector.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I
      DO I=1,3
        U(I)=B(I)*V(1)+B(I+3)*V(2)+B(I+6)*V(3)
        ENDDO
      RETURN
      END


      SUBROUTINE ROTZXM (Z,X, A)      ! A= rotMat  using Z vector and X-Z plane
C_Title  ROTZXM  Make rotation matrix from vectors along Z-axis, and in X-Z plane
      IMPLICIT NONE
C_Arguments
      REAL*8 Z(3)     ![I] Vector along Z axis (need not be normalized).
      REAL*8 X(3)     ![I] Vector in the X-Z plane (need not be normalized).
      REAL*8 A(9)     ![O] 3x3 Rotation matrix to reference system, from
                  !orientation specified by Z and X.
C_Keys
C_Desc Construct a rotation matrix from system in which  Z and  X are specified
C to a sytem in which the  Z axis is along the input  Z vector, and the  X axis
C is in the plane definded by the input  Z and  X vectors.
C Finds the new  Y axis as the normalized cross product of the input  Z  and X.
C_Lim
C Degenerate if the  X vector is parallel to  Z.
C_Calls VNORM VCROSS
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C 2016sep25 HK Transpose to be matrix for rotating a vector from old to new.
C_Pause

      REAL*8 b(9)    
      CALL VNORM  (Z, B(7))         ! unit vector along Z-axis
      CALL VCROSS (Z,X, B(4))       ! vector in direction of Y-axis
      CALL VNORM  (B(4), B(4))      ! unit vector along Y-axis
      CALL VCROSS (B(4),B(7), B(1)) ! unit vector along X-axis
      CALL TRANS3 (B, A) ! transpose to sense of rotaing vector for old into new
      RETURN
      END


      SUBROUTINE TRANS3 (B, A)        ! A = B_TRANSPOSE for 9v matrices
C_Title  TRANS3  Transpose a 3x3 matrix,  A and  B may be same array.
      IMPLICIT NONE
C_Arguments
      REAL*8 B(9)     ![I] 3x3 matrix.
      REAL*8 A(9)     ![O] transposed matrix.
C_Keys  
C_Description  TRANS3 will transpose a 3x3 matrix; the
C original and the transposed matrix may be the same array.
C this routine coded to allow expansion to any size matrix.
C_Calls 0
C_History       1985apr H.Kieffer argument order revised
C             1985oct15 H.Kieffer standard documentation
C_Pause
      INTEGER I,II,III,IOFF,IT,M,MP,MM
      REAL*8 E
      DATA M,MP,MM/3,4,9/     ! M = matrix size, MP = M+1, MM=M*M 
      DO I=1,MM,MP          ! transfer diagonal
            A(I)=B(I)
            ENDDO
      DO II=2,M             ! do increasing off-diagonal bands
        IOFF=(II-1)*(M-1)
        III=M*(MP-II)
        DO I=II,III,MP
            IT=I+IOFF
            E=B(IT)       ! save one element so transpose
            A(IT)=B(I)      !  may overwrite input array
            A(I)=E
            ENDDO
        ENDDO
      RETURN
      END


      SUBROUTINE VROTV (VIN,AXIS,THETA, ROUT) ! rotate VIN about AXIS by THETA
C_Titl  VROTV  Vector rotation about another vector
      IMPLICIT NONE
C_Args
      REAL*8   VIN  (3)   ! I   Vector to be rotated.
      REAL*8   AXIS (3  ) ! I   Axis of the rotation
      REAL*8   THETA      ! I Angle of rotation (radians)
      REAL*8   ROUT (3)   ! O Result of rotating V about AXIS by THETA
C_Calls    VADD  VCROS VCROSS  VFDOT  VEQUAL  VFMAG  VNORM  VSUB   
C_History
C 2003feb04 Hugh Kieffer  Derived IDL version from NAIF vrotv.f
C 2012may31 HK  Derive from IDL version using local .f library
C 2012jun03 HK  Extensive testing. 
C 2013jul24 HK  Move into rotmsp and use vaddsp routines
C_End
      INTEGER I
      REAL*8 AXMAG,Q,CT,ST ! ,  Q2,q3
      REAL*8 RPLANE (3)
      REAL*8 PP     (3) ! ,  p3(3)
      REAL*8 V1     (3)
      REAL*8 V2     (3)
      REAL*8 XX     (3)
 
      CALL VMAG(AXIS, AXMAG) ! length of AXIS, used to normalize it
C
      IF (AXMAG .EQ. 0.) THEN ! if axis has zero length
       CALL VEQUAL (VIN, ROUT)     ! return the input vector
       RETURN
      END IF
C 
      DO I=1,3              ! equiv to VHAT(AXIS,XX)
       XX(I)=AXIS(I)/AXMAG     !  unit vector in the direction of axis
      ENDDO
C
C     Compute the projection of V onto AXIS.  Call it PP.
C      CALL VPROJ ( V, XX, PP )   sub VPROJ ( A, B, PP )
C  t=a/maxa  r=b/maxb || scale= vdot(T,R)*maxa / cdot(R,R)==r^2
C  VSCL (scale,R,PP)  == out=scale*R
      CALL VDOT (VIN,XX,Q)          ! dot product with length of VIN 

      CALL VSCALE(Q,XX, PP)  !PP= q* XX
C
      CALL VSUB (VIN,PP,  V1 ) ! component of V orthogonal to AXIS
C
      CALL VCROSS (XX,V1, V2) ! Rotate V1 by 90 degrees about the AXIS 
C
C     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
C     the AXIS in the plane normal to the axis, call the result RPLANE
C     CALL VLCOM ( C, V1, S, V2, RPLANE )
      CT = DCOS (THETA)
      ST = DSIN (THETA) 
      DO I=1,3
       RPLANE(I)=CT*V1(I) + ST*V2(I)
      ENDDO
C
C     Add the rotated component in the normal plane to AXIS to the
C     projection of V onto AXIS (PP) to obtain R.
C
      CALL VADD (RPLANE,PP, ROUT )

CD      i=6
CD 33   format(a6,'=[',f9.5,',',f9.5,',',f9.5,']')
CD       write(i,33)' V',vin
CD       write(i,33)'axis ',axis
CD       print *,'theta+',theta,c,s
CD       print *,'axmag',axmag
CD       write(i,33)' XX',xx
CD       print *,' Q=',q,q2,q3
CD       write(i,33)' p',pp
CD       write(i,33)'P3',p3
CD       write(i,33)'V1',v1
CD       write(i,33)'V2',v2
CD       write(i,33)'RP',rplane
CD       write(i,33)' out',rout
C
      RETURN
      END
