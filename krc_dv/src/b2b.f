        SUBROUTINE B2B (RA,RB,N)
C_TITL  B2B  Byte-to-byte array move or fill
      IMPLICIT NONE
C_ARGS
        BYTE RA(*)  ! [In] Source array or constant
C       BYTE, INTENT(IN) :: RA
        BYTE RB(*)  ! [Out] Destination array
C       BYTE, INTENT(out) :: RB
        INTEGER N   ! [In]  Number of items to move:
C       INTEGER, INTENT(IN) :: N  
C                            If  N>0, copies the source array   
C                            If  N<0, replicates the first item in  RA
C                            Into  RB  |N| times.  I.e., fill  RB
C                             with a constant value.
C_HIST  95JAN04  Hugh_h_Kieffer  USGS revise to structured code
C 2014feb26 HK Use implicit none.  Modern argument specification as comments
C 2014apr27 HK untabify, And use * rather than 1 for argument dimensions
C_END
        BYTE RA1
        INTEGER*4 I,III

        IF (N.GT.0) THEN
          DO I=1,N
            RB(I)=RA(I)
          ENDDO
        ELSEIF (N.LT.0) THEN
          III=-N
          RA1=RA(1)
          DO I=1,III
            RB(I)=RA1
          ENDDO
        ENDIF
        
        RETURN
        END
