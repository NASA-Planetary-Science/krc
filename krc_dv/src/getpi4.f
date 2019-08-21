      SUBROUTINE GETPI4 (CCC,RR,SSS,NN)
C_titl  GETPR8  Get modify Real*8 double precision values in array from keyboard
C           ^^     ^=places that change between routines in this family
      IMPLICIT NONE
      CHARACTER*(*) CCC         ! in.  Title for the full list of items
      INTEGER*4 RR(NN)          ! both.  Vector of values to modify
C     ^^^^^^^
      CHARACTER(len=*) SSS(NN)  ! in.  Description of each item 
      INTEGER NN                ! in.  Number of items in list
C_Hist 2017apr01  Hugh  Kieffer  Derive from  GETPR8
C_End

      INTEGER I
      INTEGER*4 VV
C     ^^^^^^^

 10   WRITE(6,*) 'Input item # and its new value:  -2 x for quit'
      WRITE(6,*)' -1 x for current list, x>=2 for help'     
      WRITE(6,*) CCC

 20   WRITE(6,*),'Index value description'
      DO I=1,NN       
        WRITE(6,33),I,RR(I),SSS(I)
      ENDDO
 33   FORMAT(I3, I12,2X,A)
C                ^^^

 30   WRITE (6,*)'Enter index and value '
      READ(5,*) I,VV
      IF (I.LE.-2) GOTO 9         ! done
      IF (I.LE.0) THEN 
        IF (VV.GE.2) GOTO 10    ! display current values
        GOTO 20
      ENDIF
      IF (I.GT.NN) then
        WRITE (6,*)' Index > allowed=',NN
        GOTO 10                 ! get another value
      ENDIF
      RR(I)=VV                  ! save one value
      GOTO 30

 9    RETURN
      END
