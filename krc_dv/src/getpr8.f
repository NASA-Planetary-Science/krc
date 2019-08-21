      SUBROUTINE GETPR8 (CCC,RR,SSS,NN)
C_titl  GETPR8  Get modify Real*8 double precision values in array from keyboard
C           ^^     ^=places that change between routines in this family
      IMPLICIT NONE
      CHARACTER*(*) CCC         !in.  Title for the catagory of items
      REAL*8 RR(NN)             ! both.  Vector of values to modify
C     ^^^^^^
      CHARACTER(len=*) SSS(NN)  !in.  Description of each item 
      INTEGER NN                !in.  Number of items in list
C_Hist 2016sep27  Hugh  Kieffer 
C 2017mar15  HK  Use specific format
C_End

      INTEGER I
      REAL*8 VV
C     ^^^^^^

 10   WRITE(6,*) 'Input item # and its new value:  -2 x for quit'
      WRITE(6,*)' -1 x for current list, x>=2  for help'     
      WRITE(6,*) CCC            ! catagory title

 20   WRITE(6,*),'Index value description'
      DO I=1,NN                 ! list all current values and descriptions
        WRITE(6,33),I,RR(I),SSS(I)
      ENDDO
 33   FORMAT(I3, G12.5,2X,A)
C                ^^^^^       However, G format works for all types

 30   WRITE (6,*)'Enter index and value ' ! prompt for a change
      READ(5,*) I,VV            ! read the index and new value
      IF (I.LE.-2) RETURN         ! done
      IF (I.LE.0) THEN          ! -1 x is asking for current table   and help
        IF (VV.GE.2) GOTO 10    ! display current values
        GOTO 20
      ENDIF
      IF (I.GT.NN) then         ! invalid index
        WRITE (6,*)' Index > allowed=',NN
        GOTO 10                 ! get another value
      ENDIF
      RR(I)=VV                  ! modify one value
      GOTO 30                   ! and ask for another

      END
