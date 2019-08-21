      SUBROUTINE NOWHITE (SSS, N,BUF)
C_TITLE NOWHITE  Remove all white space from character string  Lim 80 characters
C_ARG 
      IMPLICIT NONE
      CHARACTER*(*) SSS         !in.  String to be processed
      INTEGER N                 !out. Length of valid output 
      CHARACTER(LEN=80) BUF     !out. All-printable non-white string
C_DESCRIPTION
C      Removes all white or non-printing characters
C_Hist
C 2017sep30 HK Original version
C_END

      INTEGER I,J,K
      CHARACTER*1  BB       ! one character

      K=LEN(SSS)                  ! total length of input
      IF (K.GT.80) PRINT *, 
     & 'NOWHITE: Will process only first 80 of ',K,SSS
      K=MIN(K,80)               ! number of characters to process
      N=0
      DO I=1,K
        BB=SSS(I:I)               ! single character
        J = ICHAR (BB)
        IF (J.GE.33 .AND. J.LE.126) THEN ! printable non-white
          N=N+1
          BUF(N:N)=BB
        ENDIF
      ENDDO
      
      RETURN
      END
