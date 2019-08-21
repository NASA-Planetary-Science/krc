      SUBROUTINE STRUMI (PARI,NIN,S1, SSS,LOUT)
C_Titl  STRUMI  Concatonate integers into one string using separator
C            ^^     ^=places that change between routines in this family
      IMPLICIT NONE
C_Vars
      INTEGER PARI (NIN)        ! in. values to transfer
C     ^^^^^^^
      INTEGER NIN               ! in.  number of values
      CHARACTER(LEN=1) S1       ! in. separator to use

      CHARACTER*(*) SSS         ! out. long string
      INTEGER LOUT              ! out. length of output string
C_Lim  Integers must be no more than 10 characters
C_Hist 2017mar30  Hugh  Kieffer  Same function as  IDL  STRUM(  /join)
C_End

      INTEGER I,J,K             ! local use
      CHARACTER(LEN=10) BUFF,BUF2 ! local strings
C                   ^^

D      PRINT*,'Pari=',PARI
      SSS=S1//S1                ! initial pair of separators
      K=2                       ! non-blank length
      DO J=1,NIN                ! each input item
        WRITE(BUFF,'(i10)') PARI(J)
C                   ^^^^^
D        PRINT *,'buff=',BUFF
        BUF2=ADJUSTL(BUFF)      ! shift left past any leading spaces
        I=LEN_TRIM(BUF2)        ! index of last non-blank character 
        SSS=SSS(1:K)//BUF2(1:I)//S1 ! append to output string
        K=K+I+1                 ! current expected length
D        PRINT *,J,I,K,SSS(1:K)
      ENDDO
      SSS=SSS(1:K)//S1          ! final separator to make a pair
      LOUT=K+1                  ! number of defined characters
      RETURN
      END
