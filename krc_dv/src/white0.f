      INTEGER FUNCTION WHITE0(SS, WW)
C_Titl  WHITE0: remove all white space
      IMPLICIT NONE
C_Args
      CHARACTER*(*) SS  ! in. string to be treated
      CHARACTER*(*) WW  ! out. string with no blanks or tabs
C      integer white0   ! f. out. length of output string
C_Desc
C transfers all characters except blank or tab
C stops processing when either input exhausted or output full
C_Bugs
C no warning if input was too long to fit in output
C_Hist 2011aug11  Hugh kieffer  Original version. Derive from white1.f
C_end

      INTEGER I,J,LSS,LWW
      CHARACTER*1 C             ! current character

      LSS=LEN(SS) ! get defined length of input and output strings
      LWW=LEN(WW)
      WW=''                     ! fill output as null
      J=0                       ! # characters output thus far
      DO I=1,LSS
         C=SS(I:I)              ! current character
c         print *,'C>',c,'<'
         IF (C.NE.' ' .AND. C.NE.CHAR(9)) THEN ! was not blank or tab
            J=J+1               ! increment output byte
c            print *,j
            WW(J:J)=C           ! tranfer character to output
         ENDIF
         IF (J.EQ.LWW) GOTO 9   ! output area full, quit
      ENDDO
 9    WHITE0=J
      RETURN
      END

