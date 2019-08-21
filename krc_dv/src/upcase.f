      SUBROUTINE UPCASE (C,N)
C_TITLE UPCASE  Converts all characters to uppercase.
C_ARG
      CHARACTER*(*) C      ![I/O] String to be made all upper_case.
      INTEGER N      ![I] Length of string to convert.
C_DESCRIPTION
C      Takes an input string and converts all characters to uppercase.
C  NOTE:  CHAR2UP performs same function, with separate in and out strings.
C_Hist
C 2016mar22 HK untabify
C_END
      DO I=1,N
        J = ICHAR (C(I:I))
        IF (J.GE.97 .AND. J.LE.122) THEN ! is in range of lowercase
            J = J-32            ! offset from UPCASEase to lowercase
            C(I:I) = CHAR(J)      ! convert back to character
        ENDIF
        ENDDO
      RETURN
      END
