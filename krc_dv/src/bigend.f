      INTEGER FUNCTION BIGEND ()
C_Title  BIGEND  Determine if hardware byte order is big or little endian 
C_Arguments  none
C Function  Integer: 1 if big-endian    0 if little endian  
C -1=neither  2=both  These results should never ever occur!
C 
C_Calls  none
C_Desc  Overlaps in memory a 4-byte integer and 4 bytes
C_History 2013feb02 Hugh Kieffer Original version
C_end
C      
      IMPLICIT NONE  
 
      INTEGER*1 B(4)
      INTEGER*4 I
      EQUIVALENCE (I,B(1))      ! overlap B and I

      I=1                       ! set one bit of 32-bit word
      BIGEND=-1                 ! set as undetermined
      IF (B(4) .EQ. 1) BIGEND = 1    ! least-significant byte is the last
      IF (B(1) .EQ. 1) BIGEND = BIGEND+1 ! least-sig. byte is first
C              If both tests true, yields +2 . Virtually impossible

C      write(*,*) B  ! debug test 
      
      RETURN
      END
