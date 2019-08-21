      integer function white1(ss, ww)
C_Titl  WHITE1: reduce each white space to 1 blank
C_args
      character*(*) SS  ! in. string to be treated
      character*(*) WW  ! out. string with no adjacent blanks
C      integer white1   ! f. out. location of last defined character
C_Desc
C treats tabs as one blank, removes contiguous blanks
C stops processing when either input exhausted ot ourput full
C_bugs
C no warning if input was too long to fit in output
C_Hist
C  98may10  Hugh_h_Kieffer  original version
C 2009may16 HK replace CHAR("011) with CHAR(9)
C_end
      logical last              ! prior character was white
      character*1 c             ! current character

      lss=len(ss) ! get defined length of input and output strings
      lww=len(ww)
      
      last = .false.
      j=0                       ! # characters output thus far
      do i=1,lss
         c=ss(i:i)              ! current character
         if (c.eq.' ' .or. c.eq.CHAR(9)) then ! have blank or TAB
            if (.not.last) then ! if last was blank, do nothing
               j=j+1
               ww(j:j)=' ' ! output the one blank separator
               last=.true.
            endif
         else
            j=j+1
            ww(j:j)=c
            last=.false.
         endif
         if (j.eq.lww) goto 9 ! output area full, quit
      enddo
 9    white1=j
      return
      end
      
