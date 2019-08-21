        SUBROUTINE ALBVAR8 (Q1, ALBV)
C_Titl ALBVAR Compute frost albedo as linear function of insolation
C_Vars
        INCLUDE 'krcc8m.f'
C_Args:
      REAL*8 Q1    ! IN. average (over a day) solar flux (direct and diffuse) 
C   incident on a flat surface
      REAL*8 ALBV  ! OUT.  effective hemispheric albedo of frost deposit
C       ?? should phase function be here??
C 2005dec28 HK Change to use of IMPLICIT NONE. But here no change needed
C 2014mar10  HK Make  REAL*8  version.
C 2014may08 HK untabify
C_END

        ALBV = AF1 + AF2*Q1
        RETURN
        END

