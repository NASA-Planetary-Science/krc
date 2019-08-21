      SUBROUTINE dsplint(xa,ya,y2a,n,x,y) ! spline interpolation
      INTEGER n     ! in.  Size of input arrays
      REAL*8 xa(n)  ! in.  Independent values of tabulated points, in order
      REAL*8 ya(n)  ! in.  Dependent values of tabulated points
      REAL*8 y2a(n) ! in.  2nd derivative of interp. function at the tab. points
      REAL*8 x      ! in.  Independent value for interpolation
      REAL*8 y      ! out. function value interpolated at x

C Requires prior call to  DSPLINE with corresponding values of xa, ya and y2n
C 2013aug27 Hugh Kieffer  Make double precision version
      INTEGER k,khi,klo
      REAL*8 a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) then
         print *, 'bad xa input in dsplint'
         stop
      endif
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 53'3.
