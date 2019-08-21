      SUBROUTINE dspline(x,y,n,yp1,ypn,y2) ! call once to make @nd derivatives
      INTEGER N     ! in.  Size of input arrays
      REAL*8 x(n)    ! in.  Independent values of tabulated points, in order
      REAL*8 y(n)    ! in.  Dependent values of tabulated points
      REAL*8 yp1,ypn ! in.  First deriv. of the interp. function at end points
                     !      If either is 1.e30 or larger, that end set to 
                     !      natural spline, with zero second derivative.
      REAL*8 y2(n)   !out. 2nd derivative of interp. function at the tab. pointsv
C_end 2013aug27 Hugh Kieffer  Make double precision version of spline
      INTEGER NMAX
      PARAMETER (NMAX=500)      ! largest anticipated value of  N
      INTEGER i,k
      REAL*8 p,qn,sig,un,u(NMAX)
      if (yp1.gt.0.99d30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i)) -(y(i)-y(i-1)) 
     & /(x(i)-x(i-1)) ) / (x(i+1)-x(i-1))  -sig*u(i-1))  /p
11    continue
      if (ypn.gt.0.99d30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 53'3.
