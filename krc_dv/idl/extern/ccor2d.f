      real function ccor2d(xx,yy,nsx,nsy,ni,nj)
C_Title  CCOR2D:  cross-correlation  (Pearson) between  2-D arrays
      implicit none
C_Args
      REAL XX(*), yy(*)         !in. arrays to cross-correlate
CC   REAL*4 xx(NSX,*),YY(NSY,*) ! dimensioned in the calling routine
      integer NSX,NSy           !in. first dimension of arrays
      integer NI,NJ             !in. size of correlation area
      
C_DESC
C computes cross-corRelation coefficent for rectangular areas in aligned arrays
C internally treats arrays as one-dimension;
C   this is to minimixe index calculations.
c_lims
c DOES NOT CHECK FOR index range validity
C_Hist  97nov04 Hugh_Kieffer original version
      integer locx,locy,io,i,j
      real*4 zero /0./
      real*4 x,y,count,sxx,syy,sxy,tx,ty
C      write (*,*) 'nsx,nsy,ni,nj=',nsx,nsy,ni,nj,char(10)
C      write (*,*) 'XX:',xx(1),xx(nsx),xx(ni*nj),char(10)
C      write (*,*)'YY:', yy(1),yy(nsx),yy(ni*nj),char(10)

      count=real(NI*nj)
      if (count.lt.1) return
c find averages
      tx=zero
      ty=zero
      locx=0                    ! offset to start of next line in  YY
      locy=0                    ! " " in  YY
      do j=1,nj                 ! each line
        io=locy-locx            ! loc.  YY relative to that in  XX for this line
        do i=locx+1,locx+NI     ! location of  (I,J) in  XX
          Tx=tX+XX(I)           ! total the elements
          Ty=ty+yy(io+i)
        ENDDO
        locx=locx+nsx           ! increment starting location of line
        locy=locy+nsy
      ENDDO
      tx=tx/count
      ty=ty/count


C find sums of squares (relative to averages)
C uses same index logic as the loop above
      sxX=zero
      syY=zero
      sxy=zero
      locx=0
      locy=0
      do j=1,nj
        io=locy-locx
        do i=locx+1,locx+NI
          x=xx(i)-tx            ! difference from average
          y=yy(io+i)-ty
          SXY=SXY+x*y           ! sum the squares
          SXX=SXX+x**2
          SYY=SYY+y**2
        ENDDO
        locx=locx+nsx
        locy=locy+nsy
      ENDDO
      ccor2d = sxy/SQRT( sXX*sYY )
c      write (*,*) 'result=',ccor2d,char(10)
      RETURN
      END
      
