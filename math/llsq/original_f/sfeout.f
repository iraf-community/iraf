      subroutine mfeout (a,mda,m,n,names,mode)
c     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12
c     to appear in 'solving least squares problems', prentice-hall, 1974
c	   subroutine for matrix output with labeling.
c
c     a( )	   matrix to be output
c		   mda	   first dimension of a array
c		   m	     no. of rows in a matrix
c		   n	     no. of cols in a matrix
c     names()	   array of names.  if names(1) = 1h , the rest
c		   of the names() array will be ignored.
c     mode	   =1	for   4p8f15.0	format	for v matrix.
c		   =2	for   8e15.8  format  for candidate solutions.
c
      dimension    a(mda,01)
      integer names(m),ihead(2)
      logical	notblk
      data  maxcol/8/, iblank/1h /,ihead(1)/4h col/,ihead(2)/4hsoln/
c
      notblk=names(1).ne.iblank
      if (m.le.0.or.n.le.0) return
c
      if (mode.eq.2) go to 10
      write (6,70)
      go to 20
   10 write (6,80)
   20 continue
c
      nblock=n/maxcol
      last=n-nblock*maxcol
      ncol=maxcol
      j1=1
c
c			     main loop starts here
c
   30 if (nblock.gt.0) go to 40
      if (last.le.0) return
      ncol=last
      last=0
c
   40 j2=j1+ncol-1
      write (6,90) (ihead(mode),j,j=j1,j2)
c
	   do 60 i=1,m
	   name=iblank
	   if (notblk) name=names(i)
c
	   if (mode.eq.2) go to 50
	   write (6,100) i,name,(a(i,j),j=j1,j2)
	   go to 60
   50	   write (6,110) i,name,(a(i,j),j=j1,j2)
   60	   continue
c
      j1=j1+maxcol
      nblock=nblock-1
      go to 30
c
   70 format (45h0v-matrix of the singular value decomposition,
     * 8h of a*d./47h (elements of v scaled up by a factor of 10**4))
   80 format (35h0sequence of candidate solutions, x)
   90 format (1h0,11x,8(6x,a4,i4,1x)/1x)
  100 format (1x,i3,1x,a6,1x,4p8f15.0)
  110 format (1x,i3,1x,a6,1x,8e15.8)
      end
