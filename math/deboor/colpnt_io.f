      subroutine colpnt(k,rho)
c  from  * a practical guide to splines *  by c. de boor
c  the	k collocation points for the standard interval (-1,1) are sup-
c  plied here as the zeros of the legendre polynomial of degree k ,
c  provided  k .le. 8 . otherwise, uniformly spaced points are given.
      integer k,  j
      real rho(k),  fkm1o2
      if (k .gt. 8)		       go to 99
				       go to (10,20,30,40,50,60,70,80),k
   10 rho(1) = 0.
				       return
c$    (single/double) dpdata
   20 rho(2) = .57735 02691 89626 d0
      rho(1) = - rho(2)
				       return
   30 rho(3) = .77459 66692 41483 d0
      rho(1) = - rho(3)
      rho(2) = 0.
				       return
   40 rho(3) = .33998 10435 84856 d0
      rho(2) = - rho(3)
      rho(4) = .86113 63115 94053 d0
      rho(1) = - rho(4)
				       return
   50 rho(4) = .53846 93101 05683 d0
      rho(2) = - rho(4)
      rho(5) = .90617 98459 38664 d0
      rho(1) = - rho(5)
      rho(3) = 0.
				       return
   60 rho(4) = .23861 91860 83197 d0
      rho(3) = - rho(4)
      rho(5) = .66120 93864 66265 d0
      rho(2) = - rho(5)
      rho(6) = .93246 95142 03152 d0
      rho(1) = - rho(6)
				       return
   70 rho(5) = .40584 51513 77397 d0
      rho(3) = - rho(5)
      rho(6) = .74153 11855 99394 d0
      rho(2) = - rho(6)
      rho(7) = .94910 79123 42759 d0
      rho(1) = - rho(7)
      rho(4) = 0.
				       return
   80 rho(5) = .18343 46424 95650 d0
      rho(4) = - rho(5)
      rho(6) = .52553 24099 16329 d0
      rho(3) = - rho(6)
      rho(7) = .79666 64774 13627 d0
      rho(2) = - rho(7)
      rho(8) = .96028 98564 97536 d0
      rho(1) = - rho(8)
c$lbl dpdata
				       return
c  if k .gt. 8, use equispaced points, but print warning
   99 print 699,k
  699 format(11h **********/
     *	     49h equispaced collocation points are used since k =,i2,
     *	     19h is greater than 8.)
      fkm1o2 = float(k-1)/2.
      do 100 j=1,k
  100	 rho(j) = float(j-1)/fkm1o2 - 1.
				       return
      end
