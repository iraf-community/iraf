      subroutine subbak ( w, ipivot, nrow, ncol, last, x )
c  carries out backsubstitution for current block.
c
c parameters
c    w, ipivot, nrow, ncol, last  are as on return from factrb.
c    x(1),...,x(ncol)  contains, on input, the right side for the
c	     equations in this block after backsubstitution has been
c	     carried up to but not including equation ipivot(last).
c	     means that x(j) contains the right side of equation ipi-
c	     vot(j) as modified during elimination, j=1,...,last, while
c	     for j .gt. last, x(j) is already a component of the solut-
c	     ion vector.
c    x(1),...,x(ncol) contains, on output, the components of the solut-
c	     ion corresponding to the present block.
c
      integer nrow, ncol
      integer ipivot(nrow),last,  ip,j,k,kp1
      real w(nrow,ncol),x(ncol), sum
      k = last
      ip = ipivot(k)
      sum = 0.
      if (k .eq. ncol)			go to 4
      kp1 = k+1
    2	 do 3 j=kp1,ncol
    3	    sum = w(ip,j)*x(j) + sum
    4	 x(k) = (x(k) - sum)/w(ip,k)
	 if (k .eq. 1)			return
	 kp1 = k
	 k = k-1
	 ip = ipivot(k)
	 sum = 0.
					go to 2
      end
