c subroutine quick (dataum, n, index)
c
c
c A quick-sorting algorithm suggested by the discussion on pages 114-119
c of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
c D.E. Knuth, which was referenced in Don Wells' subroutine QUIK.  This
c is my own attempt at encoding a quicksort-- PBS.
c
c Arguments
c
c datum (input / output) is a vector of dimension n containing randomly 
c        ordered real data upon input.  Upon output the elements of 
c        dataum will be in order of increasing value.
c
c 
c index (output) is an integer vector of dimension n.  Upon return to
c       the calling program the i-th element of index will tell where
c       the i-th element of the sorted vector datum had been before
c       datum was sorted.
c
c
c
c      parameter (maxstack = 28)
c
c Parameters
c
c maxstack is the maximum number of entries the stack can contain.
c         A limiting stack length of 28 restricts this quicksort 

      subroutine  quick (datum, n, index, ier)
c
      implicit none
      integer n, index(n), ier
      real datum(n)
c
      real dkey
      integer stklo(28), stkhi(28), i, lo, hi, nstak, limlo, limhi, ikey
c
c Initialize error code

      ier = 0
c
c Initialize index.
c
      do 50 i = 1, n
   50 index(i) = i
c
c Initialize the pointers.
c
      nstak = 0
      limlo = 1
      limhi = n
c
  100 dkey = datum(limlo)
      ikey = index(limlo)
c
c Compare all elements in the sub-vector between limlo and limhi with
c the current key datum.
c
      lo = limlo
      hi = limhi
  101 continue
c
      if (lo .eq. hi) go to 200
c
      if (datum(hi) .le. dkey) go to 109
      hi = hi - 1
c
c The pointer hi is to be left pointing at a datum smaller than the
c key, which is intended to be overwritten.
c
      go to 101
c
  109 datum(lo) = datum(hi)
      index(lo) = index(hi)
      lo = lo + 1
  110 continue
c
      if (lo .eq. hi) go to 200
c
      if (datum(lo) .ge. dkey) go to 119
c
      lo = lo + 1
      go to 110
c
  119 datum(hi) = datum(lo)
      index(hi) = index(lo)
      hi = hi - 1
c
c The pointer LO is to be left pointing at a datum LARGER than the
c key, which is intended to be overwritten.
c
      go to 101
c
  200 continue
c
c lo and hi are equal, and point at a value which is intended to
c be overwritten.  Since all values below this point are less than
c the key and all values above this point are greater than the key,
c this is where we stick the key back into the vector.
c
      datum(lo) = dkey
      index(lo) = ikey
c
c At this point in the subroutine, all data between limlo and LO-1, 
c inclusive, are less than datum(LO), and all data between LO+1 and 
c limhi are larger than datum(LO).
c
c If both subarrays contain no more than one element, then take the most
c recent interval from the stack (if the stack is empty, we're done).
c If the larger of the two subarrays contains more than one element, and
c if the shorter subarray contains one or no elements, then forget the 
c shorter one and reduce the other subarray.  If the shorter subarray
c contains two or more elements, then place the larger subarray on the
c stack and process the subarray.
c
      if ((limhi - lo) .gt. (lo - limlo)) go to 300
c
c Case 1:  the lower subarray is longer.  If it contains one or no 
c elements then take the most recent interval from the stack and go 
c back and operate on it.
c
      if ((lo - limlo) .le. 1) go to 400
c
c If the upper (shorter) subinterval contains one or no elements, then
c process the lower (longer) one, but if the upper subinterval contains
c more than one element, then place the lower (longer) subinterval on
c the stack and process the upper one.
c
      if ((limhi - lo) .ge. 2) go to 250
c
c Case 1a:  the upper (shorter) subinterval contains no or one elements,
c so we go back and operate on the lower (longer) subinterval.
c
      limhi = lo - 1
      go to 100
c
  250 continue
c
c Case 1b:  the upper (shorter) subinterval contains at least two 
c elements, so we place the lower (longer) subinterval on the stack and
c then go back and operate on the upper subinterval.
c 
      nstak = nstak + 1
      if (nstak .gt. 28) then
          ier = -1
          return
      endif
      stklo(nstak) = limlo
      stkhi(nstak) = lo - 1
      limlo = lo + 1
      go to 100
c
  300 continue
c
c Case 2:  the upper subarray is longer.  If it contains one or no 
c elements then take the most recent interval from the stack and 
c operate on it.
c
      if ((limhi - lo) .le. 1) go to 400
c
c If the lower (shorter) subinterval contains one or no elements, then
c process the upper (longer) one, but if the lower subinterval contains
c more than one element, then place the upper (longer) subinterval on
c the stack and process the lower one.
c
      if ((lo - limlo) .ge. 2) go to 350
c
c Case 2a:  the lower (shorter) subinterval contains no or one elements,
c so we go back and operate on the upper (longer) subinterval.
c
      limlo = lo + 1
      go to 100
c
  350 continue
c
c Case 2b:  the lower (shorter) subinterval contains at least two 
c elements, so we place the upper (longer) subinterval on the stack and
c then go back and operate on the lower subinterval.
c 
      nstak = nstak + 1
      if (nstak .gt. 28) then
          ier = -1
          return
      endif
      stklo(nstak) = lo + 1
      stkhi(nstak) = limhi
      limhi = lo - 1
      go to 100
c
  400 continue
c
c Take the most recent interval from the stack.  If the stack happens 
c to be empty, we are done.
c
      if (nstak .le. 0) return
      limlo = stklo(nstak)
      limhi = stkhi(nstak)
      nstak = nstak - 1
      go to 100
c
      end
