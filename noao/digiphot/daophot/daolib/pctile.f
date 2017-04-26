c function pctile (datum, n, npct)
c
c This is a modification of a quick-sorting algorithm, which is intended
c to take in a vector of numbers, and return the value of the npct-th
c element in that vector:
c
c    dataum 			   input vector
c    n			           number of elements in dataum
c    npct			   npct-th element
c    pctile			   output value of function
c
c
c The array datum contains randomly ordered data
c
c
      real function pctile (datum, n, npct)
c
      implicit none
      integer n, npct
      real datum(1)
      integer min0, max0
      real dkey
      integer lo, hi, limlo, limhi
c
c Initialize the pointers.
c
      npct = max0 (1, min0 (n,npct))
      limlo = 1
      limhi = n
c
c Compare all elements in the sub-vector between limlo and limhi with
c the current key datum.
c
  100 dkey = datum (limlo)
      lo = limlo
      hi = limhi
c
c If lo equals hi, we have tested all the elements in the current search
c interval.
c
  101 continue
      if (lo .eq. hi) go to 200
      if (datum(hi) .le. dkey) go to 109
c
c The pointer hi is to be left pointing at a datum SMALLER than the
c key, which is intended to be overwritten.
c
      hi = hi - 1
c
      goto 101
  109 datum(lo) = datum(hi)
      lo = lo + 1
  110 continue
      if (lo .eq. hi) goto 200
      if (datum(lo) .ge. dkey) go to 119
      lo = lo + 1
c
      goto 110
  119 datum(hi) = datum(lo)
c
c The pointer LO is to be left pointing at a datum LARGER than the
c key, which is intended to be overwritten.
c
      hi = hi - 1
c
      go to 101
c
c lo and hi are equal, and point at a value which is intended to
c be overwritten.  Since all values below this point are less than
c the key and all values above this point are greater than the key,
c this is where we stick the key back into the vector.
c
  200 continue
c
c At this point in the subroutine, all data between limlo and lo-1,
c inclusive, are less than datum (lo), and all data between lo+1 and
c limhi are larger than dataum(lo).  If lo = npct, then datum(lo) is
c the value we are looking for.  If npct < lo, then we want to sort the
c values of datum from limlo to lo-1, inclusive, whereas if npct > lo,
c then we want to sort the values of datum from lo+1 to limhi,
c inclusive.
c
      datum(lo) = dkey
      if (npct - lo) 300, 900, 400
  300 limhi = lo - 1
      go to 100
  400 limlo = lo + 1
      go to 100
  900 pctile = datum(lo)
      return
      end
