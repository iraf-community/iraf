      SUBROUTINE  QUICK (DATUM, N, INDEX)
C
C=======================================================================
C
C A quick-sorting algorithm suggested by the discussion on pages 114-119
C of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
C D.E. Knuth, which was referenced in Don Wells' subroutine QUIK.  This
C is my own attempt at encoding a quicksort-- PBS.
C
C Arguments
C
C DATUM (INPUT/OUTPUT) is a vector of dimension N containing randomly 
C        ordered real data upon input.  Upon output the elements of 
C        DATUM will be in order of increasing value.
C
C 
C INDEX (OUTPUT) is an integer vector of dimension N.  Upon return to
C       the calling program the i-th element of INDEX will tell where
C       the i-th element of the sorted vector DATUM had been BEFORE 
C       DATUM was sorted.
C
C=======================================================================
C
C      PARAMETER (MAXSTAK=14)
C
C Parameter
C
C MAXSTK is the maximum number of entries the stack can contain.
C         A limiting stack length of 14 restricts this quicksort 
C         subroutine to vectors of maximum length of order 32,768 
C         (= 2**15).
      REAL*4 DATUM(N)
      INTEGER*4 INDEX(N), STKLO(14), STKHI(14), HI
C
C Initialize INDEX.
C
      DO 50 I=1,N
   50 INDEX(I)=I
C
C Initialize the pointers.
C
      NSTAK=0
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
      IKEY=INDEX(LIMLO)
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
      IF (LO .EQ. HI)GO TO 200
C
      IF (DATUM(HI) .LE. DKEY) GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      INDEX(LO)=INDEX(HI)
      LO=LO+1
  110 CONTINUE
C
      IF (LO .EQ. HI) GO TO 200
C
      IF (DATUM(LO) .GE. DKEY) GO TO 119
C
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      INDEX(HI)=INDEX(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
      INDEX(LO)=IKEY
C
C At this point in the subroutine, all data between LIMLO and LO-1, 
C inclusive, are less than DATUM(LO), and all data between LO+1 and 
C LIMHI are larger than DATUM(LO).
C
C If both subarrays contain no more than one element, then take the most
C recent interval from the stack (if the stack is empty, we're done).
C If the larger of the two subarrays contains more than one element, and
C if the shorter subarray contains one or no elements, then forget the 
C shorter one and reduce the other subarray.  If the shorter subarray
C contains two or more elements, then place the larger subarray on the
C stack and process the subarray.
C
      IF (LIMHI-LO .GT. LO-LIMLO) GO TO 300
C
C Case 1:  the lower subarray is longer.  If it contains one or no 
C elements then take the most recent interval from the stack and go 
C back and operate on it.
C
      IF (LO-LIMLO .LE. 1) GO TO 400
C
C If the upper (shorter) subinterval contains one or no elements, then
C process the lower (longer) one, but if the upper subinterval contains
C more than one element, then place the lower (longer) subinterval on
C the stack and process the upper one.
C
      IF (LIMHI-LO .GE. 2) GO TO 250
C
C Case 1a:  the upper (shorter) subinterval contains no or one elements,
C so we go back and operate on the lower (longer) subinterval.
C
      LIMHI=LO-1
      GO TO 100
C
  250 CONTINUE
C
C Case 1b:  the upper (shorter) subinterval contains at least two 
C elements, so we place the lower (longer) subinterval on the stack and
C then go back and operate on the upper subinterval.
C 
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LIMLO
      STKHI(NSTAK)=LO-1
      LIMLO=LO+1
      GO TO 100
C
  300 CONTINUE
C
C Case 2:  the upper subarray is longer.  If it contains one or no 
C elements then take the most recent interval from the stack and 
C operate on it.
C
      IF (LIMHI-LO .LE. 1) GO TO 400
C
C If the lower (shorter) subinterval contains one or no elements, then
C process the upper (longer) one, but if the lower subinterval contains
C more than one element, then place the upper (longer) subinterval on
C the stack and process the lower one.
C
      IF (LO-LIMLO .GE. 2) GO TO 350
C
C Case 2a:  the lower (shorter) subinterval contains no or one elements,
C so we go back and operate on the upper (longer) subinterval.
C
      LIMLO=LO+1
      GO TO 100
C
  350 CONTINUE
C
C Case 2b:  the lower (shorter) subinterval contains at least two 
C elements, so we place the upper (longer) subinterval on the stack and
C then go back and operate on the lower subinterval.
C 
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LO+1
      STKHI(NSTAK)=LIMHI
      LIMHI=LO-1
      GO TO 100
C
  400 CONTINUE
C
C Take the most recent interval from the stack.  If the stack happens 
C to be empty, we are done.
C
      IF (NSTAK .LE. 0) RETURN
      LIMLO=STKLO(NSTAK)
      LIMHI=STKHI(NSTAK)
      NSTAK=NSTAK-1
      GO TO 100
C
      END
