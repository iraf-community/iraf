      subroutine catsub (lin, from, to, sub, new, k, maxnew)
      integer lin(128)
      integer from(10), to(10)
      integer maxnew
      integer sub(maxnew), new(128)
      integer k
      integer i, j, junk, ri
      integer addset
      i = 1
23000 if (.not.(sub (i) .ne. -2))goto 23002
      if (.not.(sub (i) .eq. -3))goto 23003
      i = i + 1
      ri = sub (i) + 1
      j = from (ri)
23005 if (.not.(j .lt. to (ri)))goto 23007
      junk = addset (lin (j), new, k, maxnew)
23006 j = j + 1
      goto 23005
23007 continue
      goto 23004
23003 continue
      junk = addset (sub (i), new, k, maxnew)
23004 continue
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
