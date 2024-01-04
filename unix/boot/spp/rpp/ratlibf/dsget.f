      integer function dsget (w)
      integer w
      integer mem( 60000)
      common/cdsmem/mem
      integer p, q, l
      integer n, k, junk
      integer getlin
      integer c (10)
      n = w + 2
      q = 2
23000 continue
      p = mem (q + 1)
      if (.not.(p .eq. 0))goto 23003
      call remark (31Hin dsget: out of storage space.)
      call remark (41Htype 'c' or 'i' for char or integer dump.)
      junk = getlin (c, 0)
      if (.not.(c (1) .eq. 99 .or. c (1) .eq. 67))goto 23005
      call dsdump (97)
      goto 23006
23005 continue
      if (.not.(c (1) .eq. 105 .or. c (1) .eq. 73))goto 23007
      call dsdump (48)
23007 continue
23006 continue
      call error (19Hprogram terminated.)
23003 continue
      if (.not.(mem (p + 0) .ge. n))goto 23009
      goto 23002
23009 continue
      q = p
23001 goto 23000
23002 continue
      k = mem (p + 0) - n
      if (.not.(k .ge. 8))goto 23011
      mem (p + 0) = k
      l = p + k
      mem (l + 0) = n
      goto 23012
23011 continue
      mem (q + 1) = mem (p + 1)
      l = p
23012 continue
      dsget=(l + 2)
      return
      end
