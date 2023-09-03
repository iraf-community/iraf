      subroutine dsfree (block)
      integer block
      integer mem( 60000)
      common/cdsmem/mem
      integer p0, p, q
      integer n, junk
      integer con (10)
      p0 = block - 2
      n = mem (p0 + 0)
      q = 2
23000 continue
      p = mem (q + 1)
      if (.not.(p .eq. 0 .or. p .gt. p0))goto 23003
      goto 23002
23003 continue
      q = p
23001 goto 23000
23002 continue
      if (.not.(q + mem (q + 0) .gt. p0))goto 23005
      call remark (45Hin dsfree:	attempt to free unallocated block.)
      call remark (21Htype 'c' to continue.)
      junk = getlin (con, 0)
      if (.not.(con (1) .ne. 99 .and. con (1) .ne. 67))goto 23007
      call endst
23007 continue
      return
23005 continue
      if (.not.(p0 + n .eq. p .and. p .ne. 0))goto 23009
      n = n + mem (p + 0)
      mem (p0 + 1) = mem (p + 1)
      goto 23010
23009 continue
      mem (p0 + 1) = p
23010 continue
      if (.not.(q + mem (q + 0) .eq. p0))goto 23011
      mem (q + 0) = mem (q + 0) + n
      mem (q + 1) = mem (p0 + 1)
      goto 23012
23011 continue
      mem (q + 1) = p0
      mem (p0 + 0) = n
23012 continue
      return
      end
