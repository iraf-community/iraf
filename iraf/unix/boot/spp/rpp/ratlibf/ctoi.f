      integer function ctoi(in, i)
      integer in (100)
      integer i
      integer d
      external index
      integer index
      integer digits(11)
      data digits (1) /48/, digits (2) /49/, digits (3) /50/, digits (4)
     * /51/, digits (5) /52/, digits (6) /53/, digits (7) /54/, digits (
     *8) /55/, digits (9) /56/, digits (10) /57/, digits (11) /-2/
23000 if (.not.(in (i) .eq. 32 .or. in (i) .eq. 9))goto 23001
      i = i + 1
      goto 23000
23001 continue
      ctoi = 0
23002 if (.not.(in (i) .ne. -2))goto 23004
      d = index (digits, in (i))
      if (.not.(d .eq. 0))goto 23005
      goto 23004
23005 continue
      ctoi = 10 * ctoi + d - 1
23003 i = i + 1
      goto 23002
23004 continue
      return
      end
