      integer function gctoi (str, i, radix)
      integer str (100)
      integer i, radix
      integer base, v, d, j
      external index
      integer index
      integer clower
      logical neg
      integer digits(17)
      data digits(1)/48/,digits(2)/49/,digits(3)/50/,digits(4)/51/,digit
     *s(5)/52/,digits(6)/53/,digits(7)/54/,digits(8)/55/,digits(9)/56/,d
     *igits(10)/57/,digits(11)/97/,digits(12)/98/,digits(13)/99/,digits(
     *14)/100/,digits(15)/101/,digits(16)/102/,digits(17)/-2/
      v = 0
      base = radix
23000 if (.not.(str (i) .eq. 32 .or. str (i) .eq. 9))goto 23001
      i = i + 1
      goto 23000
23001 continue
      neg = (str (i) .eq. 45)
      if (.not.(str (i) .eq. 43 .or. str (i) .eq. 45))goto 23002
      i = i + 1
23002 continue
      if (.not.(str (i + 2) .eq. 114 .and. str (i) .eq. 49 .and. (48.le.
     *str (i + 1).and.str (i + 1).le.57) .or. str (i + 1) .eq. 114 .and.
     * (48.le.str (i).and.str (i).le.57)))goto 23004
      base = str (i) - 48
      j = i
      if (.not.(str (i + 1) .ne. 114))goto 23006
      j = j + 1
      base = base * 10 + (str (j) - 48)
23006 continue
      if (.not.(base .lt. 2 .or. base .gt. 16))goto 23008
      base = radix
      goto 23009
23008 continue
      i = j + 2
23009 continue
23004 continue
23010 if (.not.(str (i) .ne. -2))goto 23012
      if (.not.((48.le.str (i).and.str (i).le.57)))goto 23013
      d = str (i) - 48
      goto 23014
23013 continue
      d = index (digits, clower (str (i))) - 1
23014 continue
      if (.not.(d .lt. 0 .or. d .ge. base))goto 23015
      goto 23012
23015 continue
      v = v * base + d
23011 i = i + 1
      goto 23010
23012 continue
      if (.not.(neg))goto 23017
      gctoi=(-v)
      return
23017 continue
      gctoi=(+v)
      return
23018 continue
      end
