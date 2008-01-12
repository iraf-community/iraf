      subroutine filset (delim, array, i, set, j, maxset)
      integer i, j, maxset
      integer array (100), delim, set (maxset)
      integer esc
      integer junk
      external index
      integer addset, index
      integer digits(11)
      integer lowalf(27)
      integer upalf(27)
      data digits(1)/48/,digits(2)/49/,digits(3)/50/,digits(4)/51/,digit
     *s(5)/52/,digits(6)/53/,digits(7)/54/,digits(8)/55/,digits(9)/56/,d
     *igits(10)/57/,digits(11)/-2/
      data lowalf(1)/97/,lowalf(2)/98/,lowalf(3)/99/,lowalf(4)/100/,lowa
     *lf(5)/101/,lowalf(6)/102/,lowalf(7)/103/,lowalf(8)/104/,lowalf(9)/
     *105/,lowalf(10)/106/,lowalf(11)/107/,lowalf(12)/108/,lowalf(13)/10
     *9/,lowalf(14)/110/,lowalf(15)/111/,lowalf(16)/112/,lowalf(17)/113/
     *,lowalf(18)/114/,lowalf(19)/115/,lowalf(20)/116/,lowalf(21)/117/,l
     *owalf(22)/118/,lowalf(23)/119/,lowalf(24)/120/,lowalf(25)/121/,low
     *alf(26)/122/,lowalf(27)/-2/
      data upalf(1)/65/,upalf(2)/66/,upalf(3)/67/,upalf(4)/68/,upalf(5)/
     *69/,upalf(6)/70/,upalf(7)/71/,upalf(8)/72/,upalf(9)/73/,upalf(10)/
     *74/,upalf(11)/75/,upalf(12)/76/,upalf(13)/77/,upalf(14)/78/,upalf(
     *15)/79/,upalf(16)/80/,upalf(17)/81/,upalf(18)/82/,upalf(19)/83/,up
     *alf(20)/84/,upalf(21)/85/,upalf(22)/86/,upalf(23)/87/,upalf(24)/88
     */,upalf(25)/89/,upalf(26)/90/,upalf(27)/-2/
23000 if (.not.(array (i) .ne. delim .and. array (i) .ne. -2))goto 23002
      if (.not.(array (i) .eq. 64))goto 23003
      junk = addset (esc (array, i), set, j, maxset)
      goto 23004
23003 continue
      if (.not.(array (i) .ne. 45))goto 23005
      junk = addset (array (i), set, j, maxset)
      goto 23006
23005 continue
      if (.not.(j .le. 1 .or. array (i + 1) .eq. -2))goto 23007
      junk = addset (45, set, j, maxset)
      goto 23008
23007 continue
      if (.not.(index (digits, set (j - 1)) .gt. 0))goto 23009
      call dodash (digits, array, i, set, j, maxset)
      goto 23010
23009 continue
      if (.not.(index (lowalf, set (j - 1)) .gt. 0))goto 23011
      call dodash (lowalf, array, i, set, j, maxset)
      goto 23012
23011 continue
      if (.not.(index (upalf, set (j - 1)) .gt. 0))goto 23013
      call dodash (upalf, array, i, set, j, maxset)
      goto 23014
23013 continue
      junk = addset (45, set, j, maxset)
23014 continue
23012 continue
23010 continue
23008 continue
23006 continue
23004 continue
23001 i = i + 1
      goto 23000
23002 continue
      return
      end
