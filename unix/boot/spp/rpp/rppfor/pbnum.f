      subroutine pbnum (n)
      integer n
      integer m, num
      integer mod
      integer digits(11)
      data digits(1)/48/,digits(2)/49/,digits(3)/50/,digits(4)/51/,digit
     *s(5)/52/,digits(6)/53/,digits(7)/54/,digits(8)/55/,digits(9)/56/,d
     *igits(10)/57/,digits(11)/-2/
      num = n
23000 continue
      m = mod (num, 10)
      call putbak (digits (m + 1))
      num = num / 10
23001 if (.not.(num .eq. 0))goto 23000
23002 continue
      return
      end
