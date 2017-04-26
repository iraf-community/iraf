      subroutine fmtdat(date, time, now, form)
      integer date(100), time(100)
      integer now(7), form
      date(1) = now(2) / 10 + 48
      date(2) = mod(now(2), 10) + 48
      date(3) = 47
      date(4) = now(3) / 10 + 48
      date(5) = mod(now(3), 10) + 48
      date(6) = 47
      date(7) = mod(now(1), 100) / 10 + 48
      date(8) = mod(now(1), 10) + 48
      date(9) = -2
      time(1) = now(4) / 10 + 48
      time(2) = mod(now(4), 10) + 48
      time(3) = 58
      time(4) = now(5) / 10 + 48
      time(5) = mod(now(5), 10) + 48
      time(6) = 58
      time(7) = now(6) / 10 + 48
      time(8) = mod(now(6), 10) + 48
      time(9) = -2
      return
      end
