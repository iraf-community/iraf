      integer function wkday (month, day, year)
      integer month, day, year
      integer lmonth, lday, lyear
      lmonth = month - 2
      lday = day
      lyear = year
      if (.not.(lmonth .le. 0))goto 23000
      lmonth = lmonth + 12
      lyear = lyear - 1
23000 continue
      wkday = mod (lday + (26 * lmonth - 2) / 10 + lyear + lyear / 4 - 3
     *4, 7) + 1
      return
      end
