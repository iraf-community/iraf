include	defs

# wkday --- get day-of-week corresponding to month,day,year

   integer function wkday (month, day, year)
   integer month, day, year

   integer lmonth, lday, lyear

   lmonth = month - 2
   lday = day
   lyear = year

   if (lmonth <= 0) {
      lmonth = lmonth + 12
      lyear = lyear - 1
      }

   wkday = mod (lday + (26 * lmonth - 2) / 10 + lyear + lyear / 4 - 34,
       7) + 1

   return
   end
