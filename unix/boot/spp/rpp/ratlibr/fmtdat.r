include	defs

# fmtdat - format date and time information

   subroutine fmtdat(date, time, now, form)
   character date(ARB), time(ARB)
   integer now(7), form

   # at present, simply return mm/dd/yy and hh:mm:ss
   # 'form' is reserved for selecting different formats
   # when those have been chosen.

   date(1) = now(2) / 10 + DIG0
   date(2) = mod(now(2), 10) + DIG0
   date(3) = SLASH
   date(4) = now(3) / 10 + DIG0
   date(5) = mod(now(3), 10) + DIG0
   date(6) = SLASH
   date(7) = mod(now(1), 100) / 10 + DIG0
   date(8) = mod(now(1), 10) + DIG0
   date(9) = EOS

   time(1) = now(4) / 10 + DIG0
   time(2) = mod(now(4), 10) + DIG0
   time(3) = COLON
   time(4) = now(5) / 10 + DIG0
   time(5) = mod(now(5), 10) + DIG0
   time(6) = COLON
   time(7) = now(6) / 10 + DIG0
   time(8) = mod(now(6), 10) + DIG0
   time(9) = EOS

   return
   end
