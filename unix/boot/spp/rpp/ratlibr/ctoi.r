include	defs

# ctoi - convert string at in(i) to integer, increment i

   integer function ctoi(in, i)
   character in (ARB)
   integer i

   integer d
   external index
   integer index

   # string digits "0123456789"
   character digits(11)
   data digits (1) /DIG0/,
      digits (2) /DIG1/,
      digits (3) /DIG2/,
      digits (4) /DIG3/,
      digits (5) /DIG4/,
      digits (6) /DIG5/,
      digits (7) /DIG6/,
      digits (8) /DIG7/,
      digits (9) /DIG8/,
      digits (10) /DIG9/,
      digits (11) /EOS/

   while (in (i) == BLANK | in (i) == TAB)
      i = i + 1
   for (ctoi = 0; in (i) != EOS; i = i + 1) {
      d = index (digits, in (i))
      if (d == 0)      # non-digit
	 break
      ctoi = 10 * ctoi + d - 1
      }

   return
   end
