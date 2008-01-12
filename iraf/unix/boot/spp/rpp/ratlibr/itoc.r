include	defs

# itoc - convert integer  int  to char string in  str

   integer function itoc (int, str, size)
   integer int, size
   character str (ARB)

   integer mod
   integer d, i, intval, j, k

   # string digits "0123456789"
   character digits (11)
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

   intval = iabs (int)
   str (1) = EOS
   i = 1
   repeat {			     # generate digits
      i = i + 1
      d = mod (intval, 10)
      str (i) = digits (d+1)
      intval = intval / 10
      } until (intval == 0 | i >= size)

   if (int < 0 & i < size) {	     # then sign
      i = i + 1
      str (i) = MINUS
      }
   itoc = i - 1

   for (j = 1; j < i; j = j + 1) {   # then reverse
      k = str (i)
      str (i) = str (j)
      str (j) = k
      i = i - 1
      }

   return
   end
