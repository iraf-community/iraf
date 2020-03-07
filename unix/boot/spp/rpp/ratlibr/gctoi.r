include	defs

# gctoi --- convert any radix string to single precision integer

   integer function gctoi (str, i, radix)
   character str (ARB)
   integer i, radix

   integer base, v, d, j
   external index
   integer index

   character clower

   logical neg

   string digits "0123456789abcdef"

   v = 0
   base = radix

   while (str (i) == BLANK | str (i) == TAB)
      i = i + 1

   neg = (str (i) == MINUS)
   if (str (i) == PLUS | str (i) == MINUS)
      i = i + 1

   if (str (i + 2) == LETR & str (i) == DIG1 & DIG0 <= str(i+1) & str(i+1) <= DIG9
	 | str (i + 1) == LETR & DIG0 <= str(i) & str(i) <= DIG9) {
      base = str (i) - DIG0
      j = i
      if (str (i + 1) != LETR) {
	 j = j + 1
	 base = base * 10 + (str (j) - DIG0)
	 }
      if (base < 2 | base > 16)
	 base = radix
      else
	 i = j + 2
      }

   for (; str (i) != EOS; i = i + 1) {
      if (DIG0 <= str(i) & str(i) <= DIG9)
	 d = str (i) - DIG0
      else
	 d = index (digits, clower (str (i))) - 1
      if (d < 0 | d >= base)
	 break
      v = v * base + d
      }

   if (neg)
      return (-v)
   else
      return (+v)

   end
