#-h-  outnum			  381  local   12/01/80  15:54:32
# outnum - output decimal number
   include  defs

   subroutine outnum (n)
   integer n

   character chars (MAXCHARS)

   integer i, m

   m = iabs (n)
   i = 0
   repeat {
      i = i + 1
      chars (i) = mod (m, 10) + DIG0
      m = m / 10
      } until (m == 0 | i >= MAXCHARS)
   if (n < 0)
      call outch (MINUS)
   for ( ; i > 0; i = i - 1)
      call outch (chars (i))
   return
   end
