#-h-  pbnum			  304  local   12/01/80  15:54:33
# pbnum - convert number to string, push back on input
   include  defs

   subroutine pbnum (n)
   integer n

   integer m, num
   integer mod

   string digits '0123456789'

   num = n
   repeat {
      m = mod (num, 10)
      call putbak (digits (m + 1))
      num = num / 10
      } until (num == 0)
   return
   end
