include	defs

# concat - concatenate two strings together

   subroutine concat (buf1, buf2, outstr)
   character buf1(ARB), buf2(ARB), outstr(ARB)

   integer i

   i = 1
   call stcopy (buf1, 1, outstr, i)
   call scopy (buf2, 1, outstr, i)

   return
   end
