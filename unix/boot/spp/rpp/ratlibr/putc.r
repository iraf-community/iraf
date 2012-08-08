include	defs

# putc - put character onto STDOUT

   subroutine putc (c)
   character c

   call putch (c, STDOUT)

   return
   end
