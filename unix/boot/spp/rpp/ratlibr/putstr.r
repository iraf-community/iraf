include	defs

# putstr - output character string in specified field

   subroutine putstr (str, w, fd)
   character str (ARB)
   integer w
   filedes fd

   character length

   integer i, len

   len = length (str)
   for (i = len + 1; i <= w; i = i + 1)
      call putch (BLANK, fd)
   for (i = 1; i <= len; i = i + 1)
      call putch (str (i), fd)
   for (i = (-w) - len; i > 0; i = i - 1)
      call putch (BLANK, fd)

   return
   end
