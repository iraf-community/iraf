include	defs

# putdec - put decimal integer n in field width >= w

   subroutine putdec(n,w)
   integer n, w

   character chars (MAXCHARS)

   integer i, nd
   integer itoc

   nd = itoc (n, chars, MAXCHARS)
   for (i = nd + 1; i <= w; i = i + 1)
      call putc (BLANK)
   for (i = 1; i <= nd; i = i + 1)
      call putc (chars (i))

   return
   end
