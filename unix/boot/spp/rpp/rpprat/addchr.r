#-h-  addchr			  254  local   12/01/80  15:53:44
# addchr - put c in buf (bp) if it fits, increment bp
   include  defs

   subroutine addchr (c, buf, bp, maxsiz)
   integer bp, maxsiz
   character c, buf (ARB)

   if (bp > maxsiz)
      call baderr ("buffer overflow.")
   buf (bp) = c
   bp = bp + 1

   return
   end
