#-h-  putbak			  254  local   12/01/80  15:54:34
# putbak - push character back onto input
   include  defs

   subroutine putbak (c)
   character c

   include COMMON_BLOCKS

   if (bp <= 1)
      call baderr ("too many characters pushed back.")
   else {
      bp = bp - 1
      buf (bp) = c
      }

   return
   end
