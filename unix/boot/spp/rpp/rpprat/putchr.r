#-h-  putchr			  233  local   12/01/80  15:54:34
# putchr - put single char into eval stack
   include  defs

   subroutine putchr (c)
   character c

   include COMMON_BLOCKS

   if (ep > EVALSIZE)
      call baderr ('evaluation stack overflow.')
   evalst (ep) = c
   ep = ep + 1
   return
   end
