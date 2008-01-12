#-h-  skpblk			  247  local   12/01/80  15:55:04
# skpblk - skip blanks and tabs in current input file
   include  defs

   subroutine skpblk

   include COMMON_BLOCKS

   character c
   character ngetch

   for (c = ngetch (c); c == BLANK | c == TAB; c = ngetch (c))
      ;

   call putbak (c)
   return
   end
