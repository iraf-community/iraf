#-h-  outtab			  140  local   12/01/80  15:54:32
# outtab - get past column 6
   include  defs

   subroutine outtab

   include COMMON_BLOCKS

   while (outp < col)
      call outch (BLANK)
   return
   end
