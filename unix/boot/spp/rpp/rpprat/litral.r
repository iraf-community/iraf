#-h-  litral			  316  local   12/01/80  15:54:13
# litral - process literal Fortran line
   include  defs

   subroutine litral

   include COMMON_BLOCKS

   character ngetch

   # Finish off any left-over characters
   if (outp > 0)
      call outdwe

   for (outp = 1; ngetch (outbuf (outp)) != NEWLINE; outp = outp + 1)
      ;
   outp = outp - 1
   call outdwe
   return
   end
