#-h-  outdon			  257  local   12/01/80  15:54:31
# outdon - finish off an output line
   include  defs

   subroutine outdon

   include COMMON_BLOCKS

   integer allblk

   outbuf (outp + 1) = NEWLINE
   outbuf (outp + 2) = EOS
   if (allblk (outbuf) == NO)
      call putlin (outbuf, STDOUT)
   outp = 0
   return
   end
