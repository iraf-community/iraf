#-h-  outcon			  332  local   12/01/80  15:54:31
# outcon - output "n   continue"
   include  defs

   subroutine outcon (n)
   integer n

   include COMMON_BLOCKS

   string contin "continue"

   xfer = NO
   if (n <= 0 & outp == 0)
      return		# don't need unlabeled continues
   if (n > 0)
      call outnum (n)
   call outtab
   call outstr (contin)
   call outdon
   return
   end
