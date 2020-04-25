#-h-  swvar			  157  local   12/01/80  15:55:08
# swvar - output switch variable SWnnnn, where nnnn = lab
# (modified aug82 dct to permit declaration of switch variable)

   include  defs

   subroutine swvar (lab)
   integer lab, i, labnum, ndigits

   call outch (LETS)
   call outch (LETW)

   labnum = lab
   for (ndigits=0;  labnum > 0;  labnum=labnum/10)
    	ndigits = ndigits + 1
   for (i=3;  i <= 6 - ndigits;  i=i+1)
	call outch (DIG0)
   call outnum (lab)
   return
   end
