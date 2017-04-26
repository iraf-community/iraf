#-h-  dosub			  709  local   12/01/80  15:53:50
# dosub - select macro substring
   include  defs

   subroutine dosub (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer ap, fc, k, nc
   integer ctoi, length

   if (j - i < 3)
      return
   if (j - i < 4)
      nc = MAXTOK
   else {
      k = argstk (i + 4)
      nc = ctoi (evalst, k)	 # number of characters
      }
   k = argstk (i + 3)	      # origin
   ap = argstk (i + 2)	       # target string
   fc = ap + ctoi (evalst, k) - 1   # first char of substring
   if (fc >= ap & fc < ap + length (evalst (ap))) {   # subarrays
      k = fc + min (nc, length (evalst (fc))) - 1
       for ( ; k >= fc; k = k - 1)
	  call putbak (evalst (k))
       }

    return
    end
