#-h-  evalr			 1126  local   12/01/80  15:54:06
# evalr - expand args i through j: evaluate builtin or push back defn
   include  defs

   subroutine evalr (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer argno, k, m, n, t, td, in_string, delim
   external index
   integer index, length

   string digits '0123456789'

   t = argstk (i)
   td = evalst (t)
   if (td == MACTYPE)
      call domac (argstk, i, j)
   else if (td == INCTYPE)
      call doincr (argstk, i, j)
   else if (td == SUBTYPE)
      call dosub (argstk, i, j)
   else if (td == IFTYPE)
      call doif (argstk, i, j)
   else if (td == ARITHTYPE)
      call doarth (argstk, i, j)
   else {
      in_string = NO
      for (k = t + length (evalst (t)) - 1; k > t; k = k - 1)
	 if (evalst(k) == SQUOTE | evalst(k) == DQUOTE) {
	    if (in_string == NO) {
		delim = evalst(k)
		in_string = YES
		}
	    else
		in_string = NO
	    call putbak (evalst(k))
	    }
	 # Don't expand $arg if in a string.
	 else if (evalst(k-1) != ARGFLAG | in_string == YES)
	    call putbak (evalst (k))
	 else {
	    argno = index (digits, evalst (k))	-  1
	    if (argno >= 0 & argno < j - i) {
	       n = i + argno + 1
	       m = argstk (n)
	       call pbstr (evalst (m))
	       }
	    k = k - 1	# skip over $
	    }
      if (k == t)	  # do last character
	 call putbak (evalst (k))
      }
   return
   end
