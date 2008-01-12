#-h-  outstr			  687  local   12/01/80  15:54:32
# outstr - output string; handles quoted literals
   include  defs

   subroutine outstr (str)
   character str (ARB)

   character c
   ifdef (UPPERC, character cupper)

   integer i, j

   for (i = 1; str (i) != EOS; i = i + 1) {
      c = str (i)
      if (c != SQUOTE & c != DQUOTE) {
			 # produce upper case fortran, if desired
	 ifdef (UPPERC,
	    c = cupper (c)
	    )
	 call outch (c)
	 }
      else {
	 i = i + 1
	 for (j = i; str (j) != c; j = j + 1)	# find end
	    ;
	 call outnum (j - i)
	 call outch (BIGH)
	 for ( ; i < j; i = i + 1)
	    call outch (str (i))
	 }
      }
   return
   end
