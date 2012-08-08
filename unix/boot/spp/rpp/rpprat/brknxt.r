#-h-  brknxt			 1077  local   12/01/80  15:53:46
# brknxt - generate code for break n and next n; n = 1 is default
   include  defs

   subroutine brknxt (sp, lextyp, labval, token)
   integer labval (MAXSTACK), lextyp (MAXSTACK), sp, token

   integer i, n
   integer alldig, ctoi

   character t, ptoken (MAXTOK)
   character gnbtok

   include COMMON_BLOCKS

   n = 0
   t = gnbtok (ptoken, MAXTOK)
   if (alldig (ptoken) == YES) {     # have break n or next n
      i = 1
      n = ctoi (ptoken, i) - 1
      }
   else if (t != SEMICOL)      # default case
      call pbstr (ptoken)
   for (i = sp; i > 0; i = i - 1)
      if (lextyp (i) == LEXWHILE | lextyp (i) == LEXDO
	| lextyp (i) == LEXFOR | lextyp (i) == LEXREPEAT) {
	 if (n > 0) {
	    n = n - 1
	    next	     # seek proper level
	    }
	 else if (token == LEXBREAK)
	    call outgo (labval (i) + 1)
	 else
	    call outgo (labval (i))
	 xfer = YES
	 return
	 }

   if (token == LEXBREAK)
      call synerr ("illegal break.")
   else
      call synerr ("illegal next.")

   return
   end
