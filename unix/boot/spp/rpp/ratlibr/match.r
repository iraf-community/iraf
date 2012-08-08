include	defs

# match --- find match anywhere on line

   integer function match (lin, pat)
   character lin (MAXLINE), pat (MAXPAT)

   integer i, junk (9)
   integer amatch

   for (i = 1; lin (i) != EOS; i = i + 1)
      if (amatch (lin, i, pat, junk, junk) > 0) {
	 match = YES
	 return
	 }
   match = NO
   return
   end
