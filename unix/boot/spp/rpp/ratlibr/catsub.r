include	defs

# catsub --- add replacement text to end of  new

   subroutine catsub (lin, from, to, sub, new, k, maxnew)

   character	lin(MAXLINE)
   integer	from(10), to(10)
   integer	maxnew
   character	sub(maxnew), new(MAXPAT)
   integer	k

   integer	i, j, junk, ri
   integer	addset

   for (i = 1; sub (i) != EOS; i = i + 1)
      if (sub (i) == DITTO) {
	 i = i + 1
	 ri = sub (i) + 1
	 for (j = from (ri); j < to (ri); j = j + 1)
	    junk = addset (lin (j), new, k, maxnew)
	 }
      else
	 junk = addset (sub (i), new, k, maxnew)

   return
   end
