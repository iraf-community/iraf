include	defs

# makpat --- make pattern from arg (from), terminate at delim

   integer function makpat (arg, from, delim, pat)
   character arg (MAXARG), delim, pat (MAXPAT)
   integer from

   character esc

   integer i, j, junk, lastcl, lastj, lj,
      tagnst, tagnum, tagstk (9)
   integer addset, getccl, stclos

   j = 1      # pat index
   lastj = 1
   lastcl = 0
   tagnum = 0
   tagnst = 0
   for (i = from; arg (i) != delim & arg (i) != EOS; i = i + 1) {
      lj = j
      if (arg (i) == ANY)
	 junk = addset (ANY, pat, j, MAXPAT)
      else if (arg (i) == BOL & i == from)
	 junk = addset (BOL, pat, j, MAXPAT)
      else if (arg (i) == EOL & arg (i + 1) == delim)
	 junk = addset (EOL, pat, j, MAXPAT)
      else if (arg (i) == CCL) {
	 if (getccl (arg, i, pat, j) == ERR) {
	    makpat = ERR
	    return
	    }
	 }
      else if (arg (i) == CLOSURE & i > from) {
	 lj = lastj
	 if (pat (lj) == BOL | pat (lj) == EOL | pat (lj) == CLOSURE |
	       pat (lj) == START_TAG | pat (lj) == STOP_TAG)
	    break
	 lastcl = stclos (pat, j, lastj, lastcl)
	 }
      else if (arg (i) == START_TAG) {
	 if (tagnum >= 9)    # too many tagged sub-patterns
	    break
	 tagnum = tagnum + 1
	 tagnst = tagnst + 1
	 tagstk (tagnst) = tagnum
	 junk = addset (START_TAG, pat, j, MAXPAT)
	 junk = addset (tagnum, pat, j, MAXPAT)
	 }
      else if (arg (i) == STOP_TAG & tagnst > 0) {
	 junk = addset (STOP_TAG, pat, j, MAXPAT)
	 junk = addset (tagstk (tagnst), pat, j, MAXPAT)
	 tagnst = tagnst - 1
	 }
      else {
	 junk = addset (CHAR, pat, j, MAXPAT)
	 junk = addset (esc (arg, i), pat, j, MAXPAT)
	 }
      lastj = lj
      }
   if (arg (i) != delim)   # terminated early
      makpat = ERR
   else if (addset (EOS, pat, j, MAXPAT) == NO)   # no room
      makpat = ERR
   else if (tagnst != 0)
      makpat = ERR
   else
      makpat = i
   return
   end
