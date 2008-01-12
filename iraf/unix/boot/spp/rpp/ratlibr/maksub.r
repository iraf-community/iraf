include	defs

# maksub --- make substitution string in sub

   integer function maksub (arg, from, delim, sub)
   character arg (MAXARG), delim, sub (MAXPAT)
   integer from

   character esc, type

   integer i, j, junk
   integer addset

   j = 1
   for (i = from; arg (i) != delim & arg (i) != EOS; i = i + 1)
      if (arg (i) == AND) {
	 junk = addset (DITTO, sub, j, MAXPAT)
	 junk = addset (0, sub, j, MAXPAT)
	 }
      else if (arg (i) == ESCAPE & type (arg (i + 1)) == DIGIT) {
	 i = i + 1
	 junk = addset (DITTO, sub, j, MAXPAT)
	 junk = addset (arg (i) - DIG0, sub, j, MAXPAT)
	 }
      else
	 junk = addset (esc (arg, i), sub, j, MAXPAT)
   if (arg (i) != delim)   # missing delimiter
      maksub = ERR
   else if (addset (EOS, sub, j, MAXPAT) == NO)   # no room
      maksub = ERR
   else
      maksub = i
   return
   end
