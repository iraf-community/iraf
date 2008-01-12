include	defs

# addstr - add s to str(j) if it fits, increment j

   integer function addstr (s, str, j, maxsiz)
   integer j, maxsiz
   character s (ARB), str (maxsiz)

   integer i, addset

   for (i = 1; s (i) != EOS; i = i + 1)
      if (addset (s (i), str, j, maxsiz) == NO) {
	 addstr = NO
	 return
	 }
   addstr = YES

   return
   end
