include	defs

# locate --- look for c in char class at pat (offset)

   integer function locate (c, pat, offset)
   character c, pat (MAXPAT)
   integer offset

   integer i

   # size of class is at pat (offset), characters follow
   for (i = offset + pat (offset); i > offset; i = i - 1)
      if (c == pat (i))
	 return (YES)

   return (NO)
   end
