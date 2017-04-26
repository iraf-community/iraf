include	defs

# dodash --- expand array (i-1)-array (i+1) into set (j)... from valid

   subroutine dodash (valid, array, i, set, j, maxset)
   integer i, j, maxset
   character valid (ARB), array (ARB), set (maxset)

   character esc

   integer junk, k, limit
   external index
   integer addset, index

   i = i + 1
   j = j - 1
   limit = index (valid, esc (array, i))
   for (k = index (valid, set (j)); k <= limit; k = k + 1)
      junk = addset (valid (k), set, j, maxset)

   return
   end
