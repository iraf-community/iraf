include	defs

# addset - put c in string (j) if it fits, increment j

   integer function addset (c, str, j, maxsiz)
   integer j, maxsiz
   character c, str (maxsiz)

   if (j > maxsiz)
      addset = NO
   else {
      str(j) = c
      j = j + 1
      addset = YES
      }

   return
   end
