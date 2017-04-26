include	defs

# filset --- expand set at  array (i)  into  set (j),  stop at	delim

   subroutine filset (delim, array, i, set, j, maxset)
   integer i, j, maxset
   character array (ARB), delim, set (maxset)

   character esc

   integer junk
   external index
   integer addset, index

   string digits "0123456789"
   string lowalf "abcdefghijklmnopqrstuvwxyz"
   string upalf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

   for ( ; array (i) != delim & array (i) != EOS; i = i + 1)
      if (array (i) == ESCAPE)
	 junk = addset (esc (array, i), set, j, maxset)
      else if (array (i) != DASH)
	 junk = addset (array (i), set, j, maxset)
      else if (j <= 1 | array (i + 1) == EOS)	# literal -
	 junk = addset (DASH, set, j, maxset)
      else if (index (digits, set (j - 1)) > 0)
	 call dodash (digits, array, i, set, j, maxset)
      else if (index (lowalf, set (j - 1)) > 0)
	 call dodash (lowalf, array, i, set, j, maxset)
      else if (index (upalf, set (j - 1)) > 0)
	 call dodash (upalf, array, i, set, j, maxset)
      else
	 junk = addset (DASH, set, j, maxset)
   return
   end
