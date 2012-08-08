include	defs

# strim --- trim trailing blanks and tabs from a string

   integer function strim (str)
   character str (ARB)

   integer lnb, i

   lnb = 0
   for (i = 1; str (i) != EOS; i = i + 1)
      if (str (i) != BLANK & str (i) != TAB)
	 lnb = i

   str (lnb + 1) = EOS
   return (lnb)

   end
