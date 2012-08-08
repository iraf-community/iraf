include	defs

# equal - compare str1 to str2;  return YES if equal, NO if not

   integer function equal (str1, str2)
   character str1(ARB), str2(ARB)

   integer i

   for (i = 1; str1 (i) == str2 (i); i = i + 1)
      if (str1 (i) == EOS)
	 return (YES)

   return (NO)
   end
