include	defs

# strcmp - compare 2 strings; return -1 if <, 0 if =, +1 if >

   integer function strcmp (str1, str2)
   character str1 (ARB), str2 (ARB)

   integer i

   for (i = 1; str1 (i) == str2 (i); i = i + 1)
      if (str1 (i) == EOS)
	 return (0)

   if (str1 (i) == EOS)
      strcmp = -1
   else if (str2 (i) == EOS)
      strcmp = + 1
   else if (str1 (i) < str2 (i))
      strcmp = -1
   else
      strcmp = +1

   return
   end
