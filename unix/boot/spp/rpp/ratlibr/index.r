include	defs

# index - find character  c  in string	str

   integer function index (str, c)
   character str (ARB), c

   for (index = 1; str (index) != EOS; index = index + 1)
      if (str (index) == c)
	 return

   index = 0
   return
   end
