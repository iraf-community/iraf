include	defs

# length - compute length of string

   integer function length (str)
   character str (ARB)

   for (length = 0; str (length+1) != EOS; length = length + 1)
      ;

   return
   end
