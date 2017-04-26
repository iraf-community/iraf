include	defs

# fold - fold all letters in a string to lower case

   subroutine fold (token)
   character token (ARB)

   character clower

   integer i

   for (i = 1; token (i) != EOS; i = i + 1)
      token (i) = clower (token (i))

   return
   end
