include	defs

# upper - fold all alphas to upper case

   subroutine upper (token)
   character token (ARB)

   character cupper

   integer i

   for (i = 1; token (i) != EOS; i = i + 1)
      token (i) = cupper (token (i))

   return
   end
