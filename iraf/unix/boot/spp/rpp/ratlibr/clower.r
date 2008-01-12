include	defs

# clower - change letter to lower case

   character function clower(c)
   character c

   character k

   if (c >= BIGA & c <= BIGZ) {
      k = LETA - BIGA	# avoid integer overflow in byte machines
      clower = c + k
      }
   else
      clower = c

   return
   end
