include	defs

# cupper - change letter to upper case

   character function cupper (c)
   character c

   if (c >= LETA & c <= LETZ)
      cupper = c + (BIGA - LETA)
   else
      cupper = c

   return
   end
