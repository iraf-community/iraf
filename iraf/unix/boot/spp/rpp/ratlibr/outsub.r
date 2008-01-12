include	defs

# outsub - determine if argument is STDOUT substitution

   integer function outsub (arg, file, access)
   character arg (ARB), file (ARB)
   integer access

   if (arg (1) == GREATER & arg (2) != GREATER & arg (2) != EOS) {
      outsub = YES
      access = WRITE
      call scopy (arg, 2, file, 1)
      }

   else if (arg (1) == GREATER & arg (2) == GREATER & arg (3) != EOS) {
      access = APPEND
      outsub = YES
      call scopy (arg, 3, file, 1)
      }

   else
      outsub = NO

   return
   end
