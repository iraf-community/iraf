include	defs

# errsub - see if argument is ERROUT substitution

   integer function errsub (arg, file, access)

   character arg (ARB), file (ARB)
   integer access

   if (arg (1) == QMARK & arg (2) != QMARK & arg (2) != EOS) {
      errsub = YES
      access = WRITE
      call scopy (arg, 2, file, 1)
      }

   else if (arg (1) == QMARK & arg (2) == QMARK & arg (3) != EOS) {
      errsub = YES
      access = APPEND
      call scopy (arg, 3, file, 1)
      }

   else
      errsub = NO

   return
   end
