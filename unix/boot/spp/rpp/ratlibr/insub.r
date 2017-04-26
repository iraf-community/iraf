include	defs

# insub - determine if argument is STDIN substitution

   integer function insub (arg, file)
   character arg (ARB), file (ARB)

   if (arg (1) == LESS & arg (2) != EOS) {
      insub = YES
      call scopy (arg, 2, file, 1)
      }
   else
      insub = NO

   return
   end
