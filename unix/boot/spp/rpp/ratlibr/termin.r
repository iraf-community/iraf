include	defs

# termin - pick up name of input channel to users teletype

   subroutine termin (name)
   character name (ARB)

   string tname TERMINAL_IN

   call scopy (tname, 1, name, 1)
   return
   end
