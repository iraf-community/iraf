include	defs

# trmout - pick up name of output channel to users teletype

   subroutine trmout (name)
   character name (ARB)

   string tname TERMINAL_OUT

   call scopy (tname, 1, name, 1)
   return
   end
