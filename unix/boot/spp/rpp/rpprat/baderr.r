#-h-  baderr			  144  local   12/01/80  15:53:45
# baderr --- report fatal error message, then die
   include  defs

   subroutine baderr (msg)

   character msg (ARB)
#      character*(*) msg

   call synerr (msg)
   call endst
   end
