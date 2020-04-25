#-h-  otherc			  284  local   12/01/80  15:54:30
# otherc - output ordinary Fortran statement
   include  defs

   subroutine otherc (lexstr)
   character lexstr(ARB)

   include COMMON_BLOCKS

   xfer = NO
   call outtab
   if ((BIGA <= lexstr(1) & lexstr(1) <= BIGZ)
     | (LETA <= lexstr(1) & lexstr(1) <= LETZ))
      call squash (lexstr)
   call outstr (lexstr)
   call eatup
   call outdwe
   return
   end
