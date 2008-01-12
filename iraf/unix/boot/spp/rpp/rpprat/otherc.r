#-h-  otherc			  284  local   12/01/80  15:54:30
# otherc - output ordinary Fortran statement
   include  defs

   subroutine otherc (lexstr)
   character lexstr(ARB)

   include COMMON_BLOCKS

   xfer = NO
   call outtab
   if (IS_LETTER(lexstr (1)))
      call squash (lexstr)
   call outstr (lexstr)
   call eatup
   call outdwe
   return
   end
