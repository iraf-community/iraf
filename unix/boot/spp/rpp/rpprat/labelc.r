#-h-  labelc			  404  local   12/01/80  15:54:12
# labelc - output statement number
   include  defs

   subroutine labelc (lexstr)
   character lexstr (ARB)

   include COMMON_BLOCKS

   integer length, l

   xfer = NO   # can't suppress goto's now
   l = length (lexstr)
   if (l >= 3 & l < 4)		# possible conflict with pp-generated labels
       call synerr ("Warning: statement labels 100 and above are reserved.")
   call outstr (lexstr)
   call outtab
   return
   end
