#-h-  synerr			  703  local   12/01/80  15:55:08
# synerr --- report non-fatal error
   include  defs

   subroutine synerr (msg)

   character msg
#      character*(*) msg

   include COMMON_BLOCKS
   character lc (MAXCHARS)

   integer i, junk
   integer itoc

   string of " of "
   string errmsg "Error on line "

   call putlin (errmsg, ERROUT)
   if (level >= 1)
      i = level
   else
      i = 1 # for EOF errors
   junk = itoc (linect (i), lc, MAXCHARS)
   call putlin (lc, ERROUT)
   for (i = fnamp - 1; i >= 1; i = i - 1)
      if (fnames (i - 1) == EOS | i == 1) {  # print file name
	 call putlin (of, ERROUT)
	 call putlin (fnames (i), ERROUT)
	 break
	 }

   call putch (COLON, ERROUT)
   call putch (BLANK, ERROUT)
   call remark (msg)
   return
   end
