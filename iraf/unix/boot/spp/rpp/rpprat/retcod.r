#-h-  retcod			  580  local   12/01/80  15:54:35
# retcod - generate code for return
   include  defs

   subroutine retcod

   character token (MAXTOK), t
   character gnbtok
   include COMMON_BLOCKS

   t = gnbtok (token, MAXTOK)
   if (t != NEWLINE & t != SEMICOL & t != RBRACE) {
      call pbstr (token)
      call outtab
      call scopy (fcname, 1, token, 1)
      call squash (token)
      call outstr (token)
      call outch (BLANK)
      call outch (EQUALS)
      call outch (BLANK)
      call eatup
      call outdon
      }
   else if (t == RBRACE)
      call pbstr (token)
   call outtab
   call ogotos (retlab, NO)
   xfer = YES
   return
   end
