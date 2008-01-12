#-h-  relate			 1276  local   12/01/80  15:54:35
# relate - convert relational shorthands into long form
   include  defs

   subroutine relate (token, last)
   character token (ARB)
   integer last

   character ngetch

   integer length

   if (ngetch (token (2)) != EQUALS) {
      call putbak (token (2))
      token (3) = LETT
      }
   else
      token (3) = LETE
   token (4) = PERIOD
   token (5) = EOS
   token (6) = EOS # for .not. and .and.
   if (token (1) == GREATER)
      token (2) = LETG
   else if (token (1) == LESS)
      token (2) = LETL
   else if (token (1) == NOT | token (1) == BANG |
	    token (1) == CARET | token (1) == TILDE) {
      if (token (2) != EQUALS) {
	 token (3) = LETO
	 token (4) = LETT
	 token (5) = PERIOD
	 }
      token (2) = LETN
      }
   else if (token (1) == EQUALS) {
      if (token (2) != EQUALS) {
	 token (2) = EOS
	 last = 1
	 return
	 }
      token (2) = LETE
      token (3) = LETQ
      }
   else if (token (1) == AND) {
      token (2) = LETA
      token (3) = LETN
      token (4) = LETD
      token (5) = PERIOD
      }
   else if (token (1) == OR) {
      token (2) = LETO
      token (3) = LETR
      }
   else   # can't happen
      token (2) = EOS
   token (1) = PERIOD
   last = length (token)
   return
   end
