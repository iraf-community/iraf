#-h-  balpar			  854  local   12/01/80  15:53:46
# balpar - copy balanced paren string
   include  defs

   subroutine balpar

   character t, token (MAXTOK)
   character gettok, gnbtok

   integer nlpar

   if (gnbtok (token, MAXTOK) != LPAREN) {
      call synerr ("missing left paren.")
      return
      }
   call outstr (token)
   nlpar = 1
   repeat {
      t = gettok (token, MAXTOK)
      if (t == SEMICOL | t == LBRACE | t == RBRACE | t == EOF) {
	 call pbstr (token)
	 break
	 }
      if (t == NEWLINE)      # delete newlines
	 token (1) = EOS
      else if (t == LPAREN)
	 nlpar = nlpar + 1
      else if (t == RPAREN)
	 nlpar = nlpar - 1
      if (t == ALPHA)
	 call squash (token)
      # else nothing special
      call outstr (token)
      } until (nlpar <= 0)

   if (nlpar != 0)
      call synerr ("missing parenthesis in condition.")

   return
   end
