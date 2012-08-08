#-h-  eatup			 1137  local   12/01/80  15:53:50
# eatup - process rest of statement; interpret continuations
   include  defs

   subroutine eatup

   character ptoken (MAXTOK), t, token (MAXTOK)
   character gettok
   integer nlpar, equal
   include COMMON_BLOCKS
   string serror "error"

   nlpar = 0
   token(1) = EOS

   repeat {
       call outstr (token)
       t = gettok (token, MAXTOK)
   } until (t != BLANK & t != TAB)

   if (t == ALPHA) {				# is it a "call error" stmt?
      if (equal (token, serror) == YES) {
	 # call errorc (token)
	 # return

	 # ERROR statement is now simply error checked like any other
	 # external procedure, so that it may be used the same way.
	 ername = YES
	 }
      }
   goto 10

   repeat {
      t = gettok (token, MAXTOK)
10    if (t == SEMICOL | t == NEWLINE)
	 break
      if (t == RBRACE | t == LBRACE) {
	 call pbstr (token)
	 break
	 }
      if (t == EOF) {
	 call synerr ("unexpected EOF.")
	 call pbstr (token)
	 break
	 }
      if (t == COMMA | t == PLUS | t == MINUS | t == STAR |
      (t == SLASH & body == YES) |
       t == LPAREN | t == AND | t == BAR | t == BANG | t == TILDE |
       t == NOT | t == CARET | t == EQUALS | t == UNDERLINE) {
	 while (gettok (ptoken, MAXTOK) == NEWLINE)
	    ;
	 call pbstr (ptoken)
	 if (t == UNDERLINE)
	    token (1) = EOS
	 }
      if (t == LPAREN)
	 nlpar = nlpar + 1
      else if (t == RPAREN)
	 nlpar = nlpar - 1
      if (t == ALPHA)
	 call squash (token)
      call outstr (token)
      } until (nlpar < 0)

   if (nlpar != 0)
      call synerr ("unbalanced parentheses.")

   return
   end
