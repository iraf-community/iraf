#-h-  lex			  543  local   12/01/80  15:54:12
# lex - return lexical type of token
   include  defs

   integer function lex (lexstr)
   character lexstr (MAXTOK)

   include COMMON_BLOCKS

   character gnbtok, t, c

   integer lookup, n
   string sdefault "default"

   for (lex = gnbtok (lexstr, MAXTOK);  lex == NEWLINE;
        lex = gnbtok (lexstr, MAXTOK))
      ;

   if (lex == EOF | lex == SEMICOL | lex == LBRACE | lex == RBRACE)
      return
   if (lex == DIGIT)
      lex = LEXDIGITS
   else if (lex == TOGGLE)
      lex = LEXLITERAL
   else if (lex == XPP_DIRECTIVE)
      lex = LEXDECL
   else if (lookup (lexstr, lex, rkwtbl) == YES) {
      if (lex == LEXDEFAULT) {				# "default:"
	 n = -1
	 repeat {
	    c = ngetch (c)
	    n = n + 1
	 } until (c != BLANK & c != TAB)
	 call putbak (c)

	 t = gnbtok (lexstr, MAXTOK)
	 call pbstr (lexstr)
	 if (n > 0)
	    call putbak (BLANK)
	 call scopy (sdefault, 1, lexstr, 1)
	 if (t != COLON)
	    lex = LEXOTHER
	 }
      }
   else
      lex = LEXOTHER

   return
   end
