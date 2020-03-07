include  defs

# gtok - get token for Ratfor

   character function gtok (lexstr, toksiz)
   character lexstr (MAXTOK)
   integer toksiz

   include COMMON_BLOCKS

   character c
   character ngetch

   integer i
#  external index
#  integer index

#  string digits "0123456789abcdefghijklmnopqrstuvwxyz"

   c = ngetch (lexstr (1))

   if (c == BLANK | c == TAB) {
      lexstr (1) = BLANK
      while (c == BLANK | c == TAB)    # compress many blanks to one
	 c = ngetch (c)
      if (c == SHARP)
	 while (ngetch (c) != NEWLINE)	 # strip comments
	    ;
      if (c != NEWLINE)
	 call putbak (c)
      else
	 lexstr (1) = NEWLINE
      lexstr (2) = EOS
      gtok = lexstr (1)
      return
      }

   i = 1
   if ((BIGA <= c & c <= BIGZ) | (LETA <= c & c <= LETZ)) {				# alpha
      gtok = ALPHA
      if (c == LETX) {				# "x$cccc" directive?
	 c = ngetch (lexstr(2))
	 if (c == DOLLAR) {
	    gtok = XPP_DIRECTIVE
	    i = 2
	    }
	 else
	    call putbak (c)
	 }

      for (;  i < toksiz - 2;  i=i+1) {
	 c = ngetch (lexstr(i+1))
	 if (!((BIGA <= c & c <= BIGZ)
	     | (LETA <= c & c <= LETZ)
	     | (DIG0 <= c & c <= DIG9)) & c != UNDERLINE)
	    break
	 }
      call putbak (c)

	} else if (DIG0 <= c & c <= DIG9) {   # digits
	    for (i=1;  i < toksiz - 2;  i=i+1) {
		c = ngetch (lexstr (i + 1))
		if (!(DIG0 <= c & c <= DIG9))
		    break
	    }
	    call putbak (c)
	    gtok = DIGIT
	}

# The following is not needed since XPP does base conversion, and this caused
# fixed point overflow on a Data General machine.
#
#      b = c - DIG0 # in case alternate base number
#      for (i = 1; i < toksiz - 2; i = i + 1) {
#	 c = ngetch (lexstr (i + 1))
#	 if (!(DIG0 <= c & c <= DIG9))
#	    break
#	 b = 10 * b + (c - DIG0)
#	 }
#      if (c == RADIX & b >= 2 & b <= 36) {   #n%ddd...
#	 n = 0
#	 repeat {
#	    d = index (digits, clower (ngetch (c))) - 1
#	    if (d < 0)
#	       break
#	    n = b * n + d
#	    }
#	 call putbak (c)
#	 i = itoc (n, lexstr, toksiz)
#	 }
#      else
#	 call putbak (c)
#      gtok = DIGIT
#      }

   else if (c == LBRACK) {   # allow [ for {
      lexstr (1) = LBRACE
      gtok = LBRACE
      }

   else if (c == RBRACK) {   # allow ] for }
      lexstr (1) = RBRACE
      gtok = RBRACE
      }

   else if (c == DOLLAR) {    # $( and $) now used by macro processor
      if (ngetch (lexstr (2)) == LPAREN) {
	 i = 2
	 gtok = LSTRIPC
	 }
      else if (lexstr (2) == RPAREN) {
	 i = 2
	 gtok = RSTRIPC
	 }
      else {
	 call putbak (lexstr (2))
	 gtok = DOLLAR
	 }
      }

   else if (c == SQUOTE | c == DQUOTE) {
      gtok = c
      for (i = 2; ngetch (lexstr (i)) != lexstr (1); i = i + 1) {
	 if (lexstr (i) == UNDERLINE)
	    if (ngetch (c) == NEWLINE) {
	       while (c == NEWLINE | c == BLANK | c == TAB)
		  c = ngetch (c)
	       lexstr (i) = c
	       }
	    else
	       call putbak (c)
	 if (lexstr (i) == NEWLINE | i >= toksiz - 1) {
	    call synerr ("missing quote.")
	    lexstr (i) = lexstr (1)
	    call putbak (NEWLINE)
	    break
	    }
	 }
      }

   else if (c == SHARP) {   # strip comments
      while (ngetch (lexstr (1)) != NEWLINE)
	 ;
      gtok = NEWLINE
      }

   else if (c == GREATER | c == LESS | c == NOT | c == BANG |
    c == TILDE | c == CARET | c == EQUALS | c == AND | c == OR) {
      call relate (lexstr, i)
      gtok = c
      }

   else
      gtok = c

   if (i >= toksiz - 1)
      call synerr ("token too long.")
   lexstr (i + 1) = EOS

   # Note:  line number accounting is now done in 'ngetch'

   return
   end
