#-h-  strdcl			 2575  local   12/01/80  15:55:05
# strdcl - generate code for string declaration
   include  defs

   subroutine strdcl

   include COMMON_BLOCKS

   character t, token (MAXTOK), dchar (MAXTOK)
   character gnbtok

   integer i, j, k, n, len
   integer length, ctoi, lex

   string char "integer*2/"
   string dat  "data "
   string eoss "0/"

   t = gnbtok (token, MAXTOK)
   if (t != ALPHA)
      call synerr ("missing string token.")
   call squash (token)
   call outtab
   call pbstr (char) # use defined meaning of "character"
   repeat {
      t = gnbtok (dchar, MAXTOK)
      if (t == SLASH)
	 break
      call outstr (dchar)
      }
   call outch (BLANK)  # separator in declaration
   call outstr (token)
   call addstr (token, sbuf, sbp, SBUFSIZE)  # save for later
   call addchr (EOS, sbuf, sbp, SBUFSIZE)
   if (gnbtok (token, MAXTOK) != LPAREN) {  # make size same as initial value
      len = length (token) + 1
      if (token (1) == SQUOTE | token (1) == DQUOTE)
	 len = len - 2
      }
   else { # form is string name (size) init
      t = gnbtok (token, MAXTOK)
      i = 1
      len = ctoi (token, i)
      if (token (i) != EOS)
	 call synerr ("invalid string size.")
      if (gnbtok (token, MAXTOK) != RPAREN)
	 call synerr ("missing right paren.")
      else
	 t = gnbtok (token, MAXTOK)
      }
   call outch (LPAREN)
   call outnum (len)
   call outch (RPAREN)
   call outdon
   if (token (1) == SQUOTE | token (1) == DQUOTE) {
      len = length (token)
      token (len) = EOS
      call addstr (token (2), sbuf, sbp, SBUFSIZE)
      }
   else
      call addstr (token, sbuf, sbp, SBUFSIZE)
   call addchr (EOS, sbuf, sbp, SBUFSIZE)
   t = lex (token)   # peek at next token
   call pbstr (token)
   if (t != LEXSTRING) {   # dump accumulated data statements
      for (i = 1; i < sbp; i = j + 1) {
	 call outtab
	 call outstr (dat)
	 k = 1
	 for (j = i + length (sbuf (i)) + 1; ; j = j + 1) {
	    if (k > 1)
	       call outch (COMMA)
	    call outstr (sbuf (i))
	    call outch (LPAREN)
	    call outnum (k)
	    call outch (RPAREN)
	    call outch (SLASH)
	    if (sbuf (j) == EOS)
	       break
	    n = sbuf (j)
	    call outnum (n)
	    call outch (SLASH)
	    k = k + 1
	    }
	 call pbstr (eoss) # use defined meaning of EOS
	 repeat {
	    t = gnbtok (token, MAXTOK)
	    call outstr (token)
	    } until (t == SLASH)
	 call outdon
	 }
      sbp = 1
      }

   return
   end
