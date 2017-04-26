#-h-  getdef			 1634  local   12/01/80  15:54:08
# getdef (for no arguments) - get name and definition
   include  defs

   subroutine getdef (token, toksiz, defn, defsiz)
   character token (MAXTOK), defn (MAXDEF)
   integer toksiz, defsiz

   include COMMON_BLOCKS

   character c, t, ptoken (MAXTOK)
   character gtok, ngetch

   integer i, nlpar

   call skpblk
   c = gtok (ptoken, MAXTOK)
   if (c == LPAREN)
      t = LPAREN	     # define (name, defn)
   else {
      t = BLANK 	     # define name defn
      call pbstr (ptoken)
      }
   call skpblk
   if (gtok (token, toksiz) != ALPHA)
      call baderr ("non-alphanumeric name.")
   call skpblk
   c = gtok (ptoken, MAXTOK)
   if (t == BLANK) {	     # define name defn
      call pbstr (ptoken)
      i = 1
      repeat {
	 c = ngetch (c)
	 if (i > defsiz)
	    call baderr ("definition too long.")
	 defn (i) = c
	 i = i + 1
	 } until (c == SHARP | c == NEWLINE | c == EOF)
      if (c == SHARP)
	 call putbak (c)
      }
   else if (t == LPAREN) {   # define (name, defn)
      if (c != COMMA)
	 call baderr ("missing comma in define.")
      # else got (name,
      nlpar = 0
      for (i = 1; nlpar >= 0; i = i + 1)
	 if (i > defsiz)
	    call baderr ("definition too long.")
	 else if (ngetch (defn (i)) == EOF)
	    call baderr ("missing right paren.")
	 else if (defn (i) == LPAREN)
	    nlpar = nlpar + 1
	 else if (defn (i) == RPAREN)
	    nlpar = nlpar - 1
	 # else normal character in defn (i)
      }
   else
      call baderr ("getdef is confused.")
   defn (i - 1) = EOS
   return
   end
