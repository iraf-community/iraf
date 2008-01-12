#-h-  deftok			 4116  local   12/01/80  15:53:47
# deftok - get token; process macro calls and invocations
   include  defs

# this routine has been disabled to allow defines with parameters to be added

#   character function deftok (token, toksiz)
#   character gtok
#   integer toksiz
#   character defn (MAXDEF), t, token (MAXTOK)
#   integer ludef
#   include COMMON_BLOCKS
#
#   for (t = gtok (token, toksiz); t!=EOF; t = gtok (token, toksiz)) {
#      if (t != ALPHA)	 # non-alpha
#	  break
#      if (ludef (token, defn, deftbl) == NO)	  # undefined
#	  break
#      if (defn (1) == DEFTYPE) {   # get definition
#	  call getdef (token, toksiz, defn, MAXDEF)
#	  call entdef (token, defn, deftbl)
#	  }
#      else
#	  call pbstr (defn)   # push replacement onto input
#      }
#   deftok = t
#   if (deftok == ALPHA)   # convert to single case
#      call fold (token)
#   return
#   end
# deftok - get token; process macro calls and invocations

   character function deftok (token, toksiz)
   character token (MAXTOK)
   integer toksiz

   include COMMON_BLOCKS

   character t, c, defn (MAXDEF), mdefn (MAXDEF)
   character gtok
   integer equal

   integer ap, argstk (ARGSIZE), callst (CALLSIZE),
      nlb, plev (CALLSIZE), ifl
   integer ludef, push, ifparm

   string balp "()"
   string pswrg "switch_no_range_check"

   cp = 0
   ap = 1
   ep = 1
   for (t = gtok (token, toksiz); t != EOF; t = gtok (token, toksiz)) {
      if (t == ALPHA)
	 if (ludef (token, defn, deftbl) == NO) {
	    if (cp == 0)
	       break
	    else
	       call puttok (token)
	 } else if (defn (1) == DEFTYPE) {     # process defines directly
	    call getdef (token, toksiz, defn, MAXDEF)
	    call entdef (token, defn, deftbl)
	 } else if (defn (1) == IFDEFTYPE | defn (1) == IFNOTDEFTYPE) {
	    c = defn (1)
	    call getdef (token, toksiz, defn, MAXDEF)
	    ifl = ludef (token, mdefn, deftbl)
	    if ((ifl == YES & c == IFDEFTYPE) |
	     (ifl == NO & c == IFNOTDEFTYPE))
	       call pbstr (defn)

	 } else if (defn(1) == PRAGMATYPE & cp == 0) {   # pragma
	    if (gtok (defn, MAXDEF) == BLANK) {
		if (gtok (defn, MAXDEF) == ALPHA) {
		    if (equal (defn, pswrg) == YES)
			swinrg = YES
		    else
			goto 10
		} else {
10		    call pbstr (defn)
		    call putbak (BLANK)
		    break
		}
	    } else {
		call pbstr (defn)
		break
	    }
		    
	 } else {
	    cp = cp +  1
	    if (cp > CALLSIZE)
	       call baderr ("call stack overflow.")
	    callst (cp) = ap
	    ap = push (ep, argstk, ap)
	    call puttok (defn)
	    call putchr (EOS)
	    ap = push (ep, argstk, ap)
	    call puttok (token)
	    call putchr (EOS)
	    ap = push (ep, argstk, ap)
	    t = gtok (token, toksiz)
	    if (t == BLANK) {		  # allow blanks before arguments
	       t = gtok (token, toksiz)
	       call pbstr (token)
	       if (t != LPAREN)
		  call putbak (BLANK)
	       }
	    else
	       call pbstr (token)
	    if (t != LPAREN)
	       call pbstr (balp)
	    else if (ifparm (defn) == NO)
	       call pbstr (balp)
	    plev (cp) = 0
	} else if (t == LSTRIPC) {
	 nlb = 1
	 repeat {
	    t = gtok (token, toksiz)
	    if (t == LSTRIPC)
	       nlb = nlb + 1
	    else if (t == RSTRIPC) {
	       nlb = nlb - 1
	       if (nlb == 0)
		  break
	       }
	    else if (t == EOF)
	    call baderr ("EOF in string.")
	    call puttok (token)
	    }
        }
      else if (cp == 0)
	 break
      else if (t == LPAREN) {
	 if (plev (cp) > 0)
	    call puttok (token)
	 plev (cp) = plev (cp) + 1
	 }
      else if (t == RPAREN) {
	 plev (cp) = plev (cp) - 1
	 if (plev (cp) > 0)
	    call puttok (token)
	 else {
	    call putchr (EOS)
	    call evalr (argstk, callst (cp), ap - 1)
	    ap = callst (cp)
	    ep = argstk (ap)
	    cp =  cp - 1
	    }
	 }
      else if (t == COMMA & plev (cp) == 1) {
	 call putchr (EOS)
	 ap = push (ep, argstk, ap)
	 }
      else
	 call puttok (token)
      }

   deftok = t
   if (t == ALPHA)
      call fold (token)

   return
   end
