#-h-  cascod			 1876  local   12/01/80  15:53:46
# cascod - generate code for case or default label
   include  defs

   subroutine cascod (lab, token)
   integer lab, token

   include COMMON_BLOCKS

   integer t, l, lb, ub, i, j, junk
   integer caslab, labgen, gnbtok

   character tok (MAXTOK)

   if (swtop <= 0) {
      call synerr ("illegal case or default.")
      return
      }
   call indent (-1)
   call outgo (lab + 1) # terminate previous case
   xfer = YES
   l = labgen (1)
   if (token == LEXCASE) { # case n[,n]... : ...
      while (caslab (lb, t) != EOF) {
	 ub = lb
	 if (t == MINUS)
	    junk = caslab (ub, t)
	 if (lb > ub) {
	    call synerr ("illegal range in case label.")
	    ub = lb
	    }
	 if (swlast + 3 > MAXSWITCH)
	    call baderr ("switch table overflow.")
	 for (i = swtop + 3; i < swlast; i = i + 3)
	    if (lb <= swstak (i))
	       break
	    else if (lb <= swstak (i+1))
	       call synerr ("duplicate case label.")
	 if (i < swlast & ub >= swstak (i))
	    call synerr ("duplicate case label.")
	 for (j = swlast; j > i; j = j - 1)   # insert new entry
	    swstak (j+2) = swstak (j-1)
	 swstak (i) = lb
	 swstak (i + 1) = ub
	 swstak (i + 2) = l
	 swstak (swtop + 1) = swstak (swtop + 1)  +  1
	 swlast = swlast + 3
	 if (t == COLON)
	    break
	 else if (t != COMMA)
	    call synerr ("illegal case syntax.")
	 }
      }
   else {   # default : ...
      t = gnbtok (tok, MAXTOK)
      if (swstak (swtop + 2) != 0)
	 call error ("multiple defaults in switch statement.")
      else
	 swstak (swtop + 2) = l
      }

   if (t == EOF)
      call synerr ("unexpected EOF.")
   else if (t != COLON)
      call error ("missing colon in case or default label.")

   xfer = NO
   call outcon (l)
   call indent (1)
   return
   end
