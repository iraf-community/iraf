#-h-  forcod			 2259  local   12/01/80  15:54:07
# forcod - beginning of for statement
   include  defs

   subroutine forcod (lab)
   integer lab

   include COMMON_BLOCKS

   character t, token (MAXTOK)
   character gettok, gnbtok

   integer i, j, nlpar
   integer length, labgen

   string ifnot "if (.not."
   string serrchk ".and.(.not.xerflg))) "

   lab = labgen (3)
   call outcon (0)
   if (gnbtok (token, MAXTOK) != LPAREN) {
      call synerr ("missing left paren.")
      return
      }
   if (gnbtok (token, MAXTOK) != SEMICOL) {   # real init clause
      call pbstr (token)
      call outtab
      call eatup
      call outdwe
      }
   if (gnbtok (token, MAXTOK) == SEMICOL)   # empty condition
      call outcon (lab)
   else {   # non-empty condition
      call pbstr (token)
      call outnum (lab)
      call outtab
      call outstr (ifnot)
      call outch (LPAREN)
      nlpar = 0
      while (nlpar >= 0) {
	 t = gettok (token, MAXTOK)
	 if (t == SEMICOL)
	    break
	 if (t == LPAREN)
	    nlpar = nlpar + 1
	 else if (t == RPAREN)
	    nlpar = nlpar - 1
	 if (t == EOF) {
	    call pbstr (token)
	    return
	    }
	 if (t == ALPHA)
	    call squash (token)
	 if (t != NEWLINE & t != UNDERLINE)
	    call outstr (token)
	 }

      # name encountered for which error checking is required?
      if (ername == YES)
	 call outstr (serrchk)
      else {
	  call outch (RPAREN)
	  call outch (RPAREN)
	  call outch (BLANK)
	  }
      call outgo (lab+2)			# error checking below (errgo)
      if (nlpar < 0)
	 call synerr ("invalid for clause.")
      }
   fordep = fordep + 1	 # stack reinit clause
   j = 1
   for (i = 1; i < fordep; i = i + 1)	# find end
      j = j + length (forstk (j)) + 1
   forstk (j) = EOS   # null, in case no reinit
   nlpar = 0
   t = gnbtok (token, MAXTOK)
   call pbstr (token)
   while (nlpar >= 0) {
      t = gettok (token, MAXTOK)
      if (t == LPAREN)
	 nlpar = nlpar + 1
      else if (t == RPAREN)
	 nlpar = nlpar - 1
      if (t == EOF) {
	 call pbstr (token)
	 break
	 }
      if (nlpar >= 0 & t != NEWLINE & t != UNDERLINE) {
	 if (t == ALPHA)
	    call squash (token)
	 if (j + length (token) >= MAXFORSTK)
	    call baderr ("for clause too long.")
	 call scopy (token, 1, forstk, j)
	 j = j + length (token)
	 }
      }
   lab = lab + 1   # label for next's
   call indent (1)
   call errgo
   return
   end
