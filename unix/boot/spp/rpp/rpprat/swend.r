#-h-  swend			 2714  local   12/01/80  15:55:07
# swend - finish off switch statement; generate dispatch code
   include  defs

   subroutine swend (lab)
   integer lab

   include COMMON_BLOCKS

   integer lb, ub, n, i, j, swn

   string sif "if ("
   string slt ".lt.1.or."
   string sgt ".gt."
   string sgoto "goto ("
   string seq ".eq."
   string sge ".ge."
   string sle ".le."
   string sand ".and."

   swn = swvstk(swvlev)		#get switch variable number, SWnnnn
   swvlev = max(0, swvlev - 1)

   lb = swstak (swtop + 3)
   ub = swstak (swlast - 2)
   n = swstak (swtop + 1)
   call outgo (lab + 1) # terminate last case
   if (swstak (swtop + 2) == 0)
      swstak (swtop + 2) = lab + 1  # default default label
   xfer = NO
   call indent (-1)
   call outcon (lab)  # L   continue
   call indent (1)
   if (n >= CUTOFF & ub - lb + 1 < DENSITY * n) { # output branch table
      if (lb != 1) {  # L  Innn=Innn-lb+1
	 call outtab
	 call swvar (swn)
	 call outch (EQUALS)
	 call swvar (swn)
	 if (lb < 1)
	    call outch (PLUS)
	 call outnum (-lb + 1)
	 call outdon
	 }
      if (swinrg == NO) {
	  call outtab  #  if (Innn.lt.1.or.Innn.gt.ub-lb+1)goto default
	  call outstr (sif)
	  call swvar (swn)
	  call outstr (slt)
	  call swvar (swn)
	  call outstr (sgt)
	  call outnum (ub - lb + 1)
	  call outch (RPAREN)
	  call outch (BLANK)
	  call outgo (swstak (swtop + 2))
      }
      call outtab  #  goto (....),Innn
      call outstr (sgoto)
      j = lb
      for (i = swtop + 3; i < swlast; i = i + 3) {
	 for ( ; j < swstak (i); j = j + 1) { # fill in vacancies
	    call outnum (swstak (swtop + 2))
	    call outch (COMMA)
	    }
	 for (j = swstak (i + 1) - swstak (i); j >= 0; j = j - 1)
	    call outnum (swstak (i + 2)) # fill in range
	 j = swstak (i + 1) + 1
	 if (i < swlast - 3)
	    call outch (COMMA)
	 }
      call outch (RPAREN)
      call outch (COMMA)
      call swvar (swn)
      call outdon
      }
   else if (n > 0) {  # output linear search form
      for (i = swtop + 3; i < swlast; i = i + 3) {
	 call outtab  # if (Innn
	 call outstr (sif)
	 call swvar (swn)
	 if (swstak (i) == swstak (i+1)) {
	    call outstr (seq) #   .eq....
	    call outnum (swstak (i))
	    }
	 else {
	    call outstr (sge) #   .ge.lb.and.Innn.le.ub
	    call outnum (swstak (i))
	    call outstr (sand)
	    call swvar (swn)
	    call outstr (sle)
	    call outnum (swstak (i + 1))
	    }
	 call outch (RPAREN) #	  ) goto ...
	 call outch (BLANK)
	 call outgo (swstak (i + 2))
	 }
      if (lab + 1 != swstak (swtop + 2))
	 call outgo (swstak (swtop + 2))
      }
   call indent (-1)
   call outcon (lab + 1)   # L+1  continue
   swlast = swtop # pop switch stack
   swtop = swstak (swtop)
   swinrg = NO
   return
   end
