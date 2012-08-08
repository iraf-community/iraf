include  defs

# PBSTR -- Push string back onto input.

subroutine pbstr (s)

character s(ARB)			# string to be pushed back.
integer lenstr, i
integer length

#begin
	lenstr = length (s)
	
	# We are called to push back tokens returned by GTOK, which converts
	# the ratfor relational operators >, >=, &, etc. into their Fortran
	# equivalents .gt., .ge., .and., and so on.  This conversion must be
	# reversed in the push back to prevent macro expansion from operating
	# on the strings "gt", "ge, "and", etc.  This is a stupid way to
	# handle this but this ratfor code (which was free) is a hopeless mess
	# already anyhow.

	if (s(1) == PERIOD & s(lenstr) == PERIOD)
	    if (lenstr == 4) {
		if (s(2) == LETG) {
		    if (s(3) == LETT) {			# .gt.
			call putbak (GREATER)
			return
		    } else if (s(3) == LETE) {		# .ge.
			# Note chars are pushed back in
			# reverse order.
			call putbak (EQUALS)
			call putbak (GREATER)
			return
		    }
		} else if (s(2) == LETL) {
		    if (s(3) == LETT) {			# .lt.
			call putbak (LESS)
			return
		    } else if (s(3) == LETE) {		# .le.
			call putbak (EQUALS)
			call putbak (LESS)
			return
		    }
		} else if (s(2) == LETE & s(3) == LETQ) {
		    call putbak (EQUALS)		# .eq.
		    call putbak (EQUALS)
		    return
		} else if (s(2) == LETN & s(3) == LETE) {
		    call putbak (EQUALS)		# .ne.
		    call putbak (BANG)
		    return
		} else if (s(2) == LETO & s(3) == LETR) {
		    call putbak (OR)			# .or.
		    return
		}
	    } else if (lenstr == 5) {
		if (s(2) == LETN & s(3) == LETO & s(4) == LETT) {
		    call putbak (BANG)			# .not.
		    return
		} else if (s(2) == LETA & s(3) == LETN & s(4) == LETD) {
		    call putbak (AND)			# .and.
		    return
		}
	    }

	# Push back an arbitrary string.
	for (i=lenstr;  i > 0;  i=i-1)
	    call putbak (s(i))
end
