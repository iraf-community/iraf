define	SZ_COLVAL	SZ_LINE

# MKRULES -- Add a new rule to the target and action tables
#
# B.Simon	25-Apr-88	Original

procedure mkrules (work, target, action)

pointer	work		# i: Table containing parser results
pointer	target		# i: Table containing patterns to be matched
pointer	action		# i: Table containing possible expansions
#--
int	nwork, naction, ntarget, iwork, iaction

int	numstack()

errchk	movtbrow, putstacki

begin
	# Check for null rules

	nwork = numstack (work)
	if (nwork <= 0)
	    return

	# Move the first row from the work table to the target table

	call pushstack (target)
	naction = numstack (action)
	ntarget = numstack (target)

	call movtbrow (work, 1, target, ntarget)
	call putstacki (target, "_FIRST", naction+1)
	call putstacki (target, "_LAST", naction+nwork-1)
	call putstacki (target, "_USED", NO)

	# Move the remaining rows to the action table

	iaction = naction
	do iwork = 2, nwork {
	    call pushstack (action)
	    iaction = iaction + 1
	    call movtbrow (work, iwork, action, iaction)
	}

	call tbrdel (work, 1, nwork)

end
