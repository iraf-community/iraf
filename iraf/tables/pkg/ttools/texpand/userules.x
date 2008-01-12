include	<tbset.h>

# USE_RULES -- Use the rules to expand the input table rows
#
# B.Simon	25-Apr-88	Original
# B.Simon	21-Jan-99	Modified to handle empty target tables

procedure use_rules (itp, otp, target, action, dbg, verbose)

pointer	itp		# i: Input table
pointer otp		# i: Output table
pointer	target		# u: Table of rule targets
pointer	action		# u: Table of rule actions
int	dbg		# i: Debug file descriptor
bool	verbose		# i: Print diagnostic message
#--
int	top, nrow, irow
pointer	work

int	tbpsta()
int	initstack(), numstack(), find_rule(), apply_rule()

string	tgtname "The following is the target table:"
string	actname "The following is the action table:"
string	isstart "The following row is read from the input table:"
string	isdone  "The following row is moved to the output table:"

begin
	# Do straight copy if target table is empty

	top = numstack (target)
	if (top == 0) {
	    call no_rule (itp, otp)
	    return
	}

	# Print target and action tables

	call dbg_rules (target, tgtname, 1, top, dbg)

	top = numstack (action)
	call dbg_rules (action, actname, 1, top, dbg)

	# Create a work table, which is used to store
	# intermediate results

	work = initstack (itp, "_TARGET,_INDEX")

	# Loop over each row in the input table

	nrow = tbpsta (itp, TBL_NROWS)
	do irow = 1, nrow {

	    call dbg_rules (itp, isstart, irow, irow, dbg)

	    # Push the next row from the input table
	    # into the work table. If it does not match
	    # any rule, write it to the output table.

	    call pushstack (work)
	    call movtbrow (itp, irow, work, 1)
	    if (find_rule (target, work) == 0) {
		top = numstack (work)
		call dbg_rules (work, isdone, top, top, dbg)
		call movstack (work, otp)
	    }

	    # Apply the next instance of the rule to the
	    # row on top of the stack. If the result of the
	    # application of the rule does not match any other
	    # rule, write it to the output table.

	    while (numstack (work) > 0) {
		if (apply_rule (target, action, work, dbg) == 0) {
		    top = numstack (work)
		    call dbg_rules (work, isdone, top, top, dbg)
		    call movstack (work, otp)
		    if (verbose && mod (numstack (otp), 25) == 0) {
			call printf ("\r%d rows written to output table")
			    call pargi (numstack (otp))
			call flush (STDOUT)
		    }
		}
	    }
	}

	if (verbose) {
	    call printf ("\r%39w\r")
	    call flush (STDOUT)
	}

	call freestack (target)
	call freestack (action)
	call freestack (work)
end

# APPLY_RULE -- Expand the top work table row according to a rule

int procedure apply_rule (target, action, work, dbg)

pointer	target		# i: Table of rule targets
pointer	action		# i: Table of rule actions
pointer	work		# i: Table of intermediate results
int	dbg		# i: Debug file descriptor
#--
int	wrow, trow, arow, last, rule
pointer	tgt_ptr, idx_ptr, lst_ptr, use_ptr

string	isrule  "The following rule is applied:"
string	notdone "To produce the row:"

int	numstack(), find_rule()

begin
	# Get column pointers of special columns

	call tbcfnd (work, "_TARGET", tgt_ptr, 1)
	call tbcfnd (work, "_INDEX", idx_ptr, 1)
	call tbcfnd (target, "_LAST", lst_ptr, 1)

	# Get the current row numbers for the work, target,
	# and action tables

	wrow = numstack (work)
	call tbegti (work, tgt_ptr, wrow, trow)
	call tbegti (work, idx_ptr, wrow, arow)
	call tbegti (target, lst_ptr, trow, last)

	# If the action row number is greater than the last action
	# associated with the target, all the expansions for this
	# rule have been performed. Pop the work table and mark the
	# target row as unused.

	if (arow > last) {
	    call popstack (work)
	    call tbcfnd (target, "_USED", use_ptr, 1)
	    call tbepti (target, use_ptr, trow, NO)
	    rule = trow

	# Otherwise, duplicate the top row of the work table and
	# overwrite the appropriate columns with the values stored
	# in the action row. Increment the action row for next time.
	# Initialize the special columns in the new row of the work
	# table.

	} else {
	    call pushstack (work)
	    call movtbrow (work, wrow, work, wrow+1)
	    call movtbrow (action, arow, work, wrow+1)
	    call tbepti (work, idx_ptr, wrow, arow+1) ## should be wrow+1 ??
	    call dbg_rules (target, isrule, trow, trow, dbg)
	    call dbg_rules (work, notdone, wrow+1, wrow+1, dbg)
	    rule = find_rule (target, work)
	}

	return (rule)
end

# FIND_RULE -- Find the target row which matches the top work table row

int procedure find_rule (target, work)

pointer	target		# i: Table of rule targets
pointer	work		# i: Table of intermediate results
#--
bool	match, nullflg
int	icol, jcol, tcol, wcol, trow, irow, wrow, used, first
pointer	sp, tarptr, wrkptr, colname, tarval, wrkval
pointer	use_ptr, fst_ptr, tgt_ptr, idx_ptr, iw

bool	strne()
int	tbpsta(), strlen()
pointer	tbcnum(), numstack()

begin
	# Get number of columns in tables

	tcol = tbpsta (target, TBL_NCOLS)
	wcol = tbpsta (work, TBL_NCOLS)

	# Allocate dynamic memory

	call smark (sp)
	call salloc (tarptr, tcol, TY_INT)
	call salloc (wrkptr, tcol, TY_INT)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (tarval, SZ_LINE, TY_CHAR)
	call salloc (wrkval, SZ_LINE, TY_CHAR)

	# Create arrays of corresponding column pointers
	# in the target and work tables

	jcol = 0
	do icol = 1, tcol {
	    Memi[tarptr+jcol] = tbcnum (target, icol)
	    call tbcigt (Memi[tarptr+jcol), TBL_COL_NAME,
			 Memc[colname], SZ_COLNAME)
	    call tbcfnd (work, Memc[colname], Memi[wrkptr+jcol], 1)
	    if (Memc[colname] != '_' && Memi[wrkptr+jcol] != NULL)
		jcol = jcol + 1
	}

	# Get pointers to special columns

	call tbcfnd (target, "_USED", use_ptr, 1)
	call tbcfnd (target, "_FIRST", fst_ptr, 1)
	call tbcfnd (work, "_TARGET", tgt_ptr,1)
	call tbcfnd (work, "_INDEX", idx_ptr, 1)

	# Search for a match in the target table
	# with the top row of the work table

	match = false
	wrow = numstack (work)
	trow = tbpsta (target, TBL_NROWS)
	do irow = 1, trow {

	    call tbegti (target, use_ptr, irow, used)
	    if (used == NO) {

		# Compare each non-null column of the target row
		# to the work row

		match = true
		do icol = 1, jcol {
		    call tbrgtt (target, Memi[tarptr+icol-1], Memc[tarval], 
				 nullflg, SZ_LINE, 1, irow)

		    if (! nullflg) {
			call tbegtt (work, Memi[wrkptr+icol-1], wrow, 
				     Memc[wrkval], SZ_LINE)

			iw = strlen (Memc[wrkval]) + wrkval - 1
			while (Memc[iw] == ' ')
			    iw = iw - 1
			Memc[iw+1] = EOS

			if (strne (Memc[tarval], Memc[wrkval])) {
			    match = false
			    break
			}
		    }
		}

		# If the rows match, mark the target row as used
		# and initialize the special columns in the work row

		if (match) {

		    call tbepti (target, use_ptr, irow, YES)

		    call tbegti (target, fst_ptr, irow, first)
		    call tbepti (work, idx_ptr, wrow, first)
		    call tbepti (work, tgt_ptr, wrow, irow)

		    break
		}
	    }
	}

	call sfree (sp)

	# If a match was found, return the target row number matched

	if (match)
	    return (irow)
	else
	    return (0)
end

# NO_RULE -- Do a straight copy when ther are no expansion rules

procedure no_rule (itp, otp)

pointer	itp		# i: Input table
pointer otp		# i: Output table
#--
int	irow, nrow
int	tbpsta()

begin
	nrow = tbpsta (itp, TBL_NROWS)

	do irow = 1, nrow 
	    call tbrcpy (itp,otp, irow, irow)
end
