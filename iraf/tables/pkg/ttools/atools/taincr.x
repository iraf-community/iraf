# This file contains taex_incr and taex_disaster, which are used by
# both tainsert and taextract.
#
# Phil Hodge,  7-Mar-1996  Extracted from taextract.x.
# Phil Hodge, 29-Jul-1997  In taex_disaster, get table name before closing
#			the table; rename delete to delete_flag to avoid
#			confusion with the delete subroutine.

# taex_incr -- increment variables

# On input, ncopy is the number of elements that were copied in
# the previous step.  We decrement nremain and increment first by
# this amount.  Then we determine the appropriate value of ncopy
# for the next step and update last.

procedure taex_incr (nremain, ncopy, first, last, bufsize)

int	nremain		# io: number of elements remaining to be copied
int	ncopy		# io: number of elements copied/to copy next
int	first		# io: first element (or row number)
int	last		# io: last element (or row number)
int	bufsize		# i: maximum number to copy in one step

begin
	nremain = nremain - ncopy
	first = first + ncopy
	ncopy = min (nremain, bufsize)
	last = first + ncopy - 1
end

# taex_disaster -- clean up and call error

procedure taex_disaster (itp, otp, delete_flag, message)

pointer itp, otp	# io: pointers to table struct
int	delete_flag	# i: YES if we should delete the output table
char	message[ARB]	# i: error message
#--
pointer sp
pointer outtable	# scratch for name of output table

begin
	call tbtclo (itp)
	if (delete_flag == YES) {
	    call smark (sp)
	    call salloc (outtable, SZ_FNAME, TY_CHAR)
	    call tbtnam (otp, Memc[outtable], SZ_FNAME)
	    call tbtclo (otp)
	    call tbtdel (Memc[outtable])
	    call sfree (sp)
	} else {
	    call tbtclo (otp)
	}
	call error (1, message)
end
