# IR_VECINIT -- Procedure to initialize the intensity matching algorithm.
# If the ranges are undefined and no matching is to take place the
# ishifts are set to INDEFR and the routine returns. Otherwise the shifts
# are all initialized to zero and shifts for the missing subrasters are
# set to INDEFR.

procedure ir_vecinit (deltai, nsubrasters, ranges)

real	deltai[ARB]			# intensity shifts
int	nsubrasters			# number of subrasters
int	ranges[ARB]			# ranges of missing subrasters

int	num
int	get_next_number()

begin
	# Initialize the shifts to INDEFR.
	call amovkr (INDEFR, deltai, nsubrasters)
	if (ranges[1] == NULL)
	    return

	num = 0
	while (get_next_number (ranges, num) != EOF) {
	    if (num  > nsubrasters)
		break
	    deltai[num] = 0.0
	}
end
