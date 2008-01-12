include <tbset.h>

# GT_DODEL -- Actually delete the rows marked for deletion

procedure gt_dodel (tp, tpr, deleted, npix)

pointer	tp
pointer	tpr
int	deleted[ARB]		# io: Array of deleted flags
int	npix			# io: # of rows in table

int	i, j, k

int	tbpsta()

begin
	if (tpr != NULL) {
	    # Append to whatever is already in the table
	    k = tbpsta (tpr, TBL_NROWS)
	    do i = 1, npix {
		if (deleted[i] == YES) {
		    k = k + 1
		    call tbrcpy (tp, tpr, i, k)
		}
	    }
	}

	for (j = npix; j> 0; j = j - 1) {
	    if (deleted[j] == YES) {
		i = j
		while (deleted[i] == YES) {
		    i = i - 1
		    if (i < 1)
			break
		}
		i = i + 1
		call tbrdel (tp, i, j)
		j = i - 1
	    }
	}
end
