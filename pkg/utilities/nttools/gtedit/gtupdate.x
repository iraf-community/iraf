# GT_UPDATE -- Delete points currently marked for deletion and update data

procedure gt_update (tp, tpr, x, y, deleted, npix)

pointer	tp, tpr
real	x[ARB]
real	y[ARB]
int	npix
int	deleted[ARB]

int	i, j, ndelete

begin
	# Delete the points
	call gt_dodel (tp, tpr, deleted, npix)

	# Update data arrays j = 0
	ndelete = 0
	for (i = 1; i <= npix; i = i + 1) {
	    j = j + 1
	    if (deleted[i] == YES) {
		ndelete = ndelete + 1
		i = i + 1
		while (deleted[i] == YES) {
		    ndelete = ndelete + 1
		    i = i + 1
		}
	    }
	    x[j] = x[i]
	    y[j] = y[i]
	}

	call aclri (deleted, npix)
	npix = npix - ndelete
end

