include	<math.h>

#* HISTORY *
#* B.Simon	24-Aug-94	original
#* B.Simon	18-Sep-00	Revised computation of proj and added abnorm

# GETNORM -- Compute the squared norm between two table rows

procedure getnorm (in1, in2, ncol, col1, col2, row1, row2, weight, sphere, 
		   proj, abnorm, norm)

pointer	in1		# i: first table descriptor
pointer	in2		# i: second table descriptor
int	ncol		# i: number of match columns
pointer	col1[ARB]	# i: match columns in first table
pointer	col2[ARB]	# i: match columns in second table
int	row1		# i: row number in first table
int	row2		# i: row number in second table
double	weight[ARB]	# i: weights used in computing norm
bool	sphere		# i: apply spherical correction to first column?
double	proj		# o: projection of norm on first axis
double	abnorm		# o: norm, possibly without spherical correction
double	norm		# o: norm (distance) between rows in two tables
#--
int	i
double	val1, val2, dif

begin
	# Calculate first component of norm

	call tbegtd (in1, col1[1], row1, val1)
	call tbegtd (in2, col2[1], row2, val2)

	dif = weight[1] * (val1 - val2)
	proj = dif * dif
	abnorm = proj

	# Apply correction for spherical coordinates

	if (sphere) {
	    if (dif > 180) {
		dif = dif - 360
	    } else if (dif < -180) {
		dif = dif + 360
	    }

	    call tbegtd (in1, col1[2], row1, val1)
	    call tbegtd (in2, col2[2], row2, val2)

	    val1 = 0.5 * weight[2] * (val1 + val2)
	    dif = dif * cos (DEGTORAD(val1))
	}

	# Compute remaining components

	norm = dif * dif
	do i = 2, ncol {
	    call tbegtd (in1, col1[i], row1, val1)
	    call tbegtd (in2, col2[i], row2, val2)

	    dif = weight[i] * (val1 - val2)
	    abnorm = abnorm + dif * dif
	    norm = norm + dif * dif
	}

end

