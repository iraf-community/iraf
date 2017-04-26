include	"ms.h"

# SOLVE:
# Solve for the parameter correction vector using the banded matrix
# technique decribed in Lawson and Hanson.
#
# The variables g, mdg, nb, ip, ir, mt, jt, rnorm, x and n have the
# same meaning as described in Lawson and Hanson.

procedure solve (ms, data, model, fitparams, profiles, ranges, len_line,
    len_profile, nspectra, nparams, solution, norm)

# Procedure parameters:
pointer	ms					# MULTISPEC data structure
real	data[len_line]				# Data to be fit
real	model[len_line]				# Model to be corrected
int	fitparams[nspectra, nparams]		# Model parameters to be fit
real	profiles[len_profile, nspectra, nparams]# Model parameter derivatives
real	ranges[nspectra, LEN_RANGES]		# Ranges array for profiles
int	len_line				# Length of data line
int	len_profile				# Length of profiles
int	nspectra				# Number of spectra
int	nparams					# Number of model parameters
real	solution[nspectra, nparams]		# Solution correction vector
real	norm					# Measure of fit

# Lawson and Hanson parameters:
pointer	g				# Working array
pointer	x				# Working vector
int	mdg				# Maximum dimension of g
int	n				# Number of parameters to be determined
int	nb				# Parameter bandwith
int	ip, ir, mt, jt, jt_next		# Array pointers
real	rnorm				# Deviation from fit
int	ier				# Error flag

int	ns				# Maximum spectra bandwidth
pointer	columns				# Columns to be used.
int	ncolumns			# Number of columns
pointer	spectra				# Spectra to be used.
int	nspectra_to_solve			# Number of spectra
int	k_start, k_next			# Indices to the spectra array
int	column, spectrum, parameter	# Column, spectrum and parameter values
int	ns_in_band			# Number of spectra in band
int	i, j, k, l, m
bool	is_zero
pointer	sp

begin
	# Determine columns, spectra, and parameters contributing to
	# the solution matrix and the bandwidth of the matrix.
	call smark (sp)
	call salloc (columns, len_line, TY_INT)
	call salloc (spectra, nspectra, TY_INT)
	call band_set (ms, fitparams, data, profiles, ranges, Memi[columns],
	    Memi[spectra], len_line, len_profile, nspectra, nparams, ncolumns,
	    nspectra_to_solve, n, ns, nb)
	if (n == 0) {
	    call sfree (sp)
	    return
	}

	# Allocate working memory for the Lawson and Hanson routines.
	mdg = ncolumns
	call salloc (g, mdg * (nb + 1), TY_REAL)
	call salloc (x, n, TY_REAL)

	# Initialize array indices.
	ip = 1
	ir = 1
	jt = 1
	mt = 0
	jt_next = jt
	k_next = 1

	# Accumulate banded matrix for the specifed columns, spectra, and
	# parameters.
	do i = 1, ncolumns {
	    column = Memi[columns + i - 1]

	    k_start = k_next
	    j = jt
	    ns_in_band = 0
	    do k = k_start, nspectra_to_solve {
		spectrum = Memi[spectra + k - 1]

		# Evalute parameter derivatives and determine if all
		# derivatives for the spectrum are zero.
		is_zero = TRUE
	        do parameter = 1, nparams {
		    if (fitparams[spectrum, parameter] == NO)
		         next
		    j = j + 1
		    m = column - ranges[spectrum, X_START] + 1
		    if ((m < 1) || (m > len_profile))
			Memr[x + j - 2] = 0.
		    else {
		        Memr[x + j - 2] = profiles[m, spectrum, parameter]
		        if (parameter != I0_INDEX)
			    Memr[x + j - 2] = Memr[x + j - 2] *
			        PARAMETER (ms, I0, spectrum)
		        if (Memr[x + j - 2] != 0.)
		    	    is_zero = FALSE
		    }
		}

		# If the spectrum has a non-zero contribution to the parameter
		#     matrix then increment the number of spectra in the
		#     band (ns_in_band).
		# Else if the number of spectra in the band is still zero then
		#     increment the spectrum and parameter pointers.
		# Else the band is assumed complete so break to accumulate
		#     the band.

		if (!is_zero)
		    ns_in_band = ns_in_band + 1
		else if (ns_in_band == 0) {
		    k_next = min (k + 1, nspectra_to_solve - ns + 1)
		    jt_next = min (j, n - nb + 1)
		} else {
		    do l = j, jt_next + nb - 1
			Memr[x + (l - 1)] = 0.
		    break
		}
	    }

	    # If the number of spectra in the band is zero then reset the
	    #     spectrum pointer (k_next) and go to the next column.
	    # Else if the number of spectra in the band exceeds the specified
	    #     bandwidth return an error.
	    # Else accumulate the new band.

	    if (ns_in_band == 0) {
		k_next = k_start
		jt_next = jt
		next
	    } else if (ns_in_band > ns)
		call error (MS_ERROR, "Bandwidth too small")

	    # If a new submatrix is being started accumulate last submatrix.
	    if ((jt_next != jt) && (mt > 0)) {
	        call bndacc (Memr[g], mdg, nb, ip, ir, mt, jt)
	        mt = 0
	    }

	    # Increment the submatrix line pointer (mt) and add the band to
	    # submatrix being accumulated.

	    mt = mt + 1
	    jt = jt_next
	    do k = 1, nb
		Memr[g+ir+mt-2 + (k-1)*mdg] = Memr[x + (jt - 1) + (k - 1)]
	    # INDEFR data may already be ignored in the column selection in
	    # band_set.
	    if (IS_INDEFR (data[column]))
	        Memr[g+ir+mt-2 + nb*mdg] = 0.
	    else
	        Memr[g+ir+mt-2 + nb*mdg] = data[column] - model[column]
	}

	# Accumulate last submatrix and calculate banded matrix solution vector.
	call bndacc (Memr[g], mdg, nb, ip, ir, mt, jt)
	call bndsol (1, Memr[g], mdg, nb, ip, ir, Memr[x], n, rnorm, ier)
	if (ier != 0) {
	    call error (MS_ERROR, "bandsol: Solution error")
	}

	# Compute error matrix here.  Not yet implemented.

	# The solution from bndsol is in array x.  Copy x to solution.
	j = 0
	do i = 1, nspectra_to_solve {
	    spectrum = Memi[spectra + i - 1]
	    do parameter = 1, nparams {
		if (fitparams[spectrum, parameter] == YES) {
		    solution[spectrum, parameter] = Memr[x + j]
		    j = j + 1
		} else
		    solution[spectrum, parameter] = 0.
	    }
	}
	norm = rnorm

	call sfree (sp)
end


# Reject parameters which have only zero derivatives.  Determine spectra,
# columns, and number of parameters contributing to the solution.
# Determine bandwidth of the banded matrix.

procedure band_set (ms, fitparams, data, profiles, ranges, columns, spectra,
    len_line, len_profile, nspectra, nparams, ncolumns, nspectra_to_solve,
    n, ns, nb)

pointer	ms					# MULTISPEC data structure
int	fitparams[nspectra, nparams]		# Parameters to be fit
real	data[len_line]				# Data being fit
real	profiles[len_profile, nspectra, nparams]# Parameter derivatives
real	ranges[nspectra, LEN_RANGES]		# Ranges array for profiles
int	columns[len_line]			# Return columns to be used
int	spectra[nspectra]			# Return spectra to used
int	len_line				# Length of data being fit
int	len_profile				# Length of profiles
int	nspectra				# Number of spectra
int	nparams					# Number of parameters
int	ncolumns				# Number of useful columns
int	nspectra_to_solve			# Number of useful spectra
int	n					# Number of parameters in fit
int	ns					# Number of spectra in band
int	nb					# Bandwith of matrix

int	i, j, k
int	column, spectrum, parameter
int	col_start
real	dx
int	xmin, xmax

begin
	# Initially set the spectra and columns to NO.
	call amovki (NO, spectra, nspectra)
	call amovki (NO, columns, len_line)

	# Determine the spectra and columns in which the fitparams have
	# non-zero derivatives.  Flag those fitparams which do not have
	# non-zero derivatives with NO.  Count the number of parameters
	# which have non-zero derivatives.

	n = 0
	do spectrum = 1, nspectra {
	    do parameter = 1, nparams {
	        if (fitparams[spectrum, parameter] == YES) {
		    fitparams[spectrum, parameter] = NO
		    col_start = ranges[spectrum, X_START]
		    do k = 1, len_profile {
			if (profiles[k, spectrum, parameter] != 0.) {
			    column = col_start + k - 1
			    if ((column >= 1) && (column <= len_line)) {

				# If the INDEFR data points are not to be
				# ignored but replaced by the model in solve,
				# replace the if clause with the following.
			        # columns[column] = YES
			        # fitparams[spectrum, parameter] = YES

				if (!IS_INDEFR (data[column])) {
			           columns[column] = YES
			           fitparams[spectrum, parameter] = YES
				}
			    }
			}
		    }
		    if (fitparams[spectrum, parameter] == YES) {
			n = n + 1
			spectra[spectrum] = YES
		    }
		}
	    }
	}

	# Count the number spectra to be used and set the spectra array.
	nspectra_to_solve = 0
	do spectrum = 1, nspectra {
	    if (spectra[spectrum] == YES) {
		nspectra_to_solve = nspectra_to_solve + 1
		spectra[nspectra_to_solve] = spectrum
	    }
	}

	# Count the number of columns to be used and set the columns array.
	ncolumns = 0
	do column = 1, len_line {
	    if (columns[column] == YES) {
		ncolumns = ncolumns + 1
		columns[ncolumns] = column
	    }
	}

	# Determine the maximum number spectra contributing to any column.
	ns = 1
	do i = 1, nspectra_to_solve - 1 {
	    xmax = 0
	    do parameter = 1, nparams {
		if (fitparams[spectra[i], parameter] == YES)
		    xmax = max (xmax,
			int (ranges[spectra[i], X_START] + len_profile - 1))
	    }
	    do j = i + 1, nspectra_to_solve {
		xmin = len_line
	        do parameter = 1, nparams {
		    if (fitparams[spectra[j], parameter] == YES)
		        xmin = min (xmin, int (ranges[spectra[j], X_START]))
	        }
		dx = xmax - xmin
		if (dx < 0)
		    break
		else
		    ns = max (ns, j - i + 1)
	    }
	}

	# Determine the banded matrix bandwidth.
	nb = 0
	do parameter = 1, nparams {
	    do i = 1, nspectra_to_solve {
		if (fitparams[spectra[i], parameter] == YES) {
		    nb = nb + ns
		    break
		}
	    }
	}
end
