include	<imhdr.h>
include <tbset.h>
include "../lib/daophot.h"
include "../lib/daophotdef.h"
include "../lib/nstardef.h"
include "../lib/apsel.h"

define	NCOLUMN		10

# DP_NPHOT -- Read in the groups and fit all the stars in each group
# simultaneously.

procedure dp_nphot (dao, im, grp, nst, ap_text)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
pointer	grp			# pointer to the input group file
pointer nst			# pointer to the ouput table
bool	ap_text			# photometry text file

bool	clip, converge 
int	out_record, in_record, nrow_in_table, old_size, niter, cdimen, nterm
int	stat
pointer	sp, indices, fields, icolpoint, ocolpoint, key, nstar, apsel
real	mean_sky, chiold

int	dp_ggroup(), tbpsta(), dp_nstarfit(), dp_nxycheck()
real	dp_nmsky()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (icolpoint, NAPGROUP, TY_INT)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (ocolpoint, NCOLUMN, TY_INT)

	# Get some daophot pointers.
	apsel = DP_APSEL(dao)
	nstar = DP_NSTAR (dao)

	# Allocate space for and define the output table.
	if (DP_TEXT(dao) == YES) 
	    call dp_xpnewnstar (dao, nst)
	else
	    call dp_tpnewnstar (dao, nst, Memi[ocolpoint])

	# Set up the input file
	call dp_gnindices (Memi[indices])
	if (ap_text) {
	    call pt_kyinit (key)
	    call dp_gnstpsf (Memi[indices], Memc[fields], NAPGROUP)
	    nrow_in_table = 0
	} else {
	    key = NULL
	    call dp_tnstinit (grp, Memi[icolpoint])
	    nrow_in_table = tbpsta (grp, TBL_NROWS)
	}

	# Allocate some memory for fitting.
	call dp_memapsel (dao, Memi[indices], NAPPAR, DP_MAXGROUP(dao) + 1)
	call dp_memnstar (dao, DP_MAXGROUP(dao) + 1)

	# Initialize the input file for reading.
	in_record = 1

	# Initialize the output file for writing.
	out_record = 0

	repeat {

	    # Read in the next group of stars.
	    old_size = dp_ggroup (dao, grp, key, Memc[fields], Memi[indices],
	        Memi[icolpoint], nrow_in_table, DP_MAXGROUP(dao), in_record,
		DP_GROUPNUM(nstar))
	    if (old_size <= 0)
		break
	    else
	        DP_NUMBSTAR(nstar) = old_size

	    # Print out the group number and number of stars.
	    if (DP_VERBOSE (dao) == YES) {
		call printf ("Group: %4d contains %2d stars\n")
		    call pargi (DP_GROUPNUM (nstar))
		    call pargi (DP_NUMBSTAR (nstar))
	    }

	    niter = 1

	    # If too many stars skip to the next group.
	    if (DP_NUMBSTAR (nstar) > DP_MAXGROUP(dao)) {

		if (DP_VERBOSE(dao) == YES)
		    call printf ("\tGroup larger than maximum: skipping\n")

		call dp_nstindef (dao, DP_NUMBSTAR(nstar))

	    # If the group center is undefined or off image skip to next 
	    # group.
	    } else if (dp_nxycheck (im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], DP_NUMBSTAR(nstar),
	        DP_FITRAD(dao), stat) <= 0) {

		if (DP_VERBOSE(dao) == YES) {
		    if (stat < 0)
			call printf ("\tGroup center is undefined: skipping\n")
		    else if (stat == 0)
			call printf ("\tGroup is off the image: skipping\n")
		}

		call dp_nstindef (dao, DP_NUMBSTAR(nstar))

	    # If the mean sky value of group is undefined skip to next group.
	    } else if (IS_INDEFR (dp_nmsky (Memr[DP_APMSKY(apsel)],
	        DP_NUMBSTAR(nstar), mean_sky))) {

		if (DP_VERBOSE(dao) == YES)
		    call printf ("\tGroup sky value is undefined: skipping\n")

		call dp_nstindef (dao, DP_NUMBSTAR(nstar))

	    # Do the fit.
	    } else {

		# Estimate values of INDEF mangitudes.
		call dp_nmaginit (dao, Memr[DP_APMAG(apsel)],
		    Memr[DP_APERR(apsel)], DP_NUMBSTAR(nstar))

		# Set up for the fit.
	        converge = false
	        chiold = 1.0
	        clip = false
	        nterm = 3 * DP_NUMBSTAR (nstar)
	        cdimen = 3 * DP_NUMBSTAR (nstar) + 1
	        call aclrr (Memr[DP_XOLD(nstar)], nterm)
	        call amovkr (1.0, Memr[DP_XCLAMP(nstar)], nterm)

	        # Iterate.
	        while ((! converge) && (DP_NUMBSTAR(nstar) > 0)) {
		    if (niter > DP_MAXITER(dao))
		        break
	            DP_NUMBSTAR(nstar) = dp_nstarfit (dao, im,
		        DP_NUMBSTAR(nstar), mean_sky, cdimen, chiold, clip,
			converge, niter)
		    niter = niter + 1
	        }
	    }


	    # Now write out the results.
	    niter = niter - 1
	    if (DP_TEXT(dao) == YES)
	        call dp_xntwrite (dao, nst, niter, old_size)
	    else
	        call dp_tntwrite (dao, nst, niter, old_size, out_record,
		    Memi[ocolpoint])
	} 

	if (ap_text)
	    call pt_kyfree (key)

	call sfree (sp)
end


# DP_NSTINDEF -- Set magnitudes and fitting parameters of unfit stars to indef.

procedure dp_nstindef (dao, nstars)

pointer	dao		# pointer to the daophot structure
int	nstars		# number of stars

int	i
pointer	apsel

begin
	apsel = DP_APSEL(dao)

	do i = 1, nstars {
	    Memr[DP_APMAG(apsel)+i-1] = INDEFR
	    Memr[DP_APERR(apsel)+i-1] = INDEFR
	    Memr[DP_APCHI(apsel)+i-1] = INDEFR
	    Memr[DP_APSHARP(apsel)+i-1] = INDEFR
	}
end


# DP_GNINDICES -- Get the memory allocation fields.

procedure dp_gnindices (indices)

int	indices[ARB]		# index array

begin
	indices[1] = DP_PAPID
        indices[2] = DP_PAPXCEN
	indices[3] = DP_PAPYCEN
	indices[4] = DP_PAPMAG1
	indices[5] = DP_PAPSKY
	indices[6] = DP_PAPGROUP
	indices[7] = DP_PAPMERR1
	indices[8] = DP_PAPNITER
	indices[9] = DP_PAPSHARP
	indices[10] = DP_PAPCHI
end
