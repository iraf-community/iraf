include	<mach.h>
include	<gset.h>
include	<math/iminterp.h>
include	<smw.h>
include	"../identify.h"
include	"autoid.h"


# List of debug key letters.
# Debug a: Print candidate line assignments.
# Debug b: Print search limits.
# Debug c: Print list of line ratios.
# Debug d: Graph dispersions.
# Debug f: Print final result.
# Debug i: Show ICFIT iterations.
# Debug l: Graph lines and spectra.
# Debug m: Print miscellaneous debug information
# Debug n: Show non-linearity constraint
# Debug r: Print list of reference lines.
# Debug s: Print search iterations.
# Debug t: Print list of target lines.
# Debug v: Print vote array.
# Debug w: Print wavelength bin limits.


# AID_AUTOID -- Automatically identify spectral features.
# This routine is main entry to the autoidentify algorithms.
# The return value is true if the ID pointer contains a new solution
# and false if the ID pointer is the original solution.

bool procedure aid_autoid (id, aid)

pointer	id		#I ID pointer
pointer	aid		#U AID pointer

bool	cdflip
int	i, j, k, l, iev, nbins, bin, nbest
double	best, dval1, dval2
pointer	sp, str, idr, ev, evf, sid

bool	streq(), strne()
int	stridxs()
double	dcveval(), aid_eval()
pointer	gopen(), aid_evalloc(), id_getid()
errchk	id_mapll, aid_reference, aid_target, aid_autoid1, aid_evalutate

define	done_	10
define	redo_	20

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Save original data.
	call id_saveid (id, "autoidentify backup")

	# Initialize.
	AID_IDT(aid) = id
	call ic_putr (AID_IC1(aid), "xmin", real (PIXDATA(id,1)))
	call ic_putr (AID_IC1(aid), "xmax", real (PIXDATA(id,ID_NPTS(id))))
	AID_IC2(aid) = ID_IC(id)

	if (stridxs ("ild", AID_DEBUG(aid,1)) != 0 && ID_GP(id) == NULL) {
	    call clgstr ("graphics", Memc[str], SZ_LINE)
	    ID_GP(id) = gopen (Memc[str], NEW_FILE+AW_DEFER, STDGRAPH)
	} else if (AID_DEBUG(aid,1) != EOS && ID_GP(id) != NULL)
	    call gdeactivate (ID_GP(id), 0)

	idr = AID_IDR(aid)
	if (idr == NULL) {
	    call id_init (AID_IDR(aid))
	    idr = AID_IDR(aid)
	}

	# Set reference coordinate list.
	if (strne (AID_REFLIST(aid), ID_COORDLIST(idr)) ||
	    streq (AID_REFLIST(aid), "FEATURES")) {
	    call id_unmapll (idr)
	    ID_COORDLIST(idr) = EOS

	    if (streq (AID_REFLIST(aid), "FEATURES")) {
		if (ID_NFEATURES(id) >= 10) {
		    call strcpy (AID_REFLIST(aid), ID_COORDLIST(idr),
			ID_LENSTRING)
		    i = ID_NFEATURES(id)
		    ID_NLL(idr) = i
		    call calloc (ID_LL(idr), i+1, TY_DOUBLE)
		    call calloc (ID_LLL(idr), i+1, TY_POINTER)
		    call amovd (USER(id,1), Memd[ID_LL(idr)], i)
		    Memd[ID_LL(idr)+i] = INDEFD
		}
	    } else if (AID_REFLIST(aid) != EOS) {
		call strcpy (AID_REFLIST(aid), ID_COORDLIST(idr), ID_LENSTRING)
		call id_mapll (idr)
	    }
	}

	# Get reference spectrum.
	if (AID_REFSPEC(aid) != EOS)
	    call strcpy (AID_REFSPEC(aid), ID_COORDSPEC(idr), ID_LENSTRING)
	else if (ID_COORDSPEC(idr) == EOS)
	    call strcpy (ID_COORDSPEC(id), ID_COORDSPEC(idr), ID_LENSTRING)
	if (strne (ID_COORDSPEC(idr), ID_IMAGE(idr))) {
	    if (ID_SH(idr) != NULL) {
		call smw_close (MW(ID_SH(idr)))
		call imunmap (IM(ID_SH(idr)))
		call shdr_close (ID_SH(idr))
	    }
	    call strcpy (ID_COORDSPEC(idr), ID_IMAGE(idr), ID_LENSTRING)
	    ifnoerr (call id_map (idr))
		call id_gdata(idr)
	    else {
		ID_COORDSPEC(idr) = EOS
		ID_IMAGE(idr) = EOS
	    }
	}

	ID_MAXFEATURES(idr) = AID_NRMAX(aid)
	ID_MINSEP(idr) = ID_MINSEP(id)
	ID_FTYPE(idr) = ID_FTYPE(id)
	ID_FWIDTH(idr) = ID_FWIDTH(id)
	ID_CRADIUS(idr) = ID_CRADIUS(id)
	ID_THRESHOLD(idr) = ID_THRESHOLD(id)
	ID_MATCH(idr) = ID_MATCH(id)

	# Use faster, less accurate centering for the search.
	call c1d_params (II_LINEAR, 0.02)

	# Set target lines and dispersion limits.
	call aid_target (aid)
	cdflip = (AID_CDDIR(aid) == CDUNKNOWN ||
	    (IS_INDEFD(AID_CDELT(aid)) && AID_CDDIR(aid) == CDSIGN))

	# Now search for the dispersion function and line identifications.
	# The reference spectrum is broken up into a varying number of
	# pieces and each is searched.  The order in which the reference
	# spectrum is divided is from the middle outward and overlapping
	# bins are tried as a second pass.  The hope is to find a
	# piece that is close enough to the target spectrum as quickly
	# as possible.

	AID_BEST(aid) = MAX_REAL
	nbest = 0
	iev = 0
redo_
	do i = 0, 1 {
	    do j = 1, AID_NB(aid) {
		if (j == 1)
		    nbins = (AID_NB(aid) + 2) / 2
		else if (mod (j, 2) == 0)
		    nbins = (AID_NB(aid) + 2 - j) / 2
		else
		    nbins = (AID_NB(aid) + 1 + j) / 2
		nbins = 2 * nbins - 1
		do k = 1, nbins {
		    if (k == 1)
			bin = (nbins + 2) / 2
		    else if (mod (k, 2) == 0)
			bin = (nbins + 2 - k) / 2
		    else
			bin = (nbins + 1 + k) / 2
		    if (mod ((nbins-1)/2, 2) == 0) {
			if (mod (bin, 2) == i)
			    next
		    } else {
			if (mod (bin, 2) != i)
			    next
		    }

		    iferr {
			iev = iev + 1
			ev = aid_evalloc (aid, iev)
			AID_BIN1(ev) = nbins
			AID_BIN2(ev) = bin
			call aid_reference (aid, ev, NO)
			call aid_autoid1 (aid, ev)
		    } then {
			AID_ND(ev) = 0
		    }
		    if (cdflip) {
			iferr {
			    iev = iev + 1
			    evf = aid_evalloc (aid, iev)
			    AID_BIN1(evf) = nbins
			    AID_BIN2(evf) = bin
			    call aid_reference (aid, evf, YES)
			    call aid_autoid1 (aid, evf)
			} then {
			    AID_ND(evf) = 0
			}
		    }

		    # Search the candidates with the highest weights.
		    # Terminate the search if the number of best fit values
		    # less than 1. is equal to the specified number.
		    do l = 1, 5 {
			best = aid_eval (aid, ev, l)
			if (!IS_INDEFD(best) && best < 1.) {
			    nbest = nbest + 1
			    if (nbest >= AID_NBEST(aid))
				goto done_
			}
			if (cdflip) {
			    best = aid_eval (aid, evf, l)
			    if (!IS_INDEFD(best) && best < 1.) {
				nbest = nbest + 1
				if (nbest >= AID_NBEST(aid))
				    goto done_
			    }
			}
		    }
		}
	    }
	}

	# Go back and evaluate candidates with lower weights.
	# Terminate the search if the number of best fit values
	# less than 1. is equal to the specified number.
	do j = 6, AID_ND(ev) {
	    do i = 1, iev {
		ev = aid_evalloc (aid, i)
		best = aid_eval (aid, ev, j)
		if (!IS_INDEFD(best) && best < 1.) {
		    nbest = nbest + 1
		    if (nbest >= AID_NBEST(aid))
			goto done_
		}
	    }
	}

	# Add other loops here.
	if (AID_BEST(aid) > 1.) {
	    if (AID_NP(aid) > 3) {
	        AID_NP(aid) = AID_NP(aid) - 1
		goto redo_
	    }
	}

done_
	do i = 1, iev
	    call aid_evfree (aid, i)

	# Evaluate the final solution with the full dispersion function.
	# Save the final solution.  If the final solution has a merit
	# greater than one restore the original solution.

	sid = id_getid (id, "autoidentify")
	if (sid != NULL) {
	    call dcvfree (ID_CV(id))
	    iferr (call aid_dofitf (aid, id))
		;
	    call id_sid (id, sid)
	} else {
	    ID_NFEATURES(id) = 0
	    call dcvfree (ID_CV(id))
	    call id_saveid (id, "autoidentify")
	}

	# Debug f: Print final result.
	if (stridxs ("f", AID_DEBUG(aid,1)) != 0) {
	    if (AID_BEST(aid) == MAX_REAL) {
		call eprintf ("%s %8.5g %8.3g     No solution found\n")
		    call pargstr (ID_IMAGE(id))
		    call pargd (AID_CRVAL(aid))
		    call pargd (AID_CDELT(aid))
	    } else {
		call eprintf (
		    "%s %8.5g %8.3g %8.5g %8.3g %3d %3d %6.3f %5.2f\n")
		    call pargstr (ID_IMAGE(id))
		    call pargd (AID_CRVAL(aid))
		    call pargd (AID_CDELT(aid))
		    if (ID_CV(id) == NULL) {
			dval1 = FITDATA(id,1)
			dval2 = FITDATA(id,2) - FITDATA(id,1)
		    } else  {
			dval1 = dcveval (ID_CV(id), AID_CRPIX(aid)+1D0)
			dval2 = dcveval (ID_CV(id), AID_CRPIX(aid)-1D0)
			dval2 = (dval1 - dval2) / 2D0
			dval1 = dcveval (ID_CV(id), AID_CRPIX(aid))
		    }
		    call pargd (dval1)
		    call pargd (FITDATA(id,2) - FITDATA(id,1))
		    call pargi (nint(100.*AID_FMATCH(aid)))
		    call pargi (nint(100.*AID_FTMATCH(aid)))
		    call pargr (AID_RMS(aid))
		    call pargr (AID_BEST(aid))
		call eprintf (
		    "   dCRVAL = %.4g (%.3g),  dCDELT = %.4g (%.3g)\n")
		    call pargd (dval1 - AID_CRVAL(aid))
		    call pargd (abs((dval1-AID_CRVAL(aid))/
		        (ID_NPTS(id)*AID_CDELT(aid))))
		    call pargd (dval2 - AID_CDELT(aid))
		    call pargd (abs((dval2-AID_CDELT(aid))/AID_CDELT(aid)))
	    }
	}

	if (AID_BEST(aid) > 1.) { 
	    ID_NFEATURES(id) = 0
	    ID_CURRENT(id) = 0
	    call dcvfree (ID_CV(id))
	    sid = id_getid (id, "autoidentify backup")
	    ID_NEWFEATURES(id) = NO
	    ID_NEWCV(id) = NO
	    ID_NEWGRAPH(id) = NO
	}
	call id_fitdata (id)

	# Restore centering.
	call c1d_params (II_SPLINE3, 0.001)

	call sfree (sp)

	return (AID_BEST(aid) <= 1.)
end
