include <pkg/gtools.h>
include	<error.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"

# DO_CONTINUUM - Do a continuum fitting for an given object, template or
# bin spectrum.

procedure do_continuum (rv, which)

pointer	rv					#I RV struct pointer
int 	which					#I Which spectrum to work on?

pointer	sp, fit
int	tnum, inum
int	continuum()
errchk	realloc, continuum

begin
	call smark (sp)				# Allocate some temporary space
	call salloc (fit, max(RV_RNPTS(rv),RV_NPTS(rv))+10, TY_REAL)

	tnum = RV_TEMPNUM(rv)
	inum = RV_IMNUM(rv)

	# Now parse the argument to find out what to do
	switch (which) {
	case OBJECT_SPECTRUM:			# Do object only
	    if (RV_CONTINUUM(rv) == TEMP_ONLY || RV_CONTINUUM(rv) == NONE) {
		call sfree (sp)
		return
	    }

	    call realloc (RV_OCONTP(rv), RV_NPTS(rv), TY_REAL)
	    OBJCONT(rv) = continuum (rv, OBJPIXY(rv,1), RV_NPTS(rv), 
		OCONT_DATA(rv,1), Memr[fit])
	
	case REFER_SPECTRUM:			# Do template only
	    if (RV_CONTINUUM(rv) == OBJ_ONLY || RV_CONTINUUM(rv) == NONE) {
		call sfree (sp)
		return
	    }

	    call realloc (RV_RCONTP(rv), RV_RNPTS(rv), TY_REAL)
	    REFCONT(rv) = continuum (rv, REFPIXY(rv,1), RV_RNPTS(rv), 
		RCONT_DATA(rv,1), Memr[fit])
	}

	call sfree (sp)
end


# CONTINUUM - Do the continuum normalization, either interactively or in
# batch mode.

int procedure continuum (rv, indata, npts, outdata, fit)

pointer	rv					#I RV struct pointer
real	indata[npts]				#I Array to be fit
int	npts					#I NPTS to fit
real	outdata[npts]				#O Normalized array
real	fit[npts]				#O Fit array

pointer	ic					# ICFIT ptr
pointer	gt_init()

begin
	# Set the ICFIT pointer structure.
	if (RV_ICFIT(rv) == NULL) {
	    call ic_open (ic)
	    RV_ICFIT(rv) = ic
	}

	call ic_pstr (ic, "sample", Memc[CON_SAMPLE(rv)])
	call ic_pstr (ic, "function", Memc[CON_FUNC(rv)])
	call ic_puti (ic, "naverage", CON_NAVERAGE(rv))
	call ic_puti (ic, "order", CON_ORDER(rv))
	call ic_puti (ic, "niterate", CON_NITERATE(rv))
	call ic_putr (ic, "low", CON_LOWREJECT(rv))
	call ic_puti (ic, "markrej", CON_MARKREJ(rv))
	call ic_putr (ic, "high", CON_HIGHREJECT(rv))
	call ic_putr (ic, "grow", CON_GROW(rv))
	call ic_pstr (ic, "ylabel", "")

	if (CON_INTERACTIVE(rv) == YES && RV_INTERACTIVE(rv) == YES) {
	    if (RV_GT(rv) == NULL)  		# Initialize GTOOLS if needed
	        RV_GT(rv) = gt_init()			
	    call gt_sets (RV_GT(rv), GTTYPE, "line")
	}

	# Fit the input image.
	call cn_fit1d (rv, indata, npts, ic, RV_GT(rv), CON_INTERACTIVE(rv), 
	    outdata, fit)

	call ic_closer (ic)
	RV_ICFIT(rv) = NULL
	return (YES)
end


# CN_FIT1D -- If the interactive flag is set then set the fitting 
# parameters interactively.

procedure cn_fit1d (rv, indata, npts, ic, gt, interactive, outdata, fit)

pointer	rv				#I RV struct pointer
real	indata[npts]			#I Array to be fit
int	npts				#I NPTS in data array
pointer	ic				#I ICFIT pointer
pointer	gt				#I GTOOLS pointer
int	interactive			#I Interactive?
real	outdata[npts]			#O Array of normalized data
real	fit[npts]			#O Array of fit

int	i
pointer	cv, sp, x, wts

begin
	# Allocate memory for curve fitting.
	call smark (sp)
	call salloc (x, npts, TY_REAL)
	call salloc (wts, npts, TY_REAL)

	do i = 1, npts				# Initlialize X and WTS array
	    Memr[x+i-1] = real (i)
	call amovkr (1., Memr[wts], npts)

	call ic_putr (ic, "xmin", 1.)		# Update icfit struct
	call ic_putr (ic, "xmax", real(npts))

	# If the interactive flag is set then use icg_fit to set the
	# fitting parameters.  Only done if task is run interactively
	# as well.

	if (interactive == YES && RV_INTERACTIVE(rv) == YES) {
	    if (RV_GP(rv) == NULL)
		call init_gp (rv, true, "stdgraph")
	    call gclear (RV_GP(rv))

	    call icg_fit (ic, RV_GP(rv), "cursor", gt, cv, Memr[x], indata,
	        Memr[wts], npts)
	
	    # Now recover any parameters that were changed
	    call recover_icfit_pars (rv, ic)

	} else {
	    # Do the fit non-interactively.
	    call ic_fit (ic, cv, Memr[x], indata, Memr[wts], npts, YES, YES, 
	        YES, YES)
	}

	# Replace rejected points with the fit if requested.
	if (CON_REPLACE(rv) == YES) {
	    call amovr (indata, fit, npts)
            call ic_clean (ic, cv, Memr[x], fit, Memr[wts], npts)
            call amovr (fit, indata, npts)
	}

	# Now subtract the fit.
	call cvvector (cv, Memr[x], fit, npts)
	call asubr (indata, fit, outdata, npts)

	call cvfree (cv)
	call sfree (sp)
end


# RECOVER_ICFIT_PARS - Since the ICFIT parameters may have been changed in
# an interactive operation, we need to get the new values from the ICFIT
# structure.

procedure recover_icfit_pars (rv, ic)

pointer	rv						#I RV struct pointer
pointer	ic						#I ICFIT pointer

pointer	sp, func

int	strdic(), ic_geti()
real	ic_getr()

begin
	call smark (sp)
	call salloc (func, SZ_FNAME, TY_CHAR)

	CON_NAVERAGE(rv) = ic_geti (ic, "naverage")
	CON_ORDER(rv) = ic_geti (ic, "order")
	CON_NITERATE(rv) = ic_geti (ic, "niterate")
	CON_MARKREJ(rv) = ic_geti (ic, "markrej")
	CON_LOWREJECT(rv) = ic_getr (ic, "low")
	CON_HIGHREJECT(rv) = ic_getr (ic, "high")
	CON_GROW(rv) = ic_getr (ic, "grow")

	call ic_gstr (ic, "sample", Memc[CON_SAMPLE(rv)], SZ_LINE)
	call ic_gstr (ic, "function", Memc[func], SZ_LINE)
	CON_CNFUNC(rv) = strdic(Memc[func], Memc[func], SZ_LINE, CN_INTERP_MODE)
	if (CON_CNFUNC(rv) == 0) {
	    call sfree (sp)
	    call error (0, "Unknown fitting function type")
	}

	call sfree (sp)
end
