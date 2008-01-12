include	<smw.h>
include	<units.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"
include "rvsample.h"


# RV_GETIM - Get an image from the input list given the image name, and
# then rebin it to the specified dispersion and starting wavelength.

int procedure rv_getim (rv, name, type, crval_1, crval_n, nnpts)

pointer	rv			#I RV struct pointer
char 	name[SZ_FNAME]		#I Name of image to read
int	type			#I Type of image to get (object|reference)
real	crval_1, crval_n	#I Requested wavelength region
int	nnpts			#I New number of points

pointer im			# Image pointer
pointer	smw			# SMW pointer
pointer	sh			# SHDR pointer
real	w0, wpc, tvel
int	i, npts

pointer	immap(), smw_openim()
real	imgetr()
int	rv_read(), imaccf()
bool	streq()
errchk	immap, smw_openim, rv_read, imaccf, realloc

begin
	# Open the image and the WCS.
	if (streq("",name) || streq(" ",name)) {
	    call rv_errmsg ("Null or blank image name specified.")
 	    return (ERR_READ)
	}
	iferr (im = immap (name, READ_ONLY, 0)) {
	    call rv_errmsg ("Error opening image '%s'.\n")
		call pargstr (name)
	    call flush (STDERR)
 	    return (ERR_READ)
	}
	iferr (smw = smw_openim (im)) {
	    call imunmap (im)
	    call rv_errmsg ("Error opening image WCS '%s'.\n")
		call pargstr (name)
	    call flush (STDERR)
 	    return (ERR_READ)
	}

	# Read the data and check for an error condition
	sh = NULL
	if (rv_read(rv, im, smw, sh, type, crval_1, crval_n, nnpts)==ERR_READ){
	    call shdr_close (sh)
	    call smw_close (smw)
	    call imunmap (im)
	    return (ERR_READ)
	}

	# We've had a successfull read so let's load the structure
	# All errors will hopefully have been trapped and reported by now.

	npts = SN(sh)
	if (RV_PIXCORR(rv) == YES) {
	    w0 = 1
	    wpc = 1
	} else {
	    w0 = log10 (W0(sh))
	    wpc = (log10 (W1(sh)) - w0) / (npts - 1)
	}

	if (type == OBJECT_SPECTRUM) {
	    call realloc (RV_OPIXX(rv), npts, TY_REAL)
	    call realloc (RV_OPIXY(rv), npts, TY_REAL)
	    call amovr (Memr[SX(sh)], Memr[RV_OPIXX(rv)], npts)
	    call amovr (Memr[SY(sh)], Memr[RV_OPIXY(rv)], npts)
	    do i = 1, npts
		OBJPIXX(rv,i) = w0 + (i-1) * wpc
	    RV_X1(rv) = W0(sh)
	    RV_X2(rv) = W1(sh)
	    RV_OAPNUM(rv) = AP(sh)
	    RV_DCFLAG(rv) = DC(sh)
	    RV_NPTS(rv) = npts
	    RV_OW0(rv) = w0
	    RV_OWPC(rv) = wpc
	    RV_OW2(rv) = w0 + (npts - 1) * wpc
	    RV_OAPNUM(rv) = RV_APNUM(rv)
	    SR_W0(RV_OSAMPLE(rv)) = w0
	    SR_WPC(RV_OSAMPLE(rv)) = wpc
	    if (RV_DCFLAG(rv) != DCNO)		# Get velocity dispersion
	        RV_DELTAV(rv) = wpc * CLN10
	    else 
		RV_DELTAV(rv) = INDEF
	    call strcpy (name, IMAGE(rv), SZ_FNAME)
	    call rv_fill_blanks (TITLE(sh), OBJNAME(rv), SZ_FNAME)

	    if (DEBUG(rv)) {
		call d_printf(DBG_FD(rv),"rv_getim():  OBJECT\n\t")
		call d_printf(DBG_FD(rv),":%s:  w0,wpc,npts,dcf=%g,%g,%d,%d\n")
		    call pargstr(name);  call pargr(w0); call pargr(wpc)
		    call pargi(npts); call pargi(RV_DCFLAG(rv))
	 	call d_flush(DBG_FD(rv))
	    }

	    # Do the normalization
	    OBJCONT(rv) = NO
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == OBJ_ONLY)
		call do_continuum (rv, OBJECT_SPECTRUM)

	} else {
	    call realloc (RV_RPIXX(rv), npts, TY_REAL)
	    call realloc (RV_RPIXY(rv), npts, TY_REAL)
	    call amovr (Memr[SX(sh)], Memr[RV_RPIXX(rv)], npts)
	    call amovr (Memr[SY(sh)], Memr[RV_RPIXY(rv)], npts)
	    do i = 1, npts
		REFPIXX(rv,i) = w0 + (i-1) * wpc
	    RV_X1(rv) = W0(sh)
	    RV_X2(rv) = W1(sh)
	    RV_RAPNUM(rv) = AP(sh)
	    #RV_OAPNUM(rv) = AP(sh)
	    RV_DCFLAG(rv) = DC(sh)
	    RV_RNPTS(rv) = npts
	    RV_RW0(rv) = w0
	    RV_RWPC(rv) = wpc
	    RV_RW2(rv) = w0 + (npts - 1) * wpc
	    SR_W0(RV_OSAMPLE(rv)) = w0
	    SR_WPC(RV_OSAMPLE(rv)) = wpc
	    call strcpy (name, RIMAGE(rv), SZ_FNAME)
	    call rv_fill_blanks (TITLE(sh), TEMPNAME(rv), SZ_FNAME)

	    if (DEBUG(rv)) {
		call d_printf(DBG_FD(rv),"rv_getim():  TEMPLATE\n\t")
		call d_printf(DBG_FD(rv),":%s:  w0,wpc,npts,dcf=%g,%g,%d,%d\n")
		    call pargstr(name);  call pargr(w0); call pargr(wpc)
		    call pargi(npts); call pargi(RV_DCFLAG(rv))
	 	call d_flush(DBG_FD(rv))
	    }

	    # Get the velocity from the reference star image header.  Save the
	    # warning for outputting results.
	    call realloc (RV_TEMPVEL(rv), RV_NTEMPS(rv), TY_REAL)
	    if (imaccf(im, KW_VHELIO(rv)) == YES)
	        tvel = imgetr(im, KW_VHELIO(rv))
	    else
		tvel = INDEF
	    TEMPVEL(rv,RV_TEMPNUM(rv)) = tvel

	    # Do the normalization
	    REFCONT(rv) = NO
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == TEMP_ONLY)
		call do_continuum (rv, REFER_SPECTRUM)
	}
	RV_GLOB_W1(rv) = min (RV_OW0(rv), RV_RW0(rv))
	RV_GLOB_W2(rv) = max (RV_OW2(rv), RV_RW2(rv))

	if (DEBUG(rv)) {
	    call d_printf(DBG_FD(rv),"\tapnum,oapnum,rapnum=%d,%d,%d\n")
		call pargi(RV_APNUM(rv));    call pargi(RV_OAPNUM(rv))
		call pargi(RV_RAPNUM(rv))
	    call d_printf(DBG_FD(rv),"\texiting - rv_getim\n")
	}

	call shdr_close (sh)
	call smw_close (smw)
	call imunmap (im)
	return (OK)
end


# RV_READ - Read the spectrum from "im" with aperture RV_APNUM.
# Convert to log dispersion (except for undispersion corrected data).

int procedure rv_read (rv, im, smw, sh, type, crval_1, crval_n, nnpts)

pointer	rv				#I RV struct pointer
pointer	im				#I Image pointer
pointer	smw				#I SMW pointer
pointer	sh				#O Spectrum
int	type				#I Type of spectrum to read
real	crval_1, crval_n		#I Requested wavelength region
int	nnpts				#I New number of points

int	np
real	w0, w1

errchk	shdr_open, shdr_linear

define	MAXPTS		8192

begin
	# Get the header.
	call shdr_open (im, smw, 1, 1, RV_APNUM(rv), SHHDR, sh)
	if (DC(sh) != DCNO)
	    call shdr_units (sh, "Angstroms")

	# Check units are pixels or Angstroms.
	if (DC(sh) != DCNO && UN_TYPE(UN(sh)) != UN_ANG) {
	    call rv_errmsg("Spectrum units not supported: %s")
		call pargstr (UN_USER(UN(sh)))
	    call tsleep (1)
	    return (ERR_READ)
	}

	# Get data.
	call shdr_open (im, smw, 1, 1, RV_APNUM(rv), SHDATA, sh)
	if (RV_PIXCORR(rv) == YES)
	    DC(sh) = DCNO
	if (DC(sh) != DCNO)
	    call shdr_units (sh, "Angstroms")

	# Check for maximum size.
	#if (SN(sh) > MAXPTS) {
	#    call rv_errmsg("Too many data points in image. (MAXPTS=%d)%80t")
	#	call pargi (MAXPTS)
	#    call tsleep (1)
	#    return (ERR_READ)
	#}

	# Check aperture numbers.
	if (AP(sh) != RV_APNUM(rv)) {
	    if (type == REFER_SPECTRUM && SMW_NSPEC(smw) == 1) {
		call rv_err_comment (rv, 
		    "WARNING: Template image is only 1-D.", "")
	    } else {
	        call rv_errmsg (
		    "Requested aperture number is out of range; apnum = %d.")
		    call pargi (RV_APNUM(rv))
	        return (ERR_READ)
	    }
	}

	if (IS_INDEFI(nnpts))
	    np = SN(sh)
	else 
	    np = nnpts
	if (IS_INDEF(crval_1))
	    w0 = W0(sh)
	else 
	    w0 = crval_1
	if (IS_INDEF(crval_n))
	    w1 = W1(sh)
	else 
	    w1 = crval_n

	if (DEBUG(rv)) {
	    call d_printf (DBG_FD(rv),"rv_read:\n\tap=%d line_num=%d - %d\n")
		call pargi(AP(sh)) ; call pargi(LINDEX(sh,1)) ; call pargi(type)
	    call d_printf(DBG_FD(rv),"\tval_1,val_n=%g,%g/%d  w0,w1=%g,%g/%d\n")
		call pargr(crval_1) ; call pargr(crval_n) ; call pargi(nnpts)
		call pargr(w0) ; call pargr(w1) ; call pargi(np)
	}

	# Rebin if needed.
	switch (DC(sh)) {
	case DCNO:
	    if (DEBUG(rv))
	        call d_printf (DBG_FD(rv), "\tPIXELS dispersion.\n")
	case DCLINEAR:
	    if (DEBUG(rv))
	        call d_printf (DBG_FD(rv), "\tLAMBDA dispersion.\n")
	    call shdr_linear (sh, w0, w1, np, DCLOG)
	    W0(sh) = w0
	    W1(sh) = w1
	    SN(sh) = np
	case DCLOG:
	    if (DEBUG(rv))
	        call d_printf (DBG_FD(rv), "\tLOGLAMBDA dispersion.\n")
	    if (!IS_INDEF(crval_n)) {
	        call shdr_linear (sh, w0, w1, np, DCLOG)
	        W0(sh) = w0
	        W1(sh) = w1
	        SN(sh) = np
	    }
	case DCFUNC:
	    if (DEBUG(rv))
	        call d_printf (DBG_FD(rv), "\tNONLINEAR dispersion.\n")
	    call shdr_linear (sh, w0, w1, np, DCLOG)
	    W0(sh) = w0
	    W1(sh) = w1
	    SN(sh) = np
	}

	if (DEBUG(rv)) {
	    call d_printf (DBG_FD(rv),"\tafter: w0,w1=%g,%g\n")
		call pargr(w0) ; call pargr(w1)
	    call d_printf (DBG_FD(rv), "\texiting - rv_read\n")
	}
	return (OK)
end
