include <imhdr.h>
include <imio.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"
include "rvsample.h"
include "rvmwcs.h"

# RV_GETIM - Get an image from the input list given the image name.

int procedure rv_getim (rv, name, type)

pointer	rv			#I RV struct pointer
char 	name[SZ_FNAME]		#I Name of image to read
int	type			#I Type of image to get (object|reference)

pointer im			# Image pointer
pointer	sp, px, py
real	w0, wpc, tvel
int	npts

pointer	immap()
real	imgetr()
int	rv_read(), imaccf()
bool	streq()
errchk	immap, imaccf, realloc

define	MAXPTS		8192

begin
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

	npts = IM_LEN(im, 1)
	if (npts > MAXPTS) {
	    call rv_errmsg("Too many data points in image. (MAXPTS=%d)%80t")
		call pargi (MAXPTS)
	    call tsleep (1)
	    call imunmap (im)
	    return (ERR_READ)
	}

	call smark (sp)					# Allocate some space
	call salloc (px, npts, TY_REAL)
	call salloc (py, npts, TY_REAL)

	# Read the data and check for an error condition
	if (rv_read(rv,im,type,Memr[px],Memr[py],npts,w0,wpc) == ERR_READ) {
	    call sfree (sp)
	    call imunmap (im)
	    return (ERR_READ)
	}

	# We've had a successfull read so let's load the structure
	# All errors will hopefully have been trapped and reported by now.

	if (type == OBJECT_SPECTRUM) {
	    call realloc (RV_OPIXX(rv), npts, TY_REAL)
	    call realloc (RV_OPIXY(rv), npts, TY_REAL)
	    call amovr (Memr[px], Memr[RV_OPIXX(rv)], npts)
	    call amovr (Memr[py], Memr[RV_OPIXY(rv)], npts)
	    RV_NPTS(rv) = npts
	    RV_OW0(rv) = w0
	    RV_OWPC(rv) = wpc
	    RV_OW2(rv) = w0 + (npts - 1) * wpc
	    RV_OAPNUM(rv) = RV_APNUM(rv)
	    SR_W0(RV_OSAMPLE(rv)) = w0
	    SR_WPC(RV_OSAMPLE(rv)) = wpc
	    if (RV_DCFLAG(rv) != -1) 		# Get velocity dispersion
	        RV_DELTAV(rv) = wpc * CLN10
	    else 
		RV_DELTAV(rv) = INDEF
	    call strcpy (name, IMAGE(rv), SZ_FNAME)
	    call rv_fill_blanks (IM_TITLE(im), OBJNAME(rv), SZ_FNAME)

	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
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
	    if (RV_RNPTS(rv) < npts) {
	        call realloc (RV_RPIXX(rv), npts, TY_REAL)
	        call realloc (RV_RPIXY(rv), npts, TY_REAL)
	    }
	    call amovr (Memr[px], Memr[RV_RPIXX(rv)], npts)
	    call amovr (Memr[py], Memr[RV_RPIXY(rv)], npts)
	    RV_RNPTS(rv) = npts
	    RV_RW0(rv) = w0
	    RV_RWPC(rv) = wpc
	    RV_RW2(rv) = w0 + (npts - 1) * wpc
	    SR_W0(RV_OSAMPLE(rv)) = w0
	    SR_WPC(RV_OSAMPLE(rv)) = wpc
	    call strcpy (name, RIMAGE(rv), SZ_FNAME)
	    call rv_fill_blanks (IM_TITLE(im), TEMPNAME(rv), SZ_FNAME)

	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
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
	RV_W0_SHIFT(rv) = 0.0
	RV_GLOB_W1(rv) = min (RV_OW0(rv), RV_RW0(rv))
	RV_GLOB_W2(rv) = max (RV_OW2(rv), RV_RW2(rv))

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf(DBG_FD(rv),"\tapnum,oapnum,rapnum=%d,%d,%d\n")
		call pargi(RV_APNUM(rv));    call pargi(RV_OAPNUM(rv))
		call pargi(RV_RAPNUM(rv))
	    call d_printf(DBG_FD(rv),"\texiting - rv_getim\n")
	}
	call imunmap (im)
	call sfree (sp)
	return (OK)
end


# RV_READ - Read the array pointed to by 'im'.

int procedure rv_read (rv, im, type, x, y, npts, w0, wpc)

pointer	rv					#I RV struct pointer
pointer	im					#I Image pointer
int	type					#I Type of spectrum to read
real	x[npts], y[npts]			#O Output data arrays
int	npts					#O npts in arrays
real	w0, wpc					#O dispersion info (log10)

double	dex()
int	i, dcflag, apnum, line_num
pointer	dataptr

pointer imgl2r()
int	get_disp_info(), rv_ap2line(), rv_read_ap()
errchk	imgl2r

begin
	# Get the line number to read in the data
	apnum = RV_APNUM(rv)
	line_num = rv_ap2line (rv, im, apnum)
	if (line_num > IM_LEN(im,2) || line_num == ERR) {
	    if (type == REFER_SPECTRUM && IM_LEN(im,2) <= 1) {
		call rv_err_comment (rv, 
		    "WARNING: Template image is only 1-D.", "")
		line_num = 1
		RV_RAPNUM(rv) = 1
	    } else {
	        call rv_errmsg (
		    "Requested aperture number is out of range; apnum = %d.")
		    call pargi (apnum)
	        return (ERR_READ)
	    }
	}

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf (DBG_FD(rv), "rv_read:\n\tap=%d line_num=%d - %d\n")
		call pargi (apnum);  call pargi (line_num); call pargi (type)
	}

	# Now read the data
	iferr (dataptr = imgl2r(im,line_num)) {
	    call rv_errmsg ("Error reading data from image `%s[%d]'.")
		call pargstr (IM_NAME(im))
		call pargi (apnum)
	    return (ERR_READ)
	}
	if (line_num <= IM_LEN(im,2) && type == REFER_SPECTRUM) {
	    if (IM_LEN(im,2) == 1)
	        RV_RAPNUM(rv) = rv_read_ap (rv, im, 1)
	    else
	        RV_RAPNUM(rv) = apnum
	    apnum = RV_RAPNUM(rv)
	}
	RV_OAPNUM(rv) = apnum
	call amovr (Memr[dataptr], y, npts)

	# Get the numbers
	switch (get_disp_info(rv, im, apnum, w0, wpc, dcflag)) {
	case LOGLAMBDA:
	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	        call d_printf (DBG_FD(rv), "\tLOGLAMBDA dispersion.\n")
	    do i = 1, npts 				# Log-lambda binning
	        x[i] = dex (w0 + (i-1) * wpc)
	    RV_X1(rv) = x[1]
	    RV_X2(rv) = x[npts]
	case LAMBDA:
	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	        call d_printf (DBG_FD(rv), "\tLAMBDA dispersion.\n")
	    call rv_rebin (rv, y, npts, y, npts, w0, wpc, dcflag, true)
	    do i = 1, npts
	        x[i] = dex (w0 + (i-1) * wpc)
	    dcflag = 1
	    RV_X1(rv) = w0
	    RV_X2(rv) = x[npts]
	case PIXELS:
	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	        call d_printf (DBG_FD(rv), "\tPIXELS dispersion.\n")
	    do i = 1, npts 				# Pixel data
	       x[i] = real (i)
	    RV_X1(rv) = 1.0
	    RV_X2(rv) = real (npts - 1)
	case NONLINEAR:
	    if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	        call d_printf (DBG_FD(rv), "\tNONLINEAR dispersion.\n")
	    call rv_errmsg (
		"Nonlinear dispersions not yet support: image `%s[%d]'.\007")
                    call pargstr (IM_NAME(im))
		    call pargi (apnum)
	    return (ERR_READ)
	case ERR_READ:
	    call rv_errmsg ("Error getting dispersion info from header.")
	    return (ERR_READ)
	}
	RV_DCFLAG(rv) = dcflag

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	    call d_printf (DBG_FD(rv), "\texiting - rv_read\n")
	return (OK)
end


# RV_AP2LINE - Convert an aperture number to a line to be read in the image.
# Reads through the image header and parses the APNUMn keywords until the
# requested aperture is found.

int procedure rv_ap2line (rv, im, apnum)

pointer	rv					#I RV struct descriptor
pointer	im					#I Image descriptor
int	apnum					#I Requested aperture

int	i, line_num

begin
	# Open the WCS struct...
        if (RV_MWCSP(rv) == NULL)
            call rv_mwcs_open (rv)
        call rv_mwcs (im, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv),
            RVMW_DW(rv), RVMW_NW(rv), RVMW_NSPEC(rv))
	    
	#line_num = ERR
	line_num = 1
	do i = 1, RVMW_NSPEC(rv) {
	    if (RMW_AP(rv,i) == apnum) {			# Found it
	        line_num = i
	        break
	    }
        }

        call rv_mwcs_close (rv)
	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf (DBG_FD(rv), "rv_ap2line:  ap=%d  line_num = %d\n")
		call pargi(apnum);  call pargi(line_num)
	}
	return (line_num)
end
