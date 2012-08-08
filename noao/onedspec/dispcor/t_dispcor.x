include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	<mwset.h>
include	"dispcor.h"
include	"dctable.h"
include	<smw.h>
include	<units.h>
 
# Dispersion types.
define	MULTISPEC	1
define	ECHELLE		2

 
# T_DISPCOR -- Dispersion correct spectra.
 
procedure t_dispcor ()
 
int	in			# List of input spectra
int	out			# List of output spectra
bool	linearize		# Linearize spectra?
bool	log			# Log scale?
bool	flux			# Conserve flux?
real	blank			# Blank value
int	ignoreaps		# Ignore aperture numbers?
int	fd1			# Log file descriptor
int	fd2			# Log file descriptor
 
int	i, format, naps
int	open(), nowhite(), imtopenp(), imtgetim(), errcode(), btoi()
pointer	sp, input, output, str, err, stp, table
pointer	im, im1, smw, smw1, ap, immap(), smw_openim()
bool	clgetb()
real	clgetr()
errchk	open, immap, smw_openim, dc_gms, dc_gec, dc_multispec, dc_echelle
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (err, SZ_LINE, TY_CHAR)
 
	# Task parameters
	in = imtopenp ("input")
	out = imtopenp ("output")
	call clgstr ("records", Memc[str], SZ_LINE)
	call odr_openp (in, Memc[str])
	call odr_openp (out, Memc[str])
	call clgstr ("database", Memc[str], SZ_FNAME)
	call clgstr ("logfile", Memc[err], SZ_LINE)
	linearize = clgetb ("linearize")
	ignoreaps = btoi (clgetb ("ignoreaps"))
 
	# Initialize the database cacheing and wavelength table.
	call dc_open (stp, Memc[str])
	if (linearize) {
	    log = clgetb ("log")
	    flux = clgetb ("flux")
	    blank = clgetr ("blank")

	    call dc_table (table, naps)
	    if (clgetb ("global")) {
		if (clgetb ("samedisp"))
		    call dc_global1 (in, stp, log, table, naps)
		else
		    call dc_global (in, stp, log, table, naps)
	    }
	}

	# Open logfile if specified.
	if (clgetb ("verbose"))
	    fd1 = STDOUT
	if (nowhite (Memc[err], Memc[err], SZ_LINE) != 0)
	    fd2 = open (Memc[err], APPEND, TEXT_FILE)
	else
	    fd2 = NULL

	# Loop through each input image.  Do the dispersion correction
	# in place if no output spectrum list is given or if the input
	# and output spectra names are the same.
 
	while (imtgetim (in, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (out, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
		
	    iferr {
		im = NULL; im1 = NULL
		smw = NULL; smw1 = NULL
		ap = NULL

		i = immap (Memc[input], READ_ONLY, 0); im = i
		i = smw_openim (im); smw = i

		switch (SMW_FORMAT(smw)) {
		case SMW_ND:
		    # Use first line for reference.
		    switch (SMW_LDIM(smw)) {
		    case 1:
			call strcpy (Memc[input], Memc[str], SZ_LINE)
		    case 2:
			switch (SMW_LAXIS(smw,1)) {
			case 1:
			    call sprintf (Memc[str], SZ_LINE, "%s[*,1]")
				call pargstr (Memc[input])
			case 2:
			    call sprintf (Memc[str], SZ_LINE, "%s[1,*]")
				call pargstr (Memc[input])
			}
		    case 3:
			switch (SMW_LAXIS(smw,1)) {
			case 1:
			    call sprintf (Memc[str], SZ_LINE, "%s[*,1,1]")
				call pargstr (Memc[input])
			case 2:
			    call sprintf (Memc[str], SZ_LINE, "%s[1,*,1]")
				call pargstr (Memc[input])
			case 3:
			    call sprintf (Memc[str], SZ_LINE, "%s[*,1,1]")
				call pargstr (Memc[input])
			}
		    }
		    im1 = immap (Memc[str], READ_ONLY, 0)
		    smw1 = smw_openim (im1)
		    call smw_ndes (im1, smw1)
		    if (SMW_PDIM(smw1) == 1)
			call smw_esms (smw1)

		    call dc_gms (Memc[input], im1, smw1, stp, YES, ap, fd1, fd2)
		    call dc_ndspec (im, smw, smw1, ap, Memc[input],
			Memc[output], linearize, log, flux, blank, table, naps,
			fd1, fd2)
		default:
		    # Get dispersion functions.  Determine type of dispersion
		    # by the error return.

		    format = MULTISPEC
		    iferr (call dc_gms (Memc[input], im, smw, stp, ignoreaps,
			ap, fd1, fd2)) {
			if (errcode() > 1 && errcode() < 100)
			    call erract (EA_ERROR)
			format = ECHELLE
			iferr (call dc_gec (Memc[input], im, smw, stp, ap,
			    fd1, fd2)) {
			    if (errcode() > 1 && errcode() < 100)
				call erract (EA_ERROR)
			    call erract (EA_WARN)
			    iferr (call dc_gms (Memc[input], im, smw, stp,
				ignoreaps, ap, fd1, fd2))
				call erract (EA_WARN)
			    call sprintf (Memc[err], SZ_LINE,
				"%s: Dispersion data not found")
				call pargstr (Memc[input])
			    call error (1, Memc[err])
			}
		    }

		    switch (format) {
		    case MULTISPEC:
			call dc_multispec (im, smw, ap, Memc[input],
			    Memc[output], linearize, log, flux, blank, table,
			    naps, fd1, fd2)
		    case ECHELLE:
			call dc_echelle (im, smw, ap, Memc[input],
			    Memc[output], linearize, log, flux, blank, table,
			    naps, fd1, fd2)
		    }
		}
	    } then
		call erract (EA_WARN)

	    if (ap != NULL)
		call mfree (ap, TY_STRUCT)
	    if (smw1 != NULL)
		call smw_close (smw1)
	    if (im1 != NULL)
		call imunmap (im1)
	    if (smw != NULL)
		call smw_close (smw)
	    if (im != NULL)
		call imunmap (im)
	}

	# Finish up.
	if (linearize)
	    do i = 0, naps
		call mfree (Memi[table+i], TY_STRUCT)
	call mfree (table, TY_INT)
	call dc_close (stp)
	call imtclose (in)
	call imtclose (out)
	if (fd1 != NULL)
	    call close (fd1)
	if (fd2 != NULL)
	    call close (fd2)
	call sfree (sp)
end
 
 
# DC_NDSPEC -- Dispersion correct N-dimensional spectrum.
 
procedure dc_ndspec (in, smw, smw1, ap, input, output, linearize, log, flux,
	blank, table, naps, fd1, fd2)
 
pointer	in			# Input IMIO pointer
pointer	smw			# SMW pointer
pointer	smw1			# SMW pointer
pointer	ap			# Aperture pointer
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
bool	linearize		# Linearize?
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
real	blank			# Blank value
pointer	table			# Wavelength table
int	naps			# Number of apertures
int	fd1			# Log file descriptor
int	fd2			# Log file descriptor

int	i, j, nin, ndim, dispaxis, n1, n2, n3
pointer	sp, temp, str, out, mwout, cti, cto, indata, outdata
pointer	immap(), imgs3r(), imps3r(), mw_open(), smw_sctran()
bool	clgetb(), streq()
errchk	immap, mw_open, smw_open, dispcor, imgs3r, imps3r

begin
	# Determine the wavelength parameters.
	call dc_wavelengths (in, ap, output, log, table, naps, 1,
	    DC_AP(ap,1), DC_W1(ap,1), DC_W2(ap,1), DC_DW(ap,1), DC_NW(ap,1))
	DC_Z(ap,1) = 0.
	if (log)
	    DC_DT(ap,1) = 1
	else
	    DC_DT(ap,1) = 0
	
	call dc_log (fd1, output, ap, 1, log)
	call dc_log (fd2, output, ap, 1, log)

	if (clgetb ("listonly"))
	    return

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Open output image.  Use temp. image if output is the same as input.
	if (streq (input, output)) {
	    call mktemp ("temp", Memc[temp], SZ_LINE)
	    out = immap (Memc[temp], NEW_COPY, in)
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL
	} else {
	    out = immap (output, NEW_COPY, in)
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL
	}

	# Set dimensions.
	ndim = SMW_LDIM(smw)
	dispaxis = SMW_LAXIS(smw,1)
	n1 = DC_NW(ap,1)
	n2 = SMW_LLEN(smw,2)
	n3 = SMW_LLEN(smw,3)
	nin = IM_LEN(in,dispaxis)
	IM_LEN(out,dispaxis) = n1

	# Set WCS header.
	mwout = mw_open (NULL, ndim)
	call mw_newsystem (mwout, "world", ndim)
	do i = 1, ndim
	    call mw_swtype (mwout, i, 1, "linear", "")
	if (UN_LABEL(DC_UN(ap,1)) != EOS)
	    call mw_swattrs (mwout, dispaxis, "label", UN_LABEL(DC_UN(ap,1)))
	if (UN_UNITS(DC_UN(ap,1)) != EOS)
	    call mw_swattrs (mwout, dispaxis, "units", UN_UNITS(DC_UN(ap,1)))
	call smw_open (mwout, NULL, out)
	call smw_swattrs (mwout, INDEFI, INDEFI, INDEFI, INDEFI, DC_DT(ap,1),
	    DC_W1(ap,1), DC_DW(ap,1), DC_NW(ap,1), DC_Z(ap,1), INDEFR, INDEFR,
	    "")

	# Set WCS transformations.
	cti = smw_sctran (smw1, "world", "logical", 3)
	switch (dispaxis) {
	case 1:
	    cto = smw_sctran (mwout, "logical", "world", 1)
	case 2:
	    cto = smw_sctran (mwout, "logical", "world", 2)
	case 3:
	    cto = smw_sctran (mwout, "logical", "world", 4)
	}

	# Dispersion correct.
	do j = 1, n3 {
	    do i = 1, n2 {
		switch (dispaxis) {
		case 1:
		    indata = imgs3r (in, 1, nin, i, i, j, j)
		    outdata = imps3r (out, 1, n1, i, i, j, j)
		case 2:
		    indata = imgs3r (in, i, i, 1, nin, j, j)
		    outdata = imps3r (out, i, i, 1, n1, j, j)
		case 3:
		    indata = imgs3r (in, i, i, j, j, 1, nin)
		    outdata = imps3r (out, i, i, j, j, 1, n1)
		}

		call aclrr (Memr[outdata], n1)
		call dispcora (cti, 1, cto, INDEFI, Memr[indata], nin,
		    Memr[outdata], n1, flux, blank)
	    }
	}

	# Save REFSPEC keywords if present.
	call dc_refspec (out)
 
	# Finish up.  Replace input by output if needed.
	call smw_ctfree (cti)
	call smw_ctfree (cto)
	call smw_saveim (mwout, out)
	call smw_close (mwout)
	call imunmap (out)
	call imunmap (in)
	if (streq (input, output)) {
	    call imdelete (input)
	    call imrename (Memc[temp], output)
	}
 
	call sfree (sp)
end
 
 
# DC_MULTISPEC -- Linearize multispec apertures into an MULTISPEC format
# spectrum.  The number of pixels in each image line is the maximum
# required to contain the longest spectrum.
 
procedure dc_multispec (in, smw, ap, input, output, linearize, log, flux,
	blank, table, naps, fd1, fd2)
 
pointer	in			# Input IMIO pointer
pointer	smw			# SMW pointer
pointer	ap			# Aperture pointer
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
bool	linearize		# Linearize?
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
real	blank			# Blank value
pointer	table			# Wavelength table
int	naps			# Number of apertures
int	fd1			# Log file descriptor
int	fd2			# Log file descriptor
 
int	i, j, nc, nl, nb, axis[2]
pointer	sp, temp, str, out, mwout, cti, cto, indata, outdata
pointer	immap(), imgl3r(), impl3r()
pointer	mw_open(), smw_sctran()
bool	clgetb(), streq()
errchk	immap, mw_open, smw_open, dispcor, imgl3r, impl3r

data	axis/1,2/
 
begin
	# Determine the wavelength parameters for each aperture.
	# The options are to have all apertures have the same dispersion
	# or have each aperture have independent dispersion.  The global
	# parameters have already been calculated if needed.

	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	nb = IM_LEN(in,3)
 
	if (linearize) {
	    if (log)
		DC_DT(ap,1) = 1
	    else
		DC_DT(ap,1) = 0
	    if (clgetb ("samedisp")) {
		call dc_wavelengths1 (in, smw, ap, output, log, table, naps,
		    DC_W1(ap,1), DC_W2(ap,1), DC_DW(ap,1), DC_NW(ap,1))
		if ((DC_DW(ap,1)*(DC_W2(ap,1)-DC_W1(ap,1)) <= 0.) ||
		    (DC_NW(ap,1) < 1))
		    call error (1, "Error in wavelength scale")
		do i = 2, nl {
		    DC_W1(ap,i) = DC_W1(ap,1)
		    DC_W2(ap,i) = DC_W2(ap,1)
		    DC_DW(ap,i) = DC_DW(ap,1)
		    DC_NW(ap,i) = DC_NW(ap,1)
		    DC_Z(ap,i) = 0.
		    DC_DT(ap,i) = DC_DT(ap,1)
		}
	    } else {
		do i = 1, nl {
		    call dc_wavelengths (in, ap, output, log, table, naps, i,
			DC_AP(ap,i), DC_W1(ap,i), DC_W2(ap,i), DC_DW(ap,i),
			DC_NW(ap,i))
		    DC_Z(ap,i) = 0.
		    DC_DT(ap,i) = DC_DT(ap,1)
		}
	    }
	}
	call dc_log (fd1, output, ap, nl, log)
	call dc_log (fd2, output, ap, nl, log)

	if (clgetb ("listonly"))
	    return

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Use a temporary image if the output has the same name as the input.
	if (streq (input, output)) {
	    if (linearize) {
		call mktemp ("temp", Memc[temp], SZ_LINE)
		out = immap (Memc[temp], NEW_COPY, in)
		if (IM_PIXTYPE(out) != TY_DOUBLE)
		    IM_PIXTYPE(out) = TY_REAL
	    } else {
		call imunmap (in)
		i = immap (input, READ_WRITE, 0)
		in = i
		out = i
	    }
	} else {
	    out = immap (output, NEW_COPY, in)
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL
	}

	# Set MWCS or linearize
	if (!linearize) {
	    if (out != in)
		do j = 1, nb
		    do i = 1, nl
			call amovr (Memr[imgl3r(in,i,j)], Memr[impl3r(out,i,j)],
			    IM_LEN(in,1))
	    call smw_saveim (smw, out)
	} else {
	    if (nb > 1)
		i = 3
	    else
		i = 2
	    mwout = mw_open (NULL, i)
	    call mw_newsystem (mwout, "multispec", i)
	    call mw_swtype (mwout, axis, 2, "multispec", "")
	    if (UN_LABEL(DC_UN(ap,1)) != EOS)
		call mw_swattrs (mwout, 1, "label", UN_LABEL(DC_UN(ap,1)))
	    if (UN_UNITS(DC_UN(ap,1)) != EOS)
		call mw_swattrs (mwout, 1, "units", UN_UNITS(DC_UN(ap,1)))
	    if (i == 3)
		call mw_swtype (mwout, 3, 1, "linear", "")
	    call smw_open (mwout, NULL, out)
	    do i = 1, nl {
		call smw_swattrs (mwout, i, 1, DC_AP(ap,i), DC_BM(ap,i),
		    DC_DT(ap,i), DC_W1(ap,i), DC_DW(ap,i), DC_NW(ap,i),
		    DC_Z(ap,i), DC_LW(ap,i), DC_UP(ap,i), "")
		call smw_gapid (smw, i, 1, Memc[str], SZ_LINE)
		call smw_sapid (mwout, i, 1, Memc[str])
	    }

	    IM_LEN(out,1) = DC_NW(ap,1)
	    do i = 2, nl
		IM_LEN(out,1) = max (DC_NW(ap,i), IM_LEN(out,1))
	    cti = smw_sctran (smw, "world", "logical", 3)
	    cto = smw_sctran (mwout, "logical", "world", 3)
	    do j = 1, nb {
		do i = 1, nl {
		    indata = imgl3r (in, i, j)
		    outdata = impl3r (out, i, j)
		    call aclrr (Memr[outdata], IM_LEN(out,1))
		    call dispcora (cti, i, cto, i, Memr[indata], nc,
			Memr[outdata], DC_NW(ap,i), flux, blank)
		    if (DC_NW(ap,i) < IM_LEN(out,1))
			call amovkr (Memr[outdata+DC_NW(ap,i)-1],
			    Memr[outdata+DC_NW(ap,i)],IM_LEN(out,1)-DC_NW(ap,i))
		}
	    }
	    call smw_ctfree (cti)
	    call smw_ctfree (cto)
	    call smw_saveim (mwout, out)
	    call smw_close (mwout)
	}

	# Save REFSPEC keywords if present.
	call dc_refspec (out)
 
	# Finish up.  Replace input by output if needed.
	if (out == in) {
	    call imunmap (in)
	} else {
	    call imunmap (in)
	    call imunmap (out)
	    if (streq (input, output)) {
		call imdelete (input)
		call imrename (Memc[temp], output)
	    }
	}
 
	call sfree (sp)
end
 
 
# DC_ECHELLE -- Linearize echelle orders into an ECHELLE format
# spectrum.  The number of pixels in each image line is the maximum
# required to contain the longest spectrum.
 
procedure dc_echelle (in, smw, ap, input, output, linearize, log, flux,
	blank, table, naps, fd1, fd2)
 
pointer	in			# IMIO pointer
pointer	smw			# SMW pointers
pointer	ap			# Aperture pointer
char	input[ARB]		# Input multispec spectrum
char	output[ARB]		# Output root name
bool	linearize		# Linearize?
bool	log			# Log wavelength parameters?
bool	flux			# Conserve flux?
real	blank			# Blank value
pointer	table			# Wavelength table
int	naps			# Number of apertures
int	fd1			# Log file descriptor
int	fd2			# Log file descriptor
 
int	i, j, nc, nl, nb, axis[2]
pointer	sp, temp, str, out, mwout, cti, cto, indata, outdata
pointer	immap(), imgl3r(), impl3r()
pointer	mw_open(), smw_sctran()
bool	clgetb(), streq()
errchk	immap, mw_open, smw_open, dispcor, imgl3r, impl3r

data	axis/1,2/
 
begin
	# Determine the wavelength parameters for each aperture.
 
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	nb = IM_LEN(in,3)
 
	if (linearize) {
	    if (log)
		DC_DT(ap,1) = 1
	    else
		DC_DT(ap,1) = 0
	    do i = 1, nl {
		call dc_wavelengths (in, ap, output, log, table, naps,
		    i, DC_AP(ap,i), DC_W1(ap,i), DC_W2(ap,i), DC_DW(ap,i),
		    DC_NW(ap,i))
		DC_Z(ap,i) = 0.
		DC_DT(ap,i) = DC_DT(ap,1)
	    }
	}
	call dc_log (fd1, output, ap, nl, log)
	call dc_log (fd2, output, ap, nl, log)

	if (clgetb ("listonly"))
	    return

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Use a temporary image if the output has the same name as the input.
	if (streq (input, output)) {
	    if (linearize) {
		call mktemp ("temp", Memc[temp], SZ_LINE)
		out = immap (Memc[temp], NEW_COPY, in)
		if (IM_PIXTYPE(out) != TY_DOUBLE)
		    IM_PIXTYPE(out) = TY_REAL
	    } else {
		call imunmap (in)
		i = immap (input, READ_WRITE, 0)
		in = i
		out = i
	    }
	} else {
	    out = immap (output, NEW_COPY, in)
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL
	}

	# Set MWCS or linearize
	if (!linearize) {
	    if (out != in)
		do j = 1, nb
		    do i = 1, nl
			call amovr (Memr[imgl3r(in,i,j)], Memr[impl3r(out,i,j)],
			    IM_LEN(in,1))
	    call smw_saveim (smw, out)
	} else {
	    if (nb > 1)
		i = 3
	    else
		i = 2
	    mwout = mw_open (NULL, i)
	    call mw_newsystem (mwout, "multispec", i)
	    call mw_swtype (mwout, axis, 2, "multispec", "")
	    if (UN_LABEL(DC_UN(ap,1)) != EOS)
		call mw_swattrs (mwout, 1, "label", UN_LABEL(DC_UN(ap,1)))
	    if (UN_UNITS(DC_UN(ap,1)) != EOS)
		call mw_swattrs (mwout, 1, "units", UN_UNITS(DC_UN(ap,1)))
	    if (i == 3)
		call mw_swtype (mwout, 3, 1, "linear", "")
	    call smw_open (mwout, NULL, out)
	    do i = 1, nl {
		call smw_swattrs (mwout, i, 1, DC_AP(ap,i), DC_BM(ap,i),
		    DC_DT(ap,i), DC_W1(ap,i), DC_DW(ap,i), DC_NW(ap,i),
		    DC_Z(ap,i), DC_LW(ap,i), DC_UP(ap,i), "")
		call smw_gapid (smw, i, 1, Memc[str], SZ_LINE)
		call smw_sapid (mwout, i, 1, Memc[str])
	    }

	    IM_LEN(out,1) = DC_NW(ap,1)
	    do i = 2, nl
		IM_LEN(out,1) = max (DC_NW(ap,i), IM_LEN(out,1))
	    cti = smw_sctran (smw, "world", "logical", 3)
	    cto = smw_sctran (mwout, "logical", "world", 3)
	    do j = 1, nb {
		do i = 1, nl {
		    indata = imgl3r (in, i, j)
		    outdata = impl3r (out, i, j)
		    call aclrr (Memr[outdata], IM_LEN(out,1))
		    call dispcora (cti, i, cto, i, Memr[indata], nc,
			Memr[outdata], DC_NW(ap,i), flux, blank)
		    if (DC_NW(ap,i) < IM_LEN(out,1))
			call amovkr (Memr[outdata+DC_NW(ap,i)-1],
			    Memr[outdata+DC_NW(ap,i)],IM_LEN(out,1)-DC_NW(ap,i))
		}
	    }
	    call smw_ctfree (cti)
	    call smw_ctfree (cto)
	    call smw_saveim (mwout, out)
	    call smw_close (mwout)
	}

	# Save REFSPEC keywords if present.
	call dc_refspec (out)
 
	# Finish up.  Replace input by output if needed.
	if (out == in) {
	    call imunmap (in)
	} else {
	    call imunmap (in)
	    call imunmap (out)
	    if (streq (input, output)) {
		call imdelete (input)
		call imrename (Memc[temp], output)
	    }
	}
 
	call sfree (sp)
end
 
 
# DC_GLOBAL1 -- Set global wavelength parameters using the minimum and
# maximum wavelengths and and the minimum dispersion over all apertures.
 
procedure dc_global1 (in, stp, log, table, naps)
 
pointer	in			# Input list
pointer	stp			# Symbol table
bool	log			# Logarithmic scale?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, nwmax, imtgetim()
double	w1, w2, dw, wmin, wmax, dwmin
pointer	sp, input, str, im, mw, ap, tbl, immap(), smw_openim()
errchk	dc_gms, dc_gec, smw_openim
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Go through all the reference spectra and determine the
	# minimum and maximum wavelengths and maximum number of pixels.
	# If there is no entry in the wavelength table add it.

	wmin = MAX_REAL
	wmax = -MAX_REAL
	dwmin = MAX_REAL

	while (imtgetim (in, Memc[input], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[input], READ_ONLY, 0))
		next
	    mw = smw_openim (im)
	    switch (SMW_FORMAT(mw)) {
	    case SMW_ND:
		nwmax = SMW_NW(mw)
		dw = SMW_DW(mw)
		w1 = SMW_W1(mw)
		w2 = w1 + dw * (nwmax - 1)
		wmin = min (wmin, w1, w2)
		wmax = max (wmax, w1, w2)
		dwmin = min (dwmin, abs (dw))
	    default:
		iferr {
		    iferr (call dc_gms (Memc[input], im, mw, stp, NO, ap,
			NULL, NULL)) {
			iferr (call dc_gec (Memc[input], im, mw, stp, ap,
			    NULL, NULL)) {
			    call sprintf (Memc[str], SZ_LINE,
				"%s: Dispersion data not found")
				call pargstr (Memc[input])
			    call error (1, Memc[str])
			}
		    }
     
		    do i = 1, IM_LEN(im,2) {
			w1 = DC_W1(ap,i)
			w2 = DC_W2(ap,i)
			dw = DC_DW(ap,i)
			wmin = min (wmin, w1, w2)
			wmax = max (wmax, w1, w2)
			dwmin = min (dwmin, abs (dw))
		    }
		} then
		    ;
	    }

	    call mfree (ap, TY_STRUCT)
	    call smw_close (mw)
	    call imunmap (im)
	}
	call imtrew (in)

	nwmax = (wmax - wmin) / dwmin + 1.5
 
	# Enter the global entry in the first table entry.
	tbl = Memi[table]
	call dc_defaults (wmin, wmax, nwmax,
	    TBL_W1(tbl), TBL_W2(tbl), TBL_DW(tbl), TBL_NW(tbl))
 
	call sfree (sp)
end
 
 
# DC_GLOBAL -- Set global wavelength parameters.  This is done for each
# aperture separately.  The wavelength table may be used to specify separate
# fixed parameters for each aperture.
 
procedure dc_global (in, stp, log, table, naps)
 
pointer	in			# Input list
pointer	stp			# Symbol table
bool	log			# Logarithmic scale?
pointer	table			# Wavelength table
int	naps			# Number of apertures
 
int	i, j, nw, imtgetim()
double	w1, w2, dw
pointer	sp, input, str, im, mw, ap, tbl, immap(), smw_openim()
errchk	dc_gms, dc_gec, smw_openim
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Go through all the reference spectra and determine the
	# minimum and maximum wavelengths and maximum number of pixels.
	# Do this by aperture.  If there is no entry in the wavelength
	# table add it.
 
	while (imtgetim (in, Memc[input], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[input], READ_ONLY, 0))
		next
	    mw = smw_openim (im)
	    switch (SMW_FORMAT(mw)) {
	    case SMW_ND:
		tbl = Memi[table]
		nw = SMW_NW(mw)
		dw = SMW_DW(mw)
		w1 = SMW_W1(mw)
		w2 = w1 + dw * (nw - 1)
		TBL_WMIN(tbl) = min (TBL_WMIN(tbl), w1, w2)
		TBL_WMAX(tbl) = max (TBL_WMAX(tbl), w1, w2)
		TBL_NWMAX(tbl) = max (TBL_NWMAX(tbl), nw)
	    default:
		iferr {
		    iferr (call dc_gms (Memc[input], im, mw, stp, NO, ap,
			NULL, NULL)) {
			iferr (call dc_gec (Memc[input], im, mw, stp, ap,
			    NULL, NULL)) {
			    call sprintf (Memc[str], SZ_LINE,
				"%s: Dispersion data not found")
				call pargstr (Memc[input])
			    call error (1, Memc[str])
			}
		    }
     
		    do i = 1, IM_LEN(im,2) {
			call dc_getentry (false, DC_AP(ap,i), table, naps, j)
			tbl = Memi[table+j]
	 
			nw = DC_NW(ap,i)
			w1 = DC_W1(ap,i)
			w2 = DC_W2(ap,i)
			TBL_WMIN(tbl) = min (TBL_WMIN(tbl), w1, w2)
			TBL_WMAX(tbl) = max (TBL_WMAX(tbl), w1, w2)
			TBL_NWMAX(tbl) = max (TBL_NWMAX(tbl), nw)
		    }
		} then
		    ;
	    }

	    call mfree (ap, TY_STRUCT)
	    call smw_close (mw)
	    call imunmap (im)
	}
	call imtrew (in)
 
	do i = 0, naps {
	    tbl = Memi[table+i]
	    call dc_defaults (TBL_WMIN(tbl), TBL_WMAX(tbl), TBL_NWMAX(tbl),
		TBL_W1(tbl), TBL_W2(tbl), TBL_DW(tbl), TBL_NW(tbl))
	}

	call sfree (sp)
end
 

# DC_WAVELENGTHS1 -- Set output wavelength parameters for a spectrum.
# Fill in any INDEF values using the limits of the dispersion function
# over all apertures and the minimum dispersion over all apertures.  The
# user may then confirm and change the wavelength parameters if desired.
 
procedure dc_wavelengths1 (im, smw, ap, output, log, table, naps, w1, w2, dw,nw)
 
pointer	im		# IMIO pointer
pointer	smw		# SMW pointer
pointer	ap		# Aperture structure
char	output[ARB]	# Output image name
bool	log		# Logarithm wavelength parameters?
pointer	table		# Wavelength table
int	naps		# Number of apertures
double	w1, w2, dw	# Image wavelength parameters
int	nw		# Image wavelength parameter
 
int	i, n, nwt, clgeti(), clgwrd()
double	a, b, c, w1t, w2t, dwt, y1, y2, dy, clgetd()
pointer	sp, key, str, tbl
bool	clgetb()
 
begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get aperture parameters.
	tbl = Memi[table]
	w1t = TBL_W1(tbl)
	w2t = TBL_W2(tbl)
	dwt = TBL_DW(tbl)
	nwt = TBL_NW(tbl)
 
	# If there are undefined wavelength scale parameters get
	# defaults based on the reference spectrum.
 
	if (IS_INDEFD(w1t)||IS_INDEFD(w2t)||IS_INDEFD(dwt)||IS_INDEFD(nwt)) {
	    a = MAX_REAL
	    b = -MAX_REAL
	    c = MAX_REAL
 
	    do i = 1, IM_LEN(im,2) {
		n = DC_NW(ap,i)
		y1 = DC_W1(ap,i)
		y2 = DC_W2(ap,i)
		dy = DC_DW(ap,i)
		a = min (a, y1, y2)
		b = max (b, y1, y2)
		c = min (c, dy)
	    }
	    n = (b - a) / c + 1.5
	}
 
	call dc_defaults (a, b, n, w1t, w2t, dwt, nwt)
	w1 = w1t
	w2 = w2t
	dw = dwt
	nw = nwt
 
	# Print the wavelength scale and allow the user to confirm and
	# change the wavelength scale.  A test is done to check which
	# parameters the user changes and give them priority in filling
	# in the remaining parameters.
 
	if (TBL_CONFIRM(tbl) == YES) {
	    repeat {
		call printf ("%s: w1 = %g, w2 = %g, dw = %g, nw = %d\n")
		    call pargstr (output)
		    call pargd (w1)
		    call pargd (w2)
		    call pargd (dw)
		    call pargi (nw)
 
		i = clgwrd ("dispcor1.change", Memc[str],SZ_LINE, "|yes|no|NO|")
		switch (i) {
		case 2:
		    break
		case 3:
		    TBL_CONFIRM(tbl) = NO
		    break
		}
		call clputd ("dispcor1.w1", w1)
		call clputd ("dispcor1.w2", w2)
		call clputd ("dispcor1.dw", dw)
		call clputi ("dispcor1.nw", nw)
		a = w1
		b = w2
		c = dw
		n = nw
		w1 = clgetd ("dispcor1.w1")
		w2 = clgetd ("dispcor1.w2")
		dw = clgetd ("dispcor1.dw")
		nw = clgeti ("dispcor1.nw")
 
		# If no INDEF's set unchanged parameters to INDEF.
		i = 0
		if (IS_INDEFD(w1))
		    i = i + 1
		if (IS_INDEFD(w2))
		    i = i + 1
		if (IS_INDEFD(dw))
		    i = i + 1
		if (IS_INDEFI(nw))
		    i = i + 1
		if (i == 0) {
		    if (w1 == a)
		        w1 = INDEFD
		    if (w2 == b)
		        w2 = INDEFD
		    if (dw == c)
		        dw = INDEFD
		    if (nw == n)
		        nw = INDEFI
		}
 
	        call dc_defaults (a, b, n, w1, w2, dw, nw)
 
		if (clgetb ("global")) {
		    TBL_W1(tbl) = w1
		    TBL_W2(tbl) = w2
		    TBL_DW(tbl) = dw
		    TBL_NW(tbl) = nw
		}
	    }
	}
	call sfree (sp)
end
 
 
# DC_WAVELENGTHS -- Set output wavelength parameters for a spectrum for
# each aperture.  The fixed parameters are given in the wavelength table.
# If there is no entry in the table for an aperture use the global
# default (entry 0).  Fill in INDEF values using the limits and number
# of pixels for the aperture.  The user may then confirm and change the
# wavelength parameters if desired.
 
procedure dc_wavelengths (im, ap, output, log, table, naps, line, apnum,
	w1, w2, dw, nw)
 
pointer	im		# IMIO pointer
pointer	ap		# Aperture structure
char	output[ARB]	# Output image name
bool	log		# Logarithm wavelength parameters?
pointer	table		# Wavelength table
int	naps		# Number of apertures
int	line		# Line
int	apnum		# Aperture number
double	w1, w2, dw	# Image wavelength parameters
int	nw		# Image wavelength parameter
 
int	i, n, nwt, clgeti(), clgwrd()
double	a, b, c, w1t, w2t, dwt, clgetd()
pointer	sp, str, tbl
bool	clgetb()
 
begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get aperture parameters.
	call dc_getentry (false, apnum, table, naps, i)
	tbl = Memi[table+i]
 
	w1t = TBL_W1(tbl)
	w2t = TBL_W2(tbl)
	dwt = TBL_DW(tbl)
	nwt = TBL_NW(tbl)
 
	# If there are undefined wavelength scale parameters get
	# defaults based on the reference spectrum.
 
	if (IS_INDEFD(w1t)||IS_INDEFD(w2t)||IS_INDEFD(dwt)||IS_INDEFI(nwt)) {
	    a = DC_W1(ap,line)
	    b = DC_W2(ap,line)
	    n = DC_NW(ap,line)
	}
 
	call dc_defaults (a, b, n, w1t, w2t, dwt, nwt)
	w1 = w1t
	w2 = w2t
	dw = dwt
	nw = nwt
 
	# Print the wavelength scale and allow the user to confirm and
	# change the wavelength scale.  A test is done to check which
	# parameters the user changes and give them priority in filling
	# in the remaining parameters.
 
	if (TBL_CONFIRM(tbl) == YES) {
	    repeat {
		call printf (
		    "%s: ap = %d, w1 = %g, w2 = %g, dw = %g, nw = %d\n")
		    call pargstr (output)
		    call pargi (apnum)
		    call pargd (w1)
		    call pargd (w2)
		    call pargd (dw)
		    call pargi (nw)
		i = clgwrd ("dispcor1.change", Memc[str],SZ_LINE, "|yes|no|NO|")
		switch (i) {
		case 2:
		    break
		case 3:
		    TBL_CONFIRM(tbl) = NO
		    break
		}
		call clputd ("dispcor1.w1", w1)
		call clputd ("dispcor1.w2", w2)
		call clputd ("dispcor1.dw", dw)
		call clputi ("dispcor1.nw", nw)
		a = w1
		b = w2
		c = dw
		n = nw
		w1 = clgetd ("dispcor1.w1")
		w2 = clgetd ("dispcor1.w2")
		dw = clgetd ("dispcor1.dw")
		nw = clgeti ("dispcor1.nw")
 
		# If no INDEF's set unchanged parameters to INDEF.
		i = 0
		if (IS_INDEFD(w1))
		    i = i + 1
		if (IS_INDEFD(w2))
		    i = i + 1
		if (IS_INDEFD(dw))
		    i = i + 1
		if (IS_INDEFI(nw))
		    i = i + 1
		if (i == 0) {
		    if (w1 == a)
		        w1 = INDEFD
		    if (w2 == b)
		        w2 = INDEFD
		    if (dw == c)
		        dw = INDEFD
		    if (nw == n)
		        nw = INDEFI
		}
 
	        call dc_defaults (a, b, n, w1, w2, dw, nw)
 
		if (clgetb ("global")) {
		    TBL_W1(tbl) = w1
		    TBL_W2(tbl) = w2
		    TBL_DW(tbl) = dw
		    TBL_NW(tbl) = nw
		}
	    }
	}
	call sfree (sp)
end


# DC_DEFAULTS -- Given some set of wavelength scale with others undefined
# (INDEF) plus some defaults fill in the undefined parameters and make
# the wavelength scale consistent.  The logic of this task is complex
# and is meant to provide an "intelligent" result based on what users
# want.

procedure dc_defaults (a, b, n, w1, w2, dw, nw)

double	a		# Default wavelength endpoint
double	b		# Default wavelength endpoint
int	n		# Default number of pixels
double	w1		# Starting wavelength
double	w2		# Ending wavelength
double	dw		# Wavelength interval
int	nw		# Number of pixels

int	nindef

begin
	# Determine how many input parameters are specfied.
	nindef = 0
	if (IS_INDEFD(w1))
	    nindef = nindef + 1
	if (IS_INDEFD(w2))
	    nindef = nindef + 1
	if (IS_INDEFD(dw))
	    nindef = nindef + 1
	if (IS_INDEFI(nw))
	    nindef = nindef + 1

	# Depending on how many parameters are specified fill in the
	# INDEF parameters.

	switch (nindef) {
	case 0:
	    # All parameters specified.  First round NW to be consistent with
	    # w1, w2, and dw.  Then adjust w2 to nearest pixel.  It is possible
	    # that nw will be negative.  Checks for this should be made by the
	    # call in program.

	    nw = (w2 - w1) / dw + 1.5
	    w2 = w1 + dw * (nw - 1)
	case 1:
	    # Find the unspecified parameter and compute it from the other
	    # three specified parameters.  For nw need to adjust w2 to
	    # agree with a pixel.

	    if (IS_INDEFD(w1))
		w1 = w2 - dw * (nw - 1)
	    if (IS_INDEFD(w2))
		w2 = w1 + dw * (nw - 1)
	    if (IS_INDEFD(dw))
		dw = (w2 - w1) / (nw - 1)
	    if (IS_INDEFI(nw)) {
	        nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    }
	case 2:
	    # Fill in two unspecified parameters using the defaults.
	    # This is tricky.

	    if (IS_INDEFD(dw)) {
		if (IS_INDEFD(w1)) {
		    if (abs (w2 - a) > abs (w2 - b))
			w1 = a
		    else
			w1 = b
		    dw = (w2 - w1) / (nw - 1)
		} else if (IS_INDEFD(w2)) {
		    if (abs (w1 - a) > abs (w1 - b))
			w2 = a
		    else
			w2 = b
		    dw = (w2 - w1) / (nw - 1)
		} else {
		    dw = (b - a) / n
		    nw = abs ((w2 - w1) / dw) + 1.5
		    dw = (w2 - w1) / (nw - 1)
		}
	    } else if (IS_INDEFI(nw)) {
		if (IS_INDEFD(w1)) {
		    if (dw > 0.)
			w1 = min (a, b)
		    else
			w1 = max (a, b)
		    nw = (w2 - w1) / dw + 1.5
		    w1 = w2 - dw * (nw - 1)
		} else {
		    if (dw > 0.)
			w2 = max (a, b)
		    else
			w2 = min (a, b)
		    nw = (w2 - w1) / dw + 1.5
		    w2 = w1 + dw * (nw - 1)
		}
	    } else {
		if (dw > 0.)
		    w1 = min (a, b)
		else
		    w1 = max (a, b)
		w2 = w1 + dw * (nw - 1)
	    }
	case 3:
	    # Find the one specfied parameter and compute the others using
	    # the supplied defaults.

	    if (!IS_INDEFD(w1)) {
		if (abs (w1 - a) > abs (w1 - b))
		    w2 = a
		else
		    w2 = b
		dw = (b - a) / n
		nw = abs ((w2 - w1) / dw) + 1.5
		dw = (w2 - w1) / (nw - 1)
	    } else if (!IS_INDEFD(w2)) {
		if (abs (w2 - a) > abs (w2 - b))
		    w1 = a
		else
		    w1 = b
		dw = (b - a) / n
		nw = abs ((w2 - w1) / dw) + 1.5
		dw = (w2 - w1) / (nw - 1)
	    } else if (!IS_INDEFI(nw)) {
		w1 = min (a, b)
		w2 = max (a, b)
	        dw = (w2 - w1) / (nw - 1)
	    } else if (dw < 0.) {
		w1 = max (a, b)
		w2 = min (a, b)
		nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    } else {
		w1 = min (a, b)
		w2 = max (a, b)
		nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    }
	case 4:
	    # Given only defaults compute a wavelength scale.  The dispersion
	    # is kept close to the default.
	    w1 = min (a, b)
	    w2 = max (a, b)
	    dw = (b - a) / (n - 1)
	    nw = abs ((w2 - w1) / dw) + 1.5
	    dw = (w2 - w1) / (nw - 1)
	}
end


# DC_LOG -- Print log of wavlength paramters

procedure dc_log (fd, output, ap, naps, log)

int	fd		# Output file descriptor
char	output[ARB]	# Output image name
pointer	ap		# Aperture structure
int	naps		# Number of apertures
bool	log		# Log dispersion?

int	i

begin
	if (fd == NULL)
	    return

	for (i=2; i<=naps; i=i+1) {
	    if (DC_W1(ap,i) != DC_W1(ap,1))
		break
	    if (DC_W2(ap,i) != DC_W2(ap,1))
		break
	    if (DC_DW(ap,i) != DC_DW(ap,1))
		break
	    if (DC_NW(ap,i) != DC_NW(ap,1))
		break
	}

	if (naps == 1 || i <= naps) {
	    do i = 1, naps {
		call fprintf (fd,
		    "%s: ap = %d, w1 = %8g, w2 = %8g, dw = %8g, nw = %d")
		    call pargstr (output)
		    call pargi (DC_AP(ap,i))
		    call pargd (DC_W1(ap,i))
		    call pargd (DC_W2(ap,i))
		    call pargd (DC_DW(ap,i))
		    call pargi (DC_NW(ap,i))
		if (log) {
		    call fprintf (fd, ", log = %b")
			call pargb (log)
		}
		call fprintf (fd, "\n")
	    }
	} else {
	    call fprintf (fd,
		"%s: w1 = %8g, w2 = %8g, dw = %8g, nw = %d")
		call pargstr (output)
		call pargd (DC_W1(ap,1))
		call pargd (DC_W2(ap,1))
		call pargd (DC_DW(ap,1))
		call pargi (DC_NW(ap,1))
	    if (log) {
		call fprintf (fd, ", log = %b")
		    call pargb (log)
	    }
	    call fprintf (fd, "\n")
	}
	call flush (fd)
end


# DC_REFSPEC -- Save REFSPEC keywords in DCLOG keywords.

procedure dc_refspec (im)

pointer	im			#U IMIO pointer

int	i, j, imaccf()
pointer	sp, dckey, dcstr, refkey, refstr

begin
	call smark (sp)
	call salloc (dckey, SZ_FNAME, TY_CHAR)
	call salloc (dcstr, SZ_LINE, TY_CHAR)
	call salloc (refkey, SZ_FNAME, TY_CHAR)
	call salloc (refstr, SZ_LINE, TY_CHAR)

	for (i=1;; i=i+1) {
	    call sprintf (Memc[dckey], SZ_FNAME, "DCLOG%d")
		call pargi (i)
	    if (imaccf (im, Memc[dckey]) == NO)
		break
	}

	do j = 1, 4 {
	    if (j == 1)
		call strcpy ("REFSPEC1", Memc[refkey], SZ_FNAME)
	    else if (j == 2)
		call strcpy ("REFSPEC2", Memc[refkey], SZ_FNAME)
	    else if (j == 3)
		call strcpy ("REFSHFT1", Memc[refkey], SZ_FNAME)
	    else if (j == 4)
		call strcpy ("REFSHFT2", Memc[refkey], SZ_FNAME)

	    ifnoerr (call imgstr (im, Memc[refkey], Memc[refstr], SZ_LINE)) {
		call sprintf (Memc[dckey], SZ_FNAME, "DCLOG%d")
		    call pargi (i)
		call sprintf (Memc[dcstr], SZ_LINE, "%s = %s")
		    call pargstr (Memc[refkey])
		    call pargstr (Memc[refstr])
		call imastr (im, Memc[dckey], Memc[dcstr])
		call imdelf (im, Memc[refkey])
		i = i + 1
	    }
	}

	call sfree (sp)
end
