include	<fset.h>
include	<imhdr.h>
include	<error.h>

# T_TOONEDSPEC -- Convert long slit spectra to one dimensional spectra.

define	COMAXBUF	100000	# Maximum column buffer.

procedure t_toonedspec ()

int	inlist			# Input image list
int	outlist			# Output image list
int	startrec		# Starting output record number
int	beamnum			# Output beam number
int	first			# First line or column
int	last			# Last line or column
int	step			# Step size
int	nsum			# Number of lines or columns to sum
bool	verbose			# Verbose output

char	list[SZ_LINE]
char	image1[SZ_FNAME], image2[SZ_FNAME], image3[SZ_FNAME]
int	i, i1, j, npts, rec, beam, dispaxis
real	rval
pointer	in, out, co, data, outbuf

int	imtopen(), imtgetim(), clgeti(), imgeti()
real	imgetr()
bool	clgetb()
pointer	immap(), comap(), impl1r()
errchk	immap, imgeti, imgetr, imgstr

begin
	# Get task parameters.
	call clgstr ("input", list, SZ_LINE)
	inlist = imtopen (list)
	call clgstr ("output", list, SZ_LINE)
	outlist = imtopen (list)

	startrec = clgeti ("startrec")
	beamnum = clgeti ("beamnum")
	first = clgeti ("first")
	last = clgeti ("last")
	step = clgeti ("step")
	nsum = clgeti ("nsum")
	verbose = clgetb ("verbose")

	call fseti (STDOUT, F_FLUSHNL, YES)

	# Loop through input images.

10	while ((imtgetim (inlist, image1, SZ_FNAME) != EOF) &&
	       (imtgetim (outlist, image2, SZ_FNAME) != EOF)) {

	    iferr (in = immap (image1, READ_ONLY, 0)) {
		call erract (EA_WARN)
		goto 10
	    }

	    iferr (dispaxis = imgeti (in, "dispaxis")) {
		call imunmap (in)
		call erract (EA_WARN)
		goto 10
	    }

	    if (IM_NDIM (in) != 2) {
		call imunmap (in)
		call eprintf ("Input image %s is not two dimensional\n")
		    call pargstr (image1)
		goto 10
	    }

	    rec = startrec - 1
	    i1 = last

	    switch (dispaxis) {
	    case 1:
		npts = IM_LEN (in, 1)
		if (IS_INDEFI (i1))
		    i1 = IM_LEN (in, 2)
		do i = first, i1, step {
		    if (i > IM_LEN (in, 2))
			break
		    rec = rec + 1
		    if (IS_INDEFI (beamnum))
			beam = i
		    else
			beam = beamnum

		    call sprintf (image3, SZ_FNAME, "%s.%04d")
			call pargstr (image2)
			call pargi (rec)

		    iferr (out = immap (image3, NEW_COPY, in)) {
			call erract (EA_WARN)
			next
		    }

		    IM_PIXTYPE (out) = TY_REAL
		    IM_NDIM (out) = 1
		    call imaddi (out, "beam-num", beam)
		    iferr (call imdelf (out, "crpix2"))
			;
		    iferr (call imdelf (out, "crval2"))
			;
		    iferr (call imdelf (out, "cdelt2"))
			;
		    iferr (call imdelf (out, "ctype2"))
			;
		    iferr (call imdelf (out, "cunit2"))
			;

		    outbuf = impl1r (out)
		    j = min (i + nsum - 1, IM_LEN (in, 2))
		    if (step < nsum)
			call xt_lsumb (in, 1, npts, i, j, data)
		    else
			call xt_lsum (in, 1, npts, i, j, data)
		    call amovr (Memr[data], Memr[outbuf], npts)

		    call mfree (data, TY_REAL)
		    call imunmap (out)

		    if (verbose) {
			call printf ("Lines %d to %d of %s -> %s\n")
			    call pargi (i)
			    call pargi (j)
			    call pargstr (image1)
			    call pargstr (image3)
		    }
		}
	    case 2:
		co = comap (in, COMAXBUF)
		npts = IM_LEN (in, 2)
		if (IS_INDEFI (i1))
		    i1 = IM_LEN (in, 1)
		do i = first, i1, step {
		    if (i > IM_LEN (in, 1))
			break
		    rec = rec + 1
		    if (IS_INDEFI (beamnum))
			beam = i
		    else
			beam = beamnum

		    call sprintf (image3, SZ_FNAME, "%s.%04d")
			call pargstr (image2)
			call pargi (rec)

		    iferr (out = immap (image3, NEW_COPY, in)) {
			call erract (EA_WARN)
			next
		    }

		    IM_PIXTYPE (out) = TY_REAL
		    IM_NDIM (out) = 1
		    IM_LEN (out, 1) = npts
		    call imaddi (out, "beam-num", beam)
		    call imaddi (out, "dispaxis", 1)
		    iferr {
			rval = imgetr (out, "crpix2")
			call imdelf (out, "crpix2")
			call imaddr (out, "crpix1", rval)
		    } then
			;
		    iferr {
			rval = imgetr (out, "crval2")
			call imdelf (out, "crval2")
			call imaddr (out, "crval1", rval)
		    } then
			;
		    iferr {
			rval = imgetr (out, "cdelt2")
			call imdelf (out, "cdelt2")
			call imaddr (out, "cdelt1", rval)
		    } then
			;
		    iferr {
			call imgstr (out, "ctype2", list, SZ_LINE)
			call imdelf (out, "ctype2")
			call imastr (out, "ctype1", list)
		    } then
			;
		    iferr {
			call imgstr (out, "cunit2", list, SZ_LINE)
			call imdelf (out, "cunit2")
			call imastr (out, "cunit1", list)
		    } then
			;

		    outbuf = impl1r (out)
		    j = min (i + nsum - 1, IM_LEN (in, 1))
		    if (step < nsum)
			call xt_csumb (co, i, j, 1, npts, data)
		    else
			call xt_csum (co, i, j, 1, npts, data)
		    call amovr (Memr[data], Memr[outbuf], npts)

		    call mfree (data, TY_REAL)
		    call imunmap (out)

		    if (verbose) {
			call printf ("Columns %d to %d of %s -> %s\n")
			    call pargi (i)
			    call pargi (j)
			    call pargstr (image1)
			    call pargstr (image3)
		    }
		}
		call counmap (co)
	    }
	    call imunmap (in)
	}

	call imtclose (inlist)
	call imtclose (outlist)
end
