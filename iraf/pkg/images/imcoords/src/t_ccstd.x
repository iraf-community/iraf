include <fset.h>
include	<ctype.h>
include <math.h>
include <pkg/skywcs.h>


define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops

# T_CCSTD -- Transform a list of x and y and RA and DEC coordinates to
# their polar coordinate equivalents, after appying an optional linear
# transformation to the x and y side

procedure t_ccstd()

bool	forward, polar
int	inlist, outlist, reclist, infd, outfd
int	xcolumn, ycolumn, lngcolumn, latcolumn, lngunits, latunits
int	geometry, min_sigdigits
pointer	sp, infile, outfile, record, str, dt, sx1, sy1, sx2, sy2, coo, mw
pointer	xformat, yformat, lngformat, latformat
bool	clgetb(), streq()
int	fntopnb(), imtopenp(), fntlenb(), imtlen(), fntgfnb(), imtgetim()
int	open(), clgwrd(), clgeti()
pointer	dtmap()

begin
	# Allocate memory for transformation parameters structure
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (record, SZ_FNAME, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
	call salloc (lngformat, SZ_FNAME, TY_CHAR)
	call salloc (latformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the input and output file lists.
	call clgstr ("input", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS)
	    call strcpy ("STDIN", Memc[str], SZ_FNAME)
	inlist = fntopnb(Memc[str], NO)
	call clgstr ("output", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS)
	    call strcpy ("STDOUT", Memc[str], SZ_FNAME)
	outlist = fntopnb (Memc[str], NO)
	call clgstr ("database", Memc[str], SZ_FNAME)
	if (Memc[str] != EOS) {
	    dt = dtmap (Memc[str], READ_ONLY)
	    reclist = imtopenp ("solutions")
	    geometry = clgwrd ("geometry", Memc[str], SZ_LINE,
	        ",linear,distortion,geometric,")
	} else {
	    dt = NULL
	    reclist = NULL
	    geometry = 0
	}
	forward = clgetb ("forward")
	polar = clgetb ("polar")

	# Test the input and out file and record lists for validity.
	if (fntlenb(inlist) <= 0)
	    call error (0, "The input file list is empty")
	if (fntlenb(outlist) <= 0)
	    call error (0, "The output file list is empty")
	if (fntlenb(outlist) > 1 && fntlenb(outlist) != fntlenb(inlist))
	    call error (0,
	        "Input and output file lists are not the same length")
	if (dt != NULL && reclist != NULL) {
	    if (imtlen (reclist) > 1 && imtlen (reclist) != fntlenb (inlist)) 
	        call error (0,
		    "Input file and record lists are not the same length.")
	}

	# Get the input file format parameters.
	xcolumn = clgeti ("xcolumn")
	ycolumn = clgeti ("ycolumn")
	lngcolumn = clgeti ("lngcolumn")
	latcolumn = clgeti ("latcolumn")
	iferr (lngunits = clgwrd ("lngunits", Memc[str], SZ_LINE, 
	    SKY_LNG_UNITLIST))
	    lngunits = 0
	iferr (latunits = clgwrd ("latunits", Memc[str], SZ_LINE, 
	    SKY_LAT_UNITLIST))
	    latunits = 0

	# Get the output file format parameters.
	call clgstr ("lngformat", Memc[lngformat], SZ_FNAME)
	call clgstr ("latformat", Memc[latformat], SZ_FNAME)
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)
	min_sigdigits = clgeti ("min_sigdigits")

	# Get the output file name.
	if (fntgfnb (outlist, Memc[outfile], SZ_FNAME) == EOF)
	    call strcpy ("STDOUT", Memc[outfile], SZ_FNAME)
	outfd = open (Memc[outfile], NEW_FILE, TEXT_FILE)
	if (streq (Memc[outfile], "STDOUT") || outfd == STDOUT)
	    call fseti (outfd, F_FLUSHNL, YES)

	# Get the record name.
	if (reclist == NULL)
	    Memc[record] = EOS
	else if (imtgetim (reclist, Memc[record], SZ_FNAME) == EOF)
	    Memc[record] = EOS

	# Call procedure to get parameters and fill structure.
	coo = NULL; sx1 = NULL; sy1 = NULL; sx2 = NULL; sy2 = NULL
	call cc_init_std (dt, Memc[record], geometry, lngunits,
	    latunits, sx1, sy1, sx2, sy2, mw, coo)

	# While input list is not depleted, open file and transform list.
	while (fntgfnb (inlist, Memc[infile], SZ_FNAME) != EOF)  {

	    infd = open (Memc[infile], READ_ONLY, TEXT_FILE)

	    # Transform the coordinates.
	    call cc_transform_std (infd, outfd, xcolumn, ycolumn, lngcolumn,
	        latcolumn, lngunits, latunits, Memc[xformat], Memc[yformat],
		Memc[lngformat], Memc[latformat], min_sigdigits, sx1, sy1, sx2,
		sy2, mw, coo, forward, polar)

	    # Do not get a new output file name if there is not output 
	    # file list or if only one output file was specified.
	    # Otherwise fetch the new name.
	    if (fntlenb(outlist) > 1) {
		call close (outfd)
	        if (fntgfnb (outlist, Memc[outfile], SZ_FNAME) != EOF)
		    outfd = open (Memc[outfile], NEW_FILE, TEXT_FILE) 
		if (streq (Memc[outfile], "STDOUT") || outfd == STDOUT)
	            call fseti (outfd, F_FLUSHNL, YES)
	    }

	    call close (infd)

	    # Do not reset the transformation if there is no record list
	    # or only one record is specified. Otherwise fetch the next
	    # record name.
	    if (reclist != NULL && imtlen (reclist) > 1) {
	        if (imtgetim (reclist, Memc[record], SZ_FNAME) != EOF) {
		    call cc_free_std (sx1, sy1, sx2, sy2, mw, coo)
	            call cc_init_std (dt, Memc[record], geometry,
		        lngunits, latunits, sx1, sy1, sx2, sy2, mw, coo)
		}
	    }
	}

	# Free the surface descriptors.
	call cc_free_std (sx1, sy1, sx2, sy2, mw, coo)

	# Close up file and record templates.
	if (dt != NULL)
	    call dtunmap (dt)
	call close (outfd)
	call fntclsb (inlist)
	call fntclsb (outlist)
	if (reclist != NULL)
	    call imtclose (reclist)
	call sfree (sp)

end


# CC_TRANSFORM_STD -- This procedure is called once for each file in the
# input list.  For each line in the input file that isn't blank or comment,
# the line is transformed.  Blank and comment lines are output unaltered.

procedure cc_transform_std (infd, outfd, xfield, yfield, lngfield, latfield,
	lngunits, latunits, xformat, yformat, lngformat, latformat,
	min_sigdigits, sx1, sy1, sx2, sy2, mw, coo, forward, polar)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
int	xfield			#I the x column number
int	yfield			#I the y column number
int	lngfield		#I the ra / longitude column number
int	latfield		#I the dec / latitude column number
int	lngunits		#I the ra / longitude units
int	latunits		#I the dec / latitude units
char	xformat[ARB]		#I output format of the r / x coordinate
char	yformat[ARB]		#I output format of the t / y coordinate
char	lngformat[ARB]		#I output format of the r / longitude coordinate
char	latformat[ARB]		#I output format of the t / latitude coordinate
int	min_sigdigits		#I the minimum number of digits to be output
pointer	sx1, sy1		#I pointers to the linear x and y surfaces
pointer	sx2, sy2		#I pointers to the x and y distortion surfaces
pointer	mw			#I pointer to the mwcs structure
pointer	coo			#I pointer to the celestial coordinate structure
bool	forward			#I Is the transform in the forward direction ?
bool	polar			#I Polar standard coordinates ?

double	xd, yd, lngd, latd, txd, tyd, tlngd, tlatd
int	max_fields, nline, nfields, nchars
int	offset, tlngunits, tlatunits
pointer	sp, inbuf, linebuf, field_pos, outbuf, ip, ct
pointer	vfields, values, nsdigits, vformats
int	getline(), li_get_numd(), sk_stati()
pointer	mw_sctran()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)
	call salloc (outbuf, SZ_LINE, TY_CHAR)
	call salloc (vfields, 4, TY_INT)
	call salloc (values, 4, TY_DOUBLE)
	call salloc (nsdigits, 4, TY_INT)
	call salloc (vformats, (SZ_FNAME + 1) * 4, TY_CHAR)

	# Determine the longitude and latitude units.
	if (lngunits <= 0) {
	    if (coo == NULL)
		tlngunits = SKY_HOURS
	    else
	        tlngunits = sk_stati (coo, S_NLNGUNITS)
	} else
	    tlngunits = lngunits
	if (latunits <= 0) {
	    if (coo == NULL)
		tlatunits = SKY_DEGREES
	    else
	        tlatunits = sk_stati (coo, S_NLATUNITS)
	} else
	    tlatunits = latunits

	# Set the output fields.
	Memi[vfields] = xfield
	Memi[vfields+1] = yfield
	Memi[vfields+2] = lngfield
	Memi[vfields+3] = latfield

	# If the formats are undefined set suitable default formats.
	if (lngformat[1] == EOS) {
	    if (forward)
		call strcpy ("%10.3f", Memc[vformats+2*(SZ_FNAME+1)], SZ_FNAME)
	    else {
		switch (tlngunits) {
		case SKY_HOURS:
		    call strcpy ("%12.2h", Memc[vformats+2*(SZ_FNAME+1)],
		        SZ_FNAME)
		case SKY_DEGREES:
		    call strcpy ("%11.1h", Memc[vformats+2*(SZ_FNAME+1)],
		        SZ_FNAME)
		case SKY_RADIANS:
		    call strcpy ("%13.7g", Memc[vformats+2*(SZ_FNAME+1)],
		        SZ_FNAME)
		default:
		    call strcpy ("%10.3f", Memc[vformats+2*(SZ_FNAME+1)],
		        SZ_FNAME)
		}
	    }
	} else
	    call strcpy (lngformat, Memc[vformats+2*(SZ_FNAME+1)], SZ_FNAME)

	if (latformat[1] == EOS) {
	    if (forward)
		call strcpy ("%10.3f", Memc[vformats+3*(SZ_FNAME+1)], SZ_FNAME)
	    else {
		switch (tlatunits) {
		case SKY_HOURS:
		    call strcpy ("%12.2h", Memc[vformats+3*(SZ_FNAME+1)],
		        SZ_FNAME)
		case SKY_DEGREES:
		    call strcpy ("%11.1h", Memc[vformats+3*(SZ_FNAME+1)],
		        SZ_FNAME)
		case SKY_RADIANS:
		    call strcpy ("%13.7g", Memc[vformats+3*(SZ_FNAME+1)],
		        SZ_FNAME)
		default:
		    call strcpy ("%10.3f", Memc[vformats+3*(SZ_FNAME+1)],
		        SZ_FNAME)
		}
	    }
	} else
	    call strcpy (latformat, Memc[vformats+3*(SZ_FNAME+1)], SZ_FNAME)

	if (xformat[1] == EOS)
	    call strcpy ("%10.3f", Memc[vformats], SZ_FNAME)
	else
	    call strcpy (xformat, Memc[vformats], SZ_FNAME)
	if (yformat[1] == EOS)
	    call strcpy ("%10.3f", Memc[vformats+(SZ_FNAME+1)], SZ_FNAME)
	else
	    call strcpy (yformat, Memc[vformats+(SZ_FNAME+1)], SZ_FNAME)


	# If the transformation can be represented by mwcs then compile the
	# appropriate transform. Other wise use the surface fitting code
	# to do the transformation.
	if (mw != NULL) {
	    if (forward)
	        ct = mw_sctran (mw, "world", "logical", 03B)
	    else
	        ct = mw_sctran (mw, "logical", "world", 03B)
	}

	max_fields = MAX_FIELDS
	for (nline=1;  getline (infd, Memc[inbuf]) != EOF;  nline = nline + 1) {

	    for (ip=inbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;

	    if (Memc[ip] == '#') {
		# Pass comment lines on to the output unchanged.
		call putline (outfd, Memc[inbuf])
		next
	    } else if (Memc[ip] == '\n' || Memc[ip] == EOS) {
		# Blank lines too.
		call putline (outfd, Memc[inbuf])
		next
	    }

	    # If the transformation is undefined then pass the line on
	    # undisturbed.
	    if (mw == NULL) {
		call putline (outfd, Memc[inbuf])
		next
	    }

	    # Expand tabs into blanks, determine field offsets.
	    call strdetab (Memc[inbuf], Memc[linebuf], SZ_LINE, TABSIZE)
	    call li_find_fields (Memc[linebuf], Memi[field_pos], max_fields,
	        nfields)

	    # Check that all the data is present.
	    if (lngfield > nfields || latfield > nfields || xfield > nfields ||
		yfield > nfields) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Not enough fields in file %s line %d\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }
		 
	    # Read the longitude / latitude or rstd / thetastd coordinates.
            offset = Memi[field_pos+lngfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], lngd,
	        Memi[nsdigits+2])
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad lng / xi value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }
            offset = Memi[field_pos+latfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], latd,
	        Memi[nsdigits+3])
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad lat / eta value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }

	    # Read the x and y or r and theta coordinates.
            offset = Memi[field_pos+xfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], xd, Memi[nsdigits])
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad x / r value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }
            offset = Memi[field_pos+yfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], yd, Memi[nsdigits+1])
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad y / theta value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }

	    # Transform the longitude / latitude coordinates in lngunits /
	    # latunits to / from the xi / eta coordinates in arcseconds, and
	    # transform the x and y coordinates to or from the r and theta
	    # coordinates.
	    if (forward) {
		switch (tlngunits) {
		case SKY_RADIANS:
		    tlngd = RADTODEG(lngd)
		case SKY_HOURS:
		    tlngd = 15.0d0 * lngd
		default:
		    tlngd = lngd
		}
		switch (tlatunits) {
		case SKY_RADIANS:
		    tlatd = RADTODEG(latd)
		case SKY_HOURS:
		    tlatd = 15.0d0 * latd
		default:
		    tlatd = latd
		}
		txd = xd
		tyd = yd
	    } else if (polar) {
		tlngd = lngd * cos (DEGTORAD(latd)) / 3600.0d0
		tlatd = lngd * sin (DEGTORAD(latd)) / 3600.0d0
		txd = xd * cos (DEGTORAD(yd))
		tyd = xd * sin (DEGTORAD(yd))
	    } else {
		tlngd = lngd / 3600.0d0
		tlatd = latd / 3600.0d0
		txd = xd
		tyd = yd
	    }
	    call mw_c2trand (ct, tlngd, tlatd, lngd, latd)
	    call cc_do_std (txd, tyd, xd, yd, sx1, sy1, sx2, sy2, forward)
	    if (! forward) {
	        switch (tlngunits) {
		case SKY_RADIANS:
		    Memd[values+2] = DEGTORAD(lngd)
		case SKY_HOURS:
		    Memd[values+2] = lngd / 15.0d0
		default:
		    Memd[values+2] = lngd
		}
		switch (tlatunits) {
		case SKY_RADIANS:
		    Memd[values+3] = DEGTORAD(latd)
		case SKY_HOURS:
		    Memd[values+3] = latd / 15.0d0
		default:
		    Memd[values+3] = latd
		}
		Memd[values] = xd
		Memd[values+1] = yd
	    } else if (polar) {
		Memd[values] = sqrt (xd * xd + yd * yd)
		Memd[values+1] = RADTODEG(atan2 (yd, xd))
		if (Memd[values+1] < 0.0d0)
		    Memd[values+1] = Memd[values+1] + 360.0d0
		Memd[values+2] = sqrt (lngd * lngd + latd * latd) * 3600.0d0
		Memd[values+3] = RADTODEG (atan2 (latd, lngd))
		if (Memd[values+3] < 0.0d0)
		    Memd[values+3] = Memd[values+3] + 360.0d0
	    } else {
		Memd[values] = xd
		Memd[values+1] = yd
		Memd[values+2] = lngd * 3600.0d0
		Memd[values+3] = latd * 3600.0d0
	    }

	    # Format the output line.
	    call li_npack_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
	        Memi[field_pos], nfields, Memi[vfields], Memd[values],
	        Memi[nsdigits], 4, Memc[vformats], SZ_FNAME, min_sigdigits)

	    call putline (outfd, Memc[outbuf])
	}

	if (ct != NULL)
	    call mw_ctfree (ct)

	call sfree (sp)
end
