include <fset.h>
include	<ctype.h>
include <math.h>
include <pkg/skywcs.h>


define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops

# T_CCTRAN -- Transform a list of x and y coordinates to RA nad DEC or vice
# versa using the celestial coordinate transformation computed by the CCMAP
# task.

procedure t_cctran()

bool	forward
int	inlist, outlist, reclist, geometry, xcolumn, ycolumn, min_sigdigits
int	infd, outfd, lngunits, latunits
pointer	sp, infile, outfile, record, xformat, yformat, str, dt
pointer	sx1, sy1, sx2, sy2, coo, mw
bool	clgetb(), streq()
int	fntopnb(), imtopenp(), fntlenb(), fntgfnb(), clgwrd(), clgeti()
int	open(), imtgetim (), imtlen()
pointer	dtmap()

begin
	# Allocate memory for transformation parameters structure
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (record, SZ_FNAME, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
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
	    reclist = imtopenp ("solution")
	} else {
	    dt = NULL
	    reclist = NULL
	}

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

	# Get the fitting geometry.
	geometry = clgwrd ("geometry", Memc[str], SZ_LINE,
	    ",linear,distortion,geometric,")
	forward = clgetb ("forward")

	# Get the input and output file parameters.
        iferr (lngunits = clgwrd ("lngunits", Memc[str], SZ_LINE,
            SKY_LNG_UNITLIST))
            lngunits = 0
        iferr (latunits = clgwrd ("latunits", Memc[str], SZ_LINE,
            SKY_LAT_UNITLIST))
            latunits = 0
	xcolumn = clgeti ("xcolumn")
	ycolumn = clgeti ("ycolumn")
	call clgstr ("lngformat", Memc[xformat], SZ_FNAME)
	call clgstr ("latformat", Memc[yformat], SZ_FNAME)
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
	call cc_init_transform (dt, Memc[record], geometry, lngunits,
	    latunits, sx1, sy1, sx2, sy2, mw, coo)

	# While input list is not depleted, open file and transform list.
	while (fntgfnb (inlist, Memc[infile], SZ_FNAME) != EOF)  {

	    infd = open (Memc[infile], READ_ONLY, TEXT_FILE)

	    # Transform the coordinates.
	    call cc_transform_file (infd, outfd, xcolumn, ycolumn, lngunits, 
	        latunits, Memc[xformat], Memc[yformat], min_sigdigits, sx1,
		sy1, sx2, sy2, mw, coo, forward)

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
		    call cc_free_transform (sx1, sy1, sx2, sy2, mw, coo)
	            call cc_init_transform (dt, Memc[record], geometry,
		        lngunits, latunits, sx1, sy1, sx2, sy2, mw, coo)
		}
	    }
	}

	# Free the surface descriptors.
	call cc_free_transform (sx1, sy1, sx2, sy2, mw, coo)

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


# CC_TRANSFORM_FILE -- This procedure is called once for each file
# in the input list.  For each line in the input file that isn't
# blank or comment, the line is transformed.  Blank and comment
# lines are output unaltered.

procedure cc_transform_file (infd, outfd, xfield, yfield, lngunits,
	latunits, xformat, yformat, min_sigdigits, sx1, sy1, sx2, sy2,
	mw, coo, forward)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
int	xfield			#I the x column number
int	yfield			#I the y column number
int     lngunits                #I the ra / longitude units
int     latunits                #I the dec / latitude units
char	xformat[ARB]		#I output format of the x coordinate
char	yformat[ARB]		#I output format of the y coordinate
int	min_sigdigits		#I the minimum number of digits to be output
pointer	sx1, sy1		#I pointers to the linear x and y surfaces
pointer	sx2, sy2		#I pointers to the x and y distortion surfaces
pointer	mw			#I pointer to the mwcs structure
pointer	coo			#I pointer to the celestial coordinate structure
bool	forward			#I forwards transform ?

double	xd, yd, xtd, ytd
int	max_fields, nline, nfields, nchars, nsdig_x, nsdig_y, offset
int	tlngunits, tlatunits
pointer	sp, inbuf, linebuf, field_pos, outbuf, ip, ct, txformat, tyformat
int	getline(), li_get_numd(), sk_stati()
pointer	mw_sctran()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)
	call salloc (outbuf, SZ_LINE, TY_CHAR)
	call salloc (txformat, SZ_LINE, TY_CHAR)
	call salloc (tyformat, SZ_LINE, TY_CHAR)

        # Determine the units.
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

	# If the formats are undefined set suitable default formats.
	if (xformat[1] == EOS) {
	    if (! forward)
		call strcpy ("%10.3f", Memc[txformat], SZ_FNAME)
	    else {
		switch (tlngunits) {
		case SKY_HOURS:
		    call strcpy ("%12.2h", Memc[txformat], SZ_FNAME)
		case SKY_DEGREES:
		    call strcpy ("%11.1h", Memc[txformat], SZ_FNAME)
		case SKY_RADIANS:
		    call strcpy ("%13.7g", Memc[txformat], SZ_FNAME)
		default:
		    call strcpy ("%10.3f", Memc[txformat], SZ_FNAME)
		}
	    }
	} else
	    call strcpy (xformat, Memc[txformat], SZ_FNAME)

	if (yformat[1] == EOS) {
	    if (! forward)
		call strcpy ("%10.3f", Memc[tyformat], SZ_FNAME)
	    else {
		switch (tlatunits) {
		case SKY_HOURS:
		    call strcpy ("%12.2h", Memc[tyformat], SZ_FNAME)
		case SKY_DEGREES:
		    call strcpy ("%11.1h", Memc[tyformat], SZ_FNAME)
		case SKY_RADIANS:
		    call strcpy ("%13.7g", Memc[tyformat], SZ_FNAME)
		default:
		    call strcpy ("%10.3f", Memc[tyformat], SZ_FNAME)
		}
	    }
	} else
	    call strcpy (yformat, Memc[tyformat], SZ_FNAME)

	# If the transformation can be represented by mwcs then compile the
	# appropriate transform. Other wise use the surface fitting code
	# to do the transformation.
	if (mw != NULL) {
	    if (forward)
	        ct = mw_sctran (mw, "logical", "world", 03B)
	    else
	        ct = mw_sctran (mw, "world", "logical", 03B)
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

	    if (xfield > nfields || yfield > nfields) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Not enough fields in file %s line %d\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }
		 
            offset = Memi[field_pos+xfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], xd, nsdig_x)
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad x value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }

            offset = Memi[field_pos+yfield-1]
	    nchars = li_get_numd (Memc[linebuf+offset-1], yd, nsdig_y)
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad y value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }

	    # Transform the coordinates.
	    if (! forward) {
		switch (tlngunits) {
		case SKY_RADIANS:
		    xd = RADTODEG(xd)
		case SKY_HOURS:
		    xd = 15.0d0 * xd
		default:
		    ;
		}
		switch (tlatunits) {
		case SKY_RADIANS:
		    yd = RADTODEG(yd)
		case SKY_HOURS:
		    yd = 15.0d0 * yd
		default:
		    ;
		}
	    }
	    if (sx2 != NULL || sy2 != NULL)
		call cc_do_transform (xd, yd, xtd, ytd, ct, sx1, sy1,
		    sx2, sy2, forward)
	    else
		call mw_c2trand (ct, xd, yd, xtd, ytd)
	    if (forward) {
	        switch (tlngunits) {
		case SKY_RADIANS:
		    xtd = DEGTORAD(xtd)
		case SKY_HOURS:
		    xtd = xtd / 15.0d0
		default:
		    ;
		}
		switch (tlatunits) {
		case SKY_RADIANS:
		    ytd = DEGTORAD(ytd)
		case SKY_HOURS:
		    ytd = ytd / 15.0d0
		default:
		    ;
		}
	    }
		 
	    # Format the output line.
	    call li_pack_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
	        Memi[field_pos], nfields, xfield, yfield, xtd, ytd,
	        Memc[txformat], Memc[tyformat], nsdig_x, nsdig_y, min_sigdigits)

	    call putline (outfd, Memc[outbuf])
	}

	if (ct != NULL)
	    call mw_ctfree (ct)

	call sfree (sp)
end

