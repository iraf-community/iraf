include <mach.h>
include	<imhdr.h>
include	<fset.h>
include	"vt.h"

define	MAX_RANGES	100
define	VT_TBUF		15000

# READVT -- Read data from tape or disk and format the data into an IRAF image.
# Display header information to the user as a check if the 'verbose' flag is
# set. 

procedure t_readvt()

pointer	infile			# pointer to input filename(s)
pointer	outfile			# pointer to output filename(s)
bool	verbose			# verbose flag
bool	headeronly		# if set, just print the header
bool	robust			# if set, ignore wrong observation type
pointer	files			# file list for multiple tape files

int	listin			# list of input images
int	listout			# list of output images
bool	selfbuf, rootflag
int	nfiles, filenumber, stat
pointer	bp, sp, tapename, dfilename, diskfile, root
int	filerange[2 * MAX_RANGES + 1]

bool	clgetb()
int	get_next_number(), mtneedfileno()
int	strlen(), decode_ranges()
int	fntopnb(), imtopenp(), clgfil(), imtgetim(), clplen(), imtlen()
int	mtfile()
errchk	vt_rfd

begin
	call smark (sp)
	call salloc (infile, SZ_LINE, TY_CHAR)
	call salloc (outfile, SZ_LINE, TY_CHAR)
	call salloc (tapename, 2*SZ_LINE, TY_CHAR)
	call salloc (dfilename, 2*SZ_LINE, TY_CHAR)
	call salloc (diskfile, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_LINE, TY_CHAR)
	call salloc (files, SZ_LINE, TY_CHAR)

	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get parameters from the CL.
	verbose = clgetb ("verbose")
	headeronly = clgetb ("headeronly")
	robust = clgetb ("robust")

        call clgstr ("infile", Memc[infile], SZ_FNAME)

	# Set up the buffer structure, we may need it.
	call salloc (bp, VT_LENBSTRUCT, TY_STRUCT)
	call salloc (VT_BUFP(bp), VT_TBUF, TY_SHORT)
	VT_BP(bp) = VT_BUFP(bp)
	VT_BUFBOT(bp) = VT_BUFP(bp)

        if (mtfile (Memc[infile]) == NO) {
	    # This is not a tape file, expand as a list template.
	    listin = fntopnb (Memc[infile], 0)
	    rootflag = FALSE
	    filenumber = 1
	    if (!headeronly) {
	        listout = imtopenp ("outfile")

		# Compare the lengths of the two lists.  If equal, proceed,
		# otherwise if the outlist is of length one, use it as a root
		# name, otherwise error.
		
		if (imtlen (listout) == 1) {
		    rootflag = TRUE
		    stat = imtgetim (listout, Memc[root], SZ_FNAME)
		} else if (clplen (listin) != imtlen (listout)) {
		    call clpcls (listin)
	    	    call imtclose (listout)
	    	    call error (1, "Wrong number of elements in operand lists")
		}
	    }

	    while (clgfil (listin, Memc[diskfile], SZ_FNAME) != EOF) {
	        if (!headeronly) {
		    if (!rootflag)
	                stat = imtgetim (listout, Memc[dfilename], SZ_FNAME)
		    else {
			# Assemble an output filename from the root name.
			call sprintf (Memc[dfilename], SZ_FNAME, "%s")
			    call pargstr (Memc[root])
	                call sprintf (Memc[dfilename+strlen(Memc[root])],
		            SZ_FNAME, "%03d")
		            call pargi (filenumber)
			filenumber = filenumber + 1
		    }
		}

		# Of course, if the user is reading from disk, we can't
		# check record sizes.

		selfbuf = false
		iferr (call vt_rfd (diskfile, dfilename,
			selfbuf, verbose, headeronly, robust, bp)) {
		    call eprintf ("Error reading file %s\n")
		    	call pargstr (Memc[infile])
	    	}
	    }
	    call clpcls (listin)
	    if (!headeronly)
	        call imtclose (listout)

	} else if (mtneedfileno(Memc[infile]) == NO) {

	    # This is a tape file and the user specified which file.
	    if (!headeronly)
	        call clgstr ("outfile", Memc[outfile], SZ_FNAME)
	    selfbuf = true
	    iferr (call vt_rfd (infile, outfile, selfbuf, verbose,
		headeronly, robust, bp)) {
		call eprintf ("Error reading file %s\n")
		    call pargstr (Memc[infile])
	    }

        } else {

	    # This is a tape file and the user did not specify which file.
	    call clgstr ("files", Memc[files], SZ_LINE)
	    if (!headeronly)
	        call clgstr ("outfile", Memc[outfile], SZ_FNAME)

	    # Set up the file names, then do the read.
            if (decode_ranges (Memc[files], filerange, MAX_RANGES,
		nfiles) == ERR)
	        call error (0, "Illegal file number list.")

            while (get_next_number (filerange, filenumber) != EOF) {
	        # Assemble the appropriate tape file name.
		call mtfname (Memc[infile], filenumber, Memc[tapename],
		    SZ_FNAME)

	        # Assemble the appropriate disk file name.
	        if (!headeronly) {
	            call strcpy (Memc[outfile], Memc[dfilename], SZ_FNAME)
	            call sprintf (Memc[dfilename+strlen(Memc[outfile])],
		        SZ_FNAME, "%03d")
		        call pargi (filenumber)
		}

	        selfbuf = TRUE
	        iferr (call vt_rfd (tapename, dfilename, selfbuf,
			verbose, headeronly, robust, bp)) {
		    call eprintf ("Error reading file %s\n")
		        call pargstr (Memc[infile])
	        }
	    }
	}

	call sfree (sp)
end


# VT_RFD -- Do the actual read of a full disk gram.

procedure vt_rfd (in, out, selfbuf, verbose, headeronly, robust, bp)

pointer	in		# input file
pointer	out		# output file
bool	selfbuf		# do input buffering and correct for bad record lengths
bool	verbose		# verbose flag
bool	headeronly	# if set, just print the header
bool	robust		# if set, ignore wrong observation type

short	one
int	date, numchars
int	subraster, x1, y1, inputfd
pointer	table, bp, im, srp, hs, sp, hbuf
pointer	immap(), imps2s()
int	mtopen(), readheader()
errchk	readheader, loadsubswath, immap, imps2s
define	exit_	10

begin
	call smark (sp)
	call salloc (hbuf, SZ_VTHDR, TY_SHORT)
	call salloc (table, SZ_TABLE, TY_SHORT)
	call salloc (hs, VT_LENHSTRUCT, TY_STRUCT)

	if (verbose) {
	    call printf ("\nfile %s  ")
		call pargstr (Memc[in])
	}

	# Open input file.
	inputfd = mtopen (Memc[in], READ_ONLY, 0)

	# Read header.
	iferr (numchars = readheader (inputfd, hbuf, selfbuf))
	    call error (0, "Error reading header information.")
	call decodeheader (hbuf, hs, verbose)
	if (verbose)
	    call printf ("\n")

	# Check the observation type in the header.  If this value is not
	# zero (full disk) then write an error message, if the robust flag
	# is set go ahead and read the file.

	if (!robust) {
	    if (VT_HOBSTYPE[hs] != 0) {
	        call printf ("file %s is not a type zero scan (full disk)\n")
		    call pargstr (Memc[in])
	        call printf ("Use 'mscan' to read this type %d area scan\n")
		    call pargi (VT_HOBSTYPE[hs])
	        goto exit_      # close input file and exit
	    }
	} else {
	    if (VT_HOBSTYPE[hs] != 0) {
		call printf ("The header for file %s contains 'observation ")
		    call pargstr (Memc[in])
		call printf ("type = %d'\n")
		    call pargi (VT_HOBSTYPE[hs])
		call printf ("READVT expects the observation type ")
		call printf ("to be zero.\n")
		call printf ("This error will be ignored since the 'robust'")
		call printf (" flag is set\n")
	    }
	}

	if (headeronly)
	    goto exit_      # close input file and exit

	if (verbose) {
	    call printf ("\nwriting %s\n")
		call pargstr (Memc[out])
	}

	# Open the output image. Set it up.
	im  = immap (Memc[out], NEW_IMAGE, 0)
	IM_NDIM(im) = 2
	IM_LEN(im,1) = DIM_VTFD
	IM_LEN(im,2) = DIM_VTFD
	IM_PIXTYPE(im) = TY_SHORT

	# Set up the 8 header fields we need and store the information we
	# obtained from the raw data image header.

	call imaddi (im, "obs_time", VT_HTIME[hs])
	date = VT_HMONTH[hs] * 10000 + VT_HDAY[hs] * 100 + VT_HYEAR[hs]

	call imaddi (im, "obs_date", date )
	call imaddi (im, "wv_lngth", VT_HWVLNGTH[hs])
	call imaddi (im, "obs_type", VT_HOBSTYPE[hs])
	call imaddi (im, "av_intns", VT_HAVINTENS[hs])
	call imaddi (im, "num_cols", VT_HNUMCOLS[hs])
	call imaddi (im, "intg/pix", VT_HINTGPIX[hs])
	call imaddi (im, "rep_time", VT_HREPTIME[hs])

	# Set up lookuptable.
	one = 1
	call amovks (one, Mems[table], SZ_TABLE)
	call aclrs (Mems[table], HALF_DIF)
	call aclrs (Mems[table + SWTHWID_14 + HALF_DIF], HALF_DIF)
	call aclrs (Mems[table + SWTHWID_23 * 3], HALF_DIF)
	call aclrs (Mems[table + SZ_TABLE - HALF_DIF], HALF_DIF)

	# Now, fill the image with data.
	do subraster = 1, NUM_SRSTR {

	    # Calculate position of bottom left corner of this subraster
	    x1 = ((NUM_SRSTR_X - 1) - mod((subraster - 1), NUM_SRSTR_X)) *
		SRSTR_WID + 1
	    y1 = ((NUM_SRSTR_Y - 1) - ((subraster - mod((subraster - 1),
		NUM_SRSTR_Y)) / NUM_SRSTR_Y)) * SWTH_HIGH + 1

	    # Get subraster.
	    srp = imps2s (im, x1, x1+(SRSTR_WID - 1), y1, y1+(SWTH_HIGH - 1))

	    # Load the subraster with data.
	    iferr (call loadsubraster (inputfd, Mems[srp], SRSTR_WID, SWTH_HIGH,
		Mems[table], subraster, selfbuf, bp)) {
		call eprintf ("Error in loadsubraster, subraster = %d\n")
		    call pargi (subraster)
		break
	    }

	    if (verbose) {
		call printf("%d%% ")
		    call pargi ((subraster*100)/NUM_SRSTR)
		call flush (STDOUT)
	    }
	}

	if (verbose)
	    call printf ("\n")

	# Unmap image and close input file.
	call imunmap (im)
exit_
	call sfree (sp)
	call close (inputfd)
end


# LOADSUBRASTER -- Get data from the input and load it into this
# subraster, look in the table to see if each subswath should be
# filled with data or zeros.

procedure loadsubraster (inputfd, array, nx, ny, table, subraster, selfbuf, bp)

int	inputfd			# input file we are reading from
short	array[nx, ny]		# array to put the data in
int	nx			# x length of the array
int	ny			# y length of the array
short	table[SZ_TABLE]		# lookup table for data
int	subraster		# subraster number are we loading
bool	selfbuf			# buffering and record length checking?
pointer	bp			# pointer to buffer pointer structure

pointer	sp, bufpointer
int	i, subswath, tableindex
errchk	readsubswath

begin
	call smark (sp)
	call salloc (bufpointer, ny, TY_SHORT)

	for (subswath = nx;  subswath >= 1;  subswath = subswath - 1) {
	    tableindex = (subraster - 1) * nx + ((nx + 1) - subswath)

	    if (table[tableindex] == IS_DATA) {
		iferr (call readsubswath (inputfd, selfbuf, Mems[bufpointer],
			ny, bp)) {

		    call eprintf ("Error in readsubswath, subswath = %d\n")
			call pargi (subswath)
		}

		do i = ny, 1, -1
		    array[subswath,i] = Mems[bufpointer + ny - i]

	    } else {
		do i = 1, ny 
		    array[subswath,i] = 0
	    }
	}

	call sfree (sp)
end
