include <fset.h>
include	<ctype.h>
include	<math/gsurfit.h>

define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops

# Define the permitted computation types
define	GEO_REAL	1		# Computation type is real
define	GEO_DOUBLE	2		# Computation type is double

# T_GEOXYTRAN -- Transform a list of x and y coordinates using the geometric
# transformation operations computed by the GEOMAP task.

procedure t_geoxytran()

int	inlist, outlist, reclist, calctype, geometry, dir, xcolumn, ycolumn
int	min_sigdigits, infd, outfd
pointer	sp, in_fname, out_fname, record, xformat, yformat, str, dt
pointer	sx1, sy1, sx2, sy2
int	clgwrd(), clgeti(), open()
bool	streq()
int	fntopnb(), fntlenb(), fntgfnb(), imtopenp(), imtlen(), imtgetim()
pointer	dtmap()

begin
	# Allocate memory for transformation parameters structure
	call smark (sp)
	call salloc (in_fname, SZ_FNAME, TY_CHAR)
	call salloc (out_fname, SZ_FNAME, TY_CHAR)
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
	    reclist = imtopenp ("transforms")
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

	# Get geometry and transformation direction.
	geometry = clgwrd ("geometry", Memc[str], SZ_LINE,
	    ",linear,distortion,geometric,")
	dir = clgwrd ("direction", Memc[str], SZ_LINE,
	    ",forward,backward,")

	# Get field numbers from cl
	if (dir == 1)
	    calctype = clgwrd ("calctype", Memc[str], SZ_LINE,
		",real,double,")
	else
	    calctype = GEO_DOUBLE
	xcolumn = clgeti ("xcolumn")
	ycolumn = clgeti ("ycolumn")
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)
	min_sigdigits = clgeti ("min_sigdigits")

	# Get the output file name.
	if (fntgfnb (outlist, Memc[out_fname], SZ_FNAME) == EOF)
	    call strcpy ("STDOUT", Memc[out_fname], SZ_FNAME)
	outfd = open (Memc[out_fname], NEW_FILE, TEXT_FILE)
	if (streq (Memc[out_fname], "STDOUT") || outfd == STDOUT)
	    call fseti (outfd, F_FLUSHNL, YES)

	# Get the record name.
	if (reclist == NULL)
	    Memc[record] = EOS
	else if (imtgetim (reclist, Memc[record], SZ_FNAME) == EOF)
	    Memc[record] = EOS

	# Call procedure to get parameters and fill structure.
	sx1 = NULL; sy1 = NULL; sx2 = NULL; sy2 = NULL
	call geo_init_transform (dt, Memc[record], calctype, geometry,
	    sx1, sy1, sx2, sy2)

	# While input list is not depleted, open file and transform list.
	while (fntgfnb (inlist, Memc[in_fname], SZ_FNAME) != EOF)  {

	    infd = open (Memc[in_fname], READ_ONLY, TEXT_FILE)

	    # Transform the coordinates.
	    call geo_transform_file (infd, outfd, xcolumn, ycolumn, dir,
	        calctype, Memc[xformat], Memc[yformat], min_sigdigits,
		sx1, sy1, sx2, sy2)

	    # Do not get a new output file name if there is not output 
	    # file list or if only one output file was specified.
	    # Otherwise fetch the new name.
	    if (fntlenb(outlist) > 1) {
		call close (outfd)
	        if (fntgfnb (outlist, Memc[out_fname], SZ_FNAME) != EOF)
		    outfd = open (Memc[out_fname], NEW_FILE, TEXT_FILE) 
		if (streq (Memc[out_fname], "STDOUT") || outfd == STDOUT)
	    	    call fseti (outfd, F_FLUSHNL, YES)
	    }

	    call close (infd)

	    # Do not reset the transformation if there is no record list
	    # or only one record is specified. Otherwise fetch the next
	    # record name.
	    if (reclist != NULL && imtlen (reclist) > 1) {
	        if (imtgetim (reclist, Memc[record], SZ_FNAME) != EOF) {
		    call geo_free_transform (calctype, sx1, sy1, sx2, sy2)
	            call geo_init_transform (dt, Memc[record], calctype,
			geometry, sx1, sy1, sx2, sy2)
		}
	    }
	}

	# Free the surface descriptors.
	call geo_free_transform (calctype, sx1, sy1, sx2, sy2)

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


# GEO_INIT_TRANSFORM -- gets parameter values relevant to the
# transformation from the cl.  List entries will be transformed
# in procedure rg_transform.

procedure geo_init_transform (dt, record, calctype, geometry, sx1, sy1,
	sx2, sy2)

pointer	dt			#I pointer to database file produced by geomap
char	record[ARB]		#I the name of the database record
int	calctype		#I the computation data type
int	geometry		#I the type of geometry to be computed
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces

begin
	if (dt == NULL) {

	    if (calctype == GEO_REAL)
		call geo_linitr (sx1, sy1, sx2, sy2)
	    else
		call geo_linitd (sx1, sy1, sx2, sy2)

	} else {

	    if (calctype == GEO_REAL)
		call geo_sinitr (dt, record, geometry, sx1, sy1,
		    sx2, sy2)
	    else
		call geo_sinitd (dt, record, geometry, sx1, sy1,
		    sx2, sy2)
	}
end


# GEO_FREE_TRANSFORM -- Free the previously defined transformation

procedure geo_free_transform (calctype, sx1, sy1, sx2, sy2)

int	calctype		#I the computation data type
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces

begin
	if (calctype == GEO_REAL)
	    call geo_sfreer (sx1, sy1, sx2, sy2)
	else
	    call geo_sfreed (sx1, sy1, sx2, sy2)
end


# GEO_TRANSFORM_FILE -- This procedure is called once for each file
# in the input list.  For each line in the input file that isn't
# blank or comment, the line is transformed.  Blank and comment
# lines are output unaltered.

procedure geo_transform_file (infd, outfd, xfield, yfield, dir, calctype,
	xformat, yformat, min_sigdigits, sx1, sy1, sx2, sy2)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
int	xfield			#I the x column number
int	yfield			#I the y column number
int	dir			#I transform direction
int	calctype		#I the computation type
char	xformat[ARB]		#I output format of the x coordinate
char	yformat[ARB]		#I output format of the y coordinate
int	min_sigdigits		#I the minimum number of digits to be output
pointer	sx1, sy1		#I pointers to the linear x and y surfaces
pointer	sx2, sy2		#I pointers to the x and y distortion surfaces

double	xd, yd, xtd, ytd
int	max_fields, nline, nfields, nchars, nsdig_x, nsdig_y, offset
real	xr, yr, xtr, ytr
pointer	sp, inbuf, linebuf, field_pos, outbuf, ip
int	getline(), li_get_numr(), li_get_numd()

int	nsx, nsy
double	der[8], xmin, xmax, ymin, ymax, tol
pointer	sx[2], sy[2]
double	dgsgetd()

#double	x, y, xt, yt

begin
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)
	call salloc (outbuf, SZ_LINE, TY_CHAR)

	max_fields = MAX_FIELDS

	# Initialize for backward transform.
	if (dir == 2) {
	    sx[1] = sx1; sy[1] = sy1; sx[2] = sx2; sy[2] = sy2
	    nsx = 2; nsy = 2
	    if (sx2 == NULL)
		nsx = 1
	    if (sy2 == NULL)
		nsy = 1
	    xmin = dgsgetd (sx1, GSXMIN)
	    xmax = dgsgetd (sx1, GSXMAX)
	    ymin = dgsgetd (sx1, GSYMIN)
	    ymax = dgsgetd (sx1, GSYMAX)
	    tol = abs (xmax - xmin) / 1E10
	    xd = (xmin + xmax) / 2
	    yd = (ymin + ymax) / 2
	    call tr_init (sx, nsx, sy, nsy, xd, yd, der)
	}

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
	    if (calctype == GEO_REAL)
	        nchars = li_get_numr (Memc[linebuf+offset-1], xr, nsdig_x)
	    else
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
	    if (calctype == GEO_REAL)
	        nchars = li_get_numr (Memc[linebuf+offset-1], yr, nsdig_y)
	    else
	        nchars = li_get_numd (Memc[linebuf+offset-1], yd, nsdig_y)
	    if (nchars == 0) {
		call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
		call eprintf ("Bad y value in file '%s' at line %d:\n")
		    call pargstr (Memc[outbuf])
		    call pargi (nline)
		call putline (outfd, Memc[linebuf])
		next
	    }
		 
	    if (calctype == GEO_REAL) {
		call geo_do_transformr (xr, yr, xtr, ytr,
		    sx1, sy1, sx2, sy2)
	        call li_pack_liner (Memc[linebuf], Memc[outbuf], SZ_LINE,
	            Memi[field_pos], nfields, xfield, yfield, xtr, ytr,
		    xformat, yformat, nsdig_x, nsdig_y, min_sigdigits)

	    } else {
		if (dir == 1)
		    call geo_do_transformd (xd, yd, xtd, ytd,
		        sx1, sy1, sx2, sy2)
		else
		    call tr_invert (sx, nsx, sy, nsy, xd, yd, xtd, ytd,
		        der, xmin, xmax, ymin, ymax, tol)
	        call li_pack_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
	            Memi[field_pos], nfields, xfield, yfield, xtd, ytd,
		    xformat, yformat, nsdig_x, nsdig_y, min_sigdigits)
	    }

	    call putline (outfd, Memc[outbuf])
	}

	call sfree (sp)
end

