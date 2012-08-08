# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pattern.h>
include	<ctype.h>

define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops
define	LEN_TR		9		# Length of structure TR

# The TR transformation descriptor structure.

define	X1		Memr[P2R($1)]	# Input origin
define	Y1		Memr[P2R($1+1)]
define	XSCALE		Memr[P2R($1+2)]	# Scale factors
define	YSCALE		Memr[P2R($1+3)]
define	THETA		Memr[P2R($1+4)]	# Rotation angle
define	X2		Memr[P2R($1+5)]	# Output origin
define	Y2		Memr[P2R($1+6)]
define  COS_THETA	Memr[P2R($1+7)]
define	SIN_THETA	Memr[P2R($1+8)]


# LINTRAN -- Performs a linear translation on each element of the
# input list, producing a transformed list as output.

procedure t_lintran()

char	in_fname[SZ_FNAME]
int	list
pointer	sp, tr
int	xfield, yfield, min_sigdigits

int 	clgeti(), clpopni(), clgfil()

begin
	# Allocate memory for transformation parameters structure
	call smark (sp)
	call salloc (tr, LEN_TR, TY_STRUCT)

	# Call procedure to get parameters and fill structure
	call lt_initialize_transform (tr)

	# Get field numbers from cl
	xfield = clgeti ("xfield")
	yfield = clgeti ("yfield")
	min_sigdigits = clgeti("min_sigdigits")

	# Open template of input files
	list = clpopni ("files")

	# While input list is not depleted, open file and transform list
	while (clgfil (list, in_fname, SZ_FNAME) != EOF) 
	    call lt_transform_file (in_fname, xfield, yfield, min_sigdigits, tr)

	# Close template
	call clpcls (list)
	call sfree (sp)
end


# LT_INITIALIZE_TRANSFORM -- gets parameter values relevant to the
# transformation from the cl.  List entries will be transformed
# in procedure lt_transform.  Scaling is performed
# first, followed by translation and then rotation.

procedure lt_initialize_transform (tr)

pointer	tr

bool	clgetb()
real	clgetr()

begin
	# Get parameters from cl
	X1(tr) = clgetr ("x1")				# (x1,y1) = crnt origin
	Y1(tr) = clgetr ("y1")
	XSCALE(tr) = clgetr ("xscale")
	YSCALE(tr) = clgetr ("yscale")
	THETA(tr) = clgetr ("angle")
	if (! clgetb ("radians"))
	    THETA(tr) = THETA(tr) / 57.29577951 
	X2(tr) = clgetr ("x2")				# (x2,y2) = new origin
	Y2(tr) = clgetr ("y2")

	# The following terms are constant for a given transformation.
	# They are calculated once and saved in the structure.

	COS_THETA(tr) = cos (THETA(tr))
	SIN_THETA(tr) = sin (THETA(tr))
end


# LT_TRANSFORM_FILE -- This procedure is called once for each file
# in the input list.  For each line in the input file that isn't
# blank or comment, the line is transformed.  Blank and comment
# lines are output unaltered.

procedure lt_transform_file (in_fname, xfield, yfield, min_sigdigits, tr)

char	in_fname[ARB]
int	xfield, yfield
pointer	tr

char	outbuf[SZ_LINE]
int	nfields, nchars, max_fields, in, nline
int	nsdig_x, nsdig_y, offset, min_sigdigits
pointer	sp, field_pos, linebuf, inbuf, ip
double	x, y, xt, yt
int	getline(), lt_get_num(), open()

begin
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)

	max_fields = MAX_FIELDS

	# Open input file
	in = open (in_fname, READ_ONLY, TEXT_FILE)

	for (nline=1;  getline (in, Memc[inbuf]) != EOF;  nline = nline + 1) {
	    for (ip=inbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '#') {
		# Pass comment lines on to the output unchanged.
		call putline (STDOUT, Memc[inbuf])
		next
	    } else if (Memc[ip] == '\n' || Memc[ip] == EOS) {
		# Blank lines too.
		call putline (STDOUT, Memc[inbuf])
		next
	    }

	    # Expand tabs into blanks, determine field offsets.
	    call strdetab (Memc[inbuf], Memc[linebuf], SZ_LINE, TABSIZE)
	    call lt_find_fields (Memc[linebuf], Memi[field_pos],
		max_fields, nfields)

	    if (xfield > nfields || yfield > nfields) {
		call eprintf ("Not enough fields in file '%s', line %d\n")
		    call pargstr (in_fname)
		    call pargi (nline)
		call putline (STDOUT, Memc[linebuf])
		next
	    }
		 
            offset = Memi[field_pos + xfield-1]
	    nchars = lt_get_num (Memc[linebuf+offset-1], x, nsdig_x)
	    if (nchars == 0) {
		call eprintf ("Bad x value in file '%s' at line %d:\n")
		    call pargstr (in_fname)
		    call pargi (nline)
		call putline (STDOUT, Memc[linebuf])
		next
	    }

            offset = Memi[field_pos + yfield-1]
	    nchars = lt_get_num (Memc[linebuf+offset-1], y, nsdig_y)
	    if (nchars == 0) {
		call eprintf ("Bad y value in file '%s' at line %d:\n")
		    call pargstr (in_fname)
		    call pargi (nline)
		call putline (STDOUT, Memc[linebuf])
		next
	    }
		 
	    call lt_transform (x, y, xt, yt, tr)

	    call lt_pack_line (Memc[linebuf], outbuf, SZ_LINE, Memi[field_pos], 
	      nfields, xfield, yfield, xt, yt, nsdig_x, nsdig_y, min_sigdigits)

	    call putline (STDOUT, outbuf)
	}

	call sfree (sp)
	call close (in)
end


# LT_FIND_FIELDS -- This procedure finds the starting column for each field
# in the input line.  These column numbers are returned in the array
# field_pos; the number of fields is also returned.

procedure lt_find_fields (linebuf, field_pos, max_fields, nfields)

char	linebuf[SZ_LINE]
int	field_pos[max_fields],max_fields, nfields
bool	in_field
int	ip, field_num

begin
	field_num = 1
	field_pos[1] = 1
	in_field = false

	for (ip=1; linebuf[ip] != '\n' && linebuf[ip] != EOS; ip=ip+1) {
	    if (! IS_WHITE(linebuf[ip]))
		in_field = true
	    else if (in_field) {
		in_field = false
		field_num = field_num + 1
		field_pos[field_num] = ip
	    }
	}

	field_pos[field_num+1] = ip 
	nfields = field_num
end


# LT_GET_NUM -- The field entry is converted from character to double
# in preparation for the transformation.  The number of significant
# digits is counted and returned as an argument; the number of chars in
# the number is returned as the function value.

int procedure lt_get_num (linebuf, dval, nsdig) 

char	linebuf[SZ_LINE]
int	nsdig
double	dval
char	ch
int 	nchar, ip

int	gctod()

begin
	ip = 1
	nsdig = 0
	nchar = gctod (linebuf, ip, dval)
	if (nchar == 0 || IS_INDEFD (dval))
	    return (nchar)

	# Skip leading white space.
	ip = 1
    	repeat {
	    ch = linebuf[ip]
	    if (! IS_WHITE(ch)) 
		break
	    ip = ip + 1
	} 

	# Count signifigant digits
	for (; ! IS_WHITE(ch) && ch != '\n' && ch != EOS; ch=linebuf[ip]) {
	    if (IS_DIGIT (ch))
		nsdig = nsdig + 1
		ip = ip + 1
	}
	return (nchar)
end


# LT_TRANSFORM -- The linear transformation is performed in this procedure.
# First the coordinates are scaled, then rotated and translated.  The
# transformed coordinates are returned.

procedure lt_transform (x, y, xt, yt, tr)

double	x, y, xt, yt
pointer	tr
double	xtemp, ytemp

begin
	# Subtract off current origin:
	if (IS_INDEFD (x))
	    xt = INDEFD
	else {
	    xt = x - X1(tr)
	}
	if (IS_INDEFD (y))
	    yt = INDEFD
	else {
	    yt = y - Y1(tr)
	}

	# Scale and rotate coordinates:
	if (THETA(tr) == 0) {
	    if (!IS_INDEFD (xt))
		xt = xt * XSCALE(tr) + X2(tr)
	    if (!IS_INDEFD (yt))
		yt = yt * YSCALE(tr) + Y2(tr)
	    return

	} else if (IS_INDEFD(xt) || IS_INDEFD(yt)) {
	    # Non-zero angle and either coordinate indefinite results in
	    # both transformed coordinates = INDEFD
	    xt = INDEFD
	    yt = INDEFD
	    return
	}

	# Rotation for non-zero angle and both coordinates defined
	xtemp = xt * XSCALE(tr)
	ytemp = yt * YSCALE(tr)

	xt = xtemp * COS_THETA(tr) - ytemp * SIN_THETA(tr) 
	yt = xtemp * SIN_THETA(tr) + ytemp * COS_THETA(tr)

	# Now shift the rotated coordinates
	xt = xt + X2(tr)
	yt = yt + Y2(tr)
end


# LT_PACK_LINE -- Fields are packed into the outbuf buffer.  Transformed
# fields are converted to strings; other fields are copied from
# the input line to output buffer.

procedure lt_pack_line (inbuf, outbuf, maxch, field_pos, nfields, 
		xfield, yfield, xt, yt, nsdig_x, nsdig_y, min_sigdigits)

char	inbuf[ARB], outbuf[maxch]
int	maxch, field_pos[ARB], nfields, xfield, yfield, nsdig_x, nsdig_y
int	min_sigdigits
double	xt, yt

char	field[SZ_LINE]
int	num_field, width, op

int	gstrcpy()

begin
	# Initialize output pointer.
	op = 1

	do num_field = 1, nfields {
	    width = field_pos[num_field + 1] - field_pos[num_field]

	    if (num_field == xfield) {
	        call lt_format_field (xt, field, maxch, nsdig_x, width, 
				      min_sigdigits)
	    } else if (num_field == yfield) {
		call lt_format_field (yt, field, maxch, nsdig_y, width,
				      min_sigdigits)
	    } else {
	        # Put "width" characters from inbuf into field
		call strcpy (inbuf[field_pos[num_field]], field, width)
	    }

	    # Fields must be delimited by at least one blank.
	    if (num_field > 1 && !IS_WHITE (field[1])) {
		outbuf[op] = ' '
		op = op + 1
	    }

	    # Copy "field" to output buffer.
	    op = op + gstrcpy (field, outbuf[op], maxch)
	}

	outbuf[op] = '\n'
	outbuf[op+1] = EOS
end


# LT_FORMAT_FIELD -- A transformed coordinate is written into a string
# buffer.  The output field is of (at least) the same width and significance
# as the input list entry.

procedure lt_format_field (dval, wordbuf, maxch, nsdig, width, min_sigdigits)

char	wordbuf[maxch]
int	width, nsdig, maxch, min_sigdigits
double	dval

begin
	call sprintf (wordbuf, maxch, "%*.*g")
	    call pargi (width)
	    call pargi (max (min_sigdigits, nsdig))
	    call pargd (dval)
end
