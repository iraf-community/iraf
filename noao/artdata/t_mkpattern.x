include	<error.h>
include	<imhdr.h>

define	LEN_UA		20000

# Editing  options
define	OPTIONS		"|replace|add|multiply|"
define	REPLACE		1	# Replace pixels
define	ADD		2	# Add to pixels
define	MULTIPLY	3	# Multiply pixels

# Patterns
define	PATTERNS	"|constant|grid|checker|coordinates|slope|square"
define	CONST		1	# Constant = v1
define	GRID		2	# Grid lines of v2 with given spacing
define	CHECK		3	# Checkboard of given size
define	COORD		4	# Coordinates
define	SLOPE		5	# Slope
define	SQUARE		6	# Square root checkerboard

# T_MKPATTERN -- Create or modify images using simple patterns.
# Images may be created of a specified size, dimensionality, and pixel
# datatype.  The images may be modified to replace, add, or multiply
# by specified values.  The patterns include a constant value,
# a grid, a checkerboard or fixed size or increasing size, the
# 1D pixel coordinate, and a slope.  For dimensions greater than
# 2 the 2D pattern is repeated.

procedure t_mkpattern ()


int	ilist				# Input image list
int	olist				# Output image list
int	op				# Operation option
int	pat				# Pattern
real	v1				# Pattern value 1
real	v2				# Pattern value 2
int	size				# Pattern size
int	nl				# Number of lines
int	nc				# Number of columns

bool	new
int	i
long	vin[IM_MAXDIM], vout[IM_MAXDIM]
pointer	sp, input, output, header, in, out, indata, outdata, pat1, pat2

char	clgetc()
bool	streq()
int	clgwrd(), clgeti()
int	imtopenp(), imtlen(), imtgetim(), imgnlr(), impnlr()
real	clgetr()
pointer	immap()
errchk	immap

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (header, SZ_FNAME, TY_CHAR)

	# Set the task parameters which apply to all images.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	pat = clgwrd ("pattern", Memc[input], SZ_FNAME, PATTERNS)
	op = clgwrd ("option", Memc[input], SZ_FNAME, OPTIONS)
	v1 = clgetr ("v1")
	v2 = clgetr ("v2")
	size = max (1, clgeti ("size"))

	if (max (1, imtlen (olist)) != imtlen (ilist))
	    call error (1, "Output image list does not match input image list")

	# Loop over the input image lists.  If no output list is given
	# then create or modify the input image.

	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF)
	        call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    # Map images.  Check for new, existing, and inplace images.
	    if (streq (Memc[input], Memc[output])) {
		ifnoerr (out = immap (Memc[output], READ_WRITE, 0)) {
		    in = out
	            new = false
	        } else {
		    iferr (out = immap (Memc[output], NEW_IMAGE, LEN_UA)) {
			call erract (EA_WARN)
			next
		    }

		    call clgstr ("header", Memc[header], SZ_FNAME)
		    iferr (call mkh_header (out, Memc[header], false, false))
			call erract (EA_WARN)

	            IM_NDIM(out) = clgeti ("ndim")
		    IM_LEN(out,1) = clgeti ("ncols")
		    IM_LEN(out,2) = clgeti ("nlines")
		    IM_LEN(out,3) = clgeti ("n3")
		    IM_LEN(out,4) = clgeti ("n4")
		    IM_LEN(out,5) = clgeti ("n5")
		    IM_LEN(out,6) = clgeti ("n6")
		    IM_LEN(out,7) = clgeti ("n7")
		    switch (clgetc ("pixtype")) {
		    case 'u':
		        IM_PIXTYPE(out) = TY_USHORT
		    case 's':
		        IM_PIXTYPE(out) = TY_SHORT
		    case 'i':
		        IM_PIXTYPE(out) = TY_INT
		    case 'l':
		        IM_PIXTYPE(out) = TY_LONG
		    case 'r':
		        IM_PIXTYPE(out) = TY_REAL
		    case 'd':
		        IM_PIXTYPE(out) = TY_DOUBLE
		    case 'c':
		        IM_PIXTYPE(out) = TY_COMPLEX
		    default:
			call error (0, "Bad pixel type")
		    }
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)
		    in = out
	            new = true
	        }
	    } else {
	        iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		}
	        iferr (out = immap (Memc[output], NEW_COPY, in)) {
		    call erract (EA_WARN)
		    call imunmap (in)
		    next
		}
	        new = false
	    }
	    nc = IM_LEN(out,1)
	    nl = IM_LEN(out,2)

	    call amovkl (long (1), vin, IM_MAXDIM)
	    call amovkl (long (1), vout, IM_MAXDIM)

	    # Initialize the pattern; two pointers are returned.
	    call mkpatinit (pat, size, v1, v2, pat1, pat2, nc, nl)

	    # Create or modify the image with the specified pattern.
	    # A new image is always the same as replace.

	    if (new) {
	        while (impnlr (out, outdata, vout) != EOF)
		    call mkpattern (pat, size, v1, v2, pat1, pat2,
			vout[2]-1, outdata, nc, nl)
	    } else {
	        switch (op) {
	        case REPLACE:
	            while (impnlr (out, outdata, vout) != EOF)
		        call mkpattern (pat, size, v1, v2, pat1, pat2,
			    vout[2]-1, outdata, nc, nl)
	        case ADD:
	            while (impnlr (out, outdata, vout) != EOF) {
		        i = imgnlr (in, indata, vin)
		        call mkpattern (pat, size, v1, v2, pat1, pat2,
			    vout[2]-1, outdata, nc, nl)
		        call aaddr (Memr[indata], Memr[outdata], Memr[outdata],
			    nc)
	            }
	        case MULTIPLY:
	            while (impnlr (out, outdata, vout) != EOF) {
		        i = imgnlr (in, indata, vin)
		        call mkpattern (pat, size, v1, v2, pat1, pat2,
			    vout[2]-1, outdata, nc, nl)
		        call amulr (Memr[indata], Memr[outdata], Memr[outdata],
			    nc)
	            }
	        }
	    }

	    call mkpatfree (pat1, pat2)
	    if (in != out)
	        call imunmap (in)
	    call imunmap (out)
	}

	call imtclose (ilist)
	call imtclose (olist)
	call sfree (sp)
end


# MKPATINIT -- Initialize the pattern.  For speed one or two lines of the
# pattern are created and then used over the image by simple array
# operations.

procedure mkpatinit (pat, size, v1, v2, pat1, pat2, nc, nl)

int	pat		# Pattern
int	size		# Pattern size
real	v1		# Value 1 for pattern
real	v2		# Value 2 for pattern
pointer	pat1		# Pattern 1 buffer
pointer	pat2		# Pattern 2 buffer
int	nc		# Number of columns
int	nl		# Number of lines

int	i

begin
	pat1 = NULL
	pat2 = NULL

	switch (pat) {
	case CONST:
	    call malloc (pat1, nc, TY_REAL)
	    call amovkr (v1, Memr[pat1], nc)
	case GRID:
	    call malloc (pat1, nc, TY_REAL)
	    call malloc (pat2, nc, TY_REAL)
	    call amovkr (v1, Memr[pat1], nc)
	    call amovkr (v2, Memr[pat2], nc)
	    size = max (size, 2)
	    do i = 1, nc-1, size
		Memr[pat1+i] = v2
	case CHECK:
	    call malloc (pat1, nc, TY_REAL)
	    call malloc (pat2, nc, TY_REAL)
	    do i = 0, nc-1 {
		if (mod (i/size, 2) == 0) {
		    Memr[pat1+i] = v1
		    Memr[pat2+i] = v2
		} else {
		    Memr[pat1+i] = v2
		    Memr[pat2+i] = v1
		}
	    }
	case COORD:
	    call malloc (pat1, nc, TY_REAL)
	    do i = 0, nc-1
		Memr[pat1+i] = i / size + 1
	case SLOPE:
	    call malloc (pat1, nc, TY_REAL)
	    call malloc (pat2, 1, TY_REAL)
	    Memr[pat2] = (v2 - v1) / ((nc + nl - 2) / size)
	    do i = 0, nc - 1
		Memr[pat1+i] = v1 + Memr[pat2] * i / size
	case SQUARE:
	    call malloc (pat1, nc, TY_REAL)
	    call malloc (pat2, nc, TY_REAL)
	    do i = 0, nc-1 {
		if (mod (int (sqrt (real (i/size))), 2) == 0) {
		    Memr[pat1+i] = v1
		    Memr[pat2+i] = v2
		} else {
		    Memr[pat1+i] = v2
		    Memr[pat2+i] = v1
		}
	    }
	}
end


# MKPATFREE -- Free memory used in the pattern buffers.

procedure mkpatfree (pat1, pat2)

pointer	pat1, pat2	# Pattern buffers

begin
	call mfree (pat1, TY_REAL)
	call mfree (pat2, TY_REAL)
end


# MKPATTERN -- Make a line of data.

procedure mkpattern (pat, size, v1, v2, pat1, pat2, line, data, nc, nl)

int	pat		# Pattern
int	size		# Pattern size
real	v1		# Pattern value
real	v2		# Pattern value
pointer	pat1		# Pattern 1
pointer	pat2		# Pattern 2
int	line		# Line
pointer	data		# Data
int	nc		# Number of columns
int	nl		# Number of lines

int	i

begin
	i = max (0, line-1) / size

	switch (pat) {
	case CONST:
	    call amovr (Memr[pat1], Memr[data], nc)
	case GRID:
	    if (mod (line, size) == 1)
	        call amovr (Memr[pat2], Memr[data], nc)
	    else
	        call amovr (Memr[pat1], Memr[data], nc)
	case CHECK:
	    if (mod (i, 2) == 0)
	        call amovr (Memr[pat1], Memr[data], nc)
	    else
	        call amovr (Memr[pat2], Memr[data], nc)
	case COORD:
	    call amovr (Memr[pat1], Memr[data], nc)
	    call aaddkr (Memr[data], real(i*nc/size), Memr[data], nc)
	case SLOPE:
	    call amovr (Memr[pat1], Memr[data], nc)
	    call aaddkr (Memr[data], i*Memr[pat2], Memr[data], nc)
	case SQUARE:
	    if (mod (int (sqrt (real (i))), 2) == 0)
	        call amovr (Memr[pat1], Memr[data], nc)
	    else
	        call amovr (Memr[pat2], Memr[data], nc)
	}
end
