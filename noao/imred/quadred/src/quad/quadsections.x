include <imhdr.h>
include "quadgeom.h"

# QUADSECTIONS.X -- File header comment.

define	OPTION_DICT	"|biassec|datasec|trimsec|reflect|duplicate|"

define	OPT_BIASSEC	1
define	OPT_DATASEC	2
define	OPT_TRIMSEC	3
define	OPT_REFLECT	4
define	OPT_DUPLICATE	5

define	DEFAULT_MACRO	"$I$S\\n"


# QUADSECTIONS -- Quadsections task.

procedure t_quadsections ()

pointer	inlist			#TI List of input image name.
char	instrument[SZ_FNAME]	#TI Instrument translation file
char	section[SZ_LINE]	#TI Section of CCD required
int	option			#TI Type of section required
char	format[SZ_LINE]		#TI strmac macro string for building output
int	xtrim1			#TI X pixels to trim at start of data
int	xtrim2			#TI X pixels to trim at end   of data
int	ytrim1			#TI Y pixels to trim at start of data
int	ytrim2			#TI Y pixels to trim at end   of data
int	xskip1			#TI X pixels to skip at start of overscan
int	xskip2			#TI X pixels to skip at end   of overscan
 
char	buffer[SZ_LINE], input[SZ_LINE]

int	clgwrd(), imtopenp(), imtgetim()

begin
	# Open input image
	inlist = imtopenp ("images")

	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Get option.
	option = clgwrd ("window", buffer, SZ_LINE, OPTION_DICT)

	# Get default section
	call clgstr ("section", section, SZ_LINE)

	# Get user defined trim and overscan margins
	#xtrim1 = clgeti ("xtrim1")
	#xtrim2 = clgeti ("xtrim2")
	#ytrim1 = clgeti ("ytrim1")
	#ytrim2 = clgeti ("ytrim2")
	#xskip1 = clgeti ("xskip1")
	#xskip2 = clgeti ("xskip2")
	xtrim1 = INDEFI
	xtrim2 = INDEFI
	ytrim1 = INDEFI
	ytrim2 = INDEFI
	xskip1 = INDEFI
	xskip2 = INDEFI

	# Get format string and convert to a strmac macro string.
	call clgstr ("template", format, SZ_LINE)
	if (format[1] == EOS)
	    call sprintf (format, SZ_LINE, "%s")
		call pargstr (DEFAULT_MACRO)
	call qsmkmacro (format, format, SZ_LINE)

	while (imtgetim (inlist, input, SZ_LINE) != EOF)  {
	    call quadsections (input, section, xtrim1, xtrim2, ytrim1, ytrim2,
	    xskip1, xskip2, option, format)
	}

	# Tidy up
	call hdmclose ()
	call imtclose (inlist)
end


# QUADSECTIONS -- ???

procedure quadsections (input, section, xtrim1, xtrim2, ytrim1, ytrim2,
	xskip1, xskip2, option, format)

char	input[SZ_FNAME]		#I Input image name.
char	section[SZ_LINE]	#I Default section specification
int	xtrim1			#I X pixels to trim at start of data
int	xtrim2			#I X pixels to trim at end   of data
int	ytrim1			#I Y pixels to trim at start of data
int	ytrim2			#I Y pixels to trim at end   of data
int	xskip1			#I X pixels to skip at start of overscan
int	xskip2			#I X pixels to skip at end   of overscan
int	option			#I Type of section required
char	format[SZ_LINE]		#I strmac macro string for building output

char	image[SZ_LINE], argstr[SZ_LINE], buffer[SZ_LINE]
char	insection[SZ_LINE], outsection[SZ_LINE]
int	amp, arg[9], len
int	i, x1,  x2,  y1,  y2

pointer	in, qg
bool	reduced

pointer	immap()
int	hdmaccf(), gstrcpy(), strlen(), strmac()
bool	quadsect()

begin
	# Parse input into an image name and an image section.
	call imgimage   (input, image,     SZ_LINE)
	call imgsection (input, insection, SZ_LINE)
	# if no section was supplied in the image name use the default section.
	if (insection[1] == EOS) {
	    call strcpy (section, insection, SZ_LINE)
	}

	# Open input image
	in = immap  (image, READ_ONLY, 0)

	# Determine if image has been trimmed or not
	reduced = (hdmaccf (in, "trim") == YES)

	if (reduced) { 
	    # OPT_BIASSEC does not make sense for reduced images.
	    if (option == OPT_BIASSEC)
		return
	    # Trimsec and datasec are identical for reduced images
	    if (option == OPT_TRIMSEC)
		option = OPT_DATASEC
	}

	# Set-up quadgeom structure
	call quadalloc (qg)
        if (hdmaccf (in, "HDR_REV") == NO) {
	    if (reduced) {
		call quadgeomred (in, qg)
	    } else {
		call quadgeom (in, qg, "", "")
	    }
        } else {
            call qghdr2 (in, qg)
        }

#	call quaddump (qg)

	# Adjust quadgeom structure for user trim and overscan margins
	if (! reduced) {
	    call qguser (qg, xtrim1, xtrim2, ytrim1, ytrim2, xskip1, xskip2)
	}
#	call quaddump (qg)


	# Store image name as first argument in macro argument string "argstr"
	arg[1] = 1
	arg[2] = 1 + arg[1] + gstrcpy (image, argstr, SZ_LINE)

	# Blank output string
	buffer[1] = EOS

	# Determine the intersection of the specified section with the portion
	# of the image read through each readout.
	do amp = 1, QG_NAMPS (qg) {

	    # skip any phantoms in raw images
	    if (QG_PHANTOM (qg, amp) == NO) {

		if (quadsect (qg, insection, option, amp, x1, x2, y1, y2)) {
		    # Build resulting section string ...
		    call sprintf (outsection, SZ_LINE, "[%d:%d,%d:%d]")
			call pargi (x1)
			call pargi (x2)
			call pargi (y1)
			call pargi (y2)

		    # ... and save it as second argument
		    arg[3] = 1 + arg[2] + gstrcpy (outsection, argstr[arg[2]],
		    SZ_LINE-arg[2]+1)

		    # Save Ampid as third argument
		    call strcpy (Memc[QG_AMPID(qg, amp)], argstr[arg[3]],
		    SZ_LINE-arg[3]+1)

		    # Process macro string
		    i = strmac (format, argstr, buffer, SZ_LINE)
		    call printf (buffer)
		}
	    }
	}

	# Output <lf> if format does not explicitly include one.
	len = strlen (buffer)
	if ((len > 2) && !(buffer[len-1]=='\\' && buffer[len]=='n')) {
	    call printf ("\n")
	}

	call flush (STDOUT)

	# Tidy up
	call quadfree (qg)
	call imunmap (in)
end


# QSMKMACRO -- Perform the following substitutions on the given macro string
#
# $I --> $1
# $S --> $2
# $A --> $3
# $? --> $?

procedure qsmkmacro (instr, outstr, maxchars)

char	instr[ARB]			#I Input macro string.
char	outstr[maxchars]		#O Output macro string.
int	maxchars			#I Maximum length of outstr

char	ch
int	ip, op

begin

	op = 1
	for (ip=1; instr[ip] != EOS; ip=ip+1) {
	    ch = instr[ip]
	    outstr[op] = ch
	    op = op + 1
	    if (op > maxchars)
		call error (0, "qsmkmacro: Output buffer overflow")
	    
	    if (ch == '$') {
		ip = ip + 1
		ch = instr[ip]

		if (ch == 'I') {
		    outstr (op) = '1'
		    op = op + 1
		} else if (ch == 'S') {
		    outstr (op) = '2'
		    op = op + 1
		} else if (ch == 'A') {
		    outstr (op) = '3'
		    op = op + 1
		} else {
		    outstr (op) = ch
		    op = op + 1
		}
		if (op > maxchars)
		    call error (0, "qsmkmacro: Output buffer overflow")
	    }
	}
end


# QUADSECT -- ??

bool procedure quadsect (qg, section, option, amp, x1, x2, y1, y2)

pointer	qg			#I Pointer to initialised quadgeom structure.
char	section[SZ_LINE]	#I Default section specification.
int	option			#I Type of section required.
int	amp			#I Amplifier for which section is required.
int	x1, x2, y1, y2		#O Corners of specified section.
bool	overlap			#O true if part of section read through amp.

int	xskip, xsize, yskip, ysize
int	sx1, sx2, sy1, sy2, sxs, sys
int	dx1, dx2, dy1, dy2
int	tx1, tx2, ty1, ty2
int	bx1, bx2, by1, by2

begin

	# Decode input section
	x1  = 1
	x2  = QG_NX(qg, 0)
	sxs = 1
	y1  = 1
	y2  = QG_NY(qg, 0)
	sys = 1
	call ccd_section (section, x1, x2, sxs, y1, y2, sys)
	sx1 = min (x1, x2)
	sx2 = max (x1, x2)
	sy1 = min (y1, y2)
	sy2 = max (y1, y2)

	# Set up null return (overlap) values in case no part of section was
	# read with this amplifier.
	overlap = false
	x1 = 0
	x2 = 0
	y1 = 0
	y2 = 0

	# Calculate suplimentary quantitiies as required.
	switch (option) {

	case OPT_REFLECT, OPT_DUPLICATE:
	    xskip = sx1 - QG_DX1(qg, 0)
	    xsize = sx2 - sx1 + 1
	    yskip = sy1 - QG_DY1(qg, 0)
	    ysize = sy2 - sy1 + 1
	}

	# Determine the intersection of the specified section with the portion
	# of the image read through the specified readout.
	switch (option) {

	case OPT_BIASSEC:
	    bx1 = QG_AX1(qg, amp) + QG_BX1(qg, amp) - 1
	    bx2 = QG_AX1(qg, amp) + QG_BX2(qg, amp) - 1
	    by1 = QG_AY1(qg, amp) + QG_BY1(qg, amp) - 1
	    by2 = QG_AY1(qg, amp) + QG_BY2(qg, amp) - 1

	    if (sx1 > bx2)
		return (overlap)
	    if (sx2 < bx1)
		return (overlap)
	    if (sy1 > by2)
		return (overlap)
	    if (sy2 < by1)
		return (overlap)

	    x1 = max (sx1, bx1)
	    x2 = min (sx2, bx2)
	    y1 = max (sy1, by1)
	    y2 = min (sy2, by2)

	case OPT_DATASEC:
	    dx1 = QG_AX1(qg, amp) + QG_DX1(qg, amp) - 1
	    dx2 = QG_AX1(qg, amp) + QG_DX2(qg, amp) - 1
	    dy1 = QG_AY1(qg, amp) + QG_DY1(qg, amp) - 1
	    dy2 = QG_AY1(qg, amp) + QG_DY2(qg, amp) - 1

	    if (sx1 > dx2)
		return (overlap)
	    if (sx2 < dx1)
		return (overlap)
	    if (sy1 > dy2)
		return (overlap)
	    if (sy2 < dy1)
		return (overlap)

	    x1 = max (sx1, dx1)
	    x2 = min (sx2, dx2)
	    y1 = max (sy1, dy1)
	    y2 = min (sy2, dy2)

	case OPT_TRIMSEC:
	    tx1 = QG_AX1(qg, amp) + QG_TX1(qg, amp) - 1
	    tx2 = QG_AX1(qg, amp) + QG_TX2(qg, amp) - 1
	    ty1 = QG_AY1(qg, amp) + QG_TY1(qg, amp) - 1
	    ty2 = QG_AY1(qg, amp) + QG_TY2(qg, amp) - 1

	    if (sx1 > tx2)
		return (overlap)
	    if (sx2 < tx1)
		return (overlap)
	    if (sy1 > ty2)
		return (overlap)
	    if (sy2 < ty1)
		return (overlap)

	    x1 = max (sx1, tx1)
	    x2 = min (sx2, tx2)
	    y1 = max (sy1, ty1)
	    y2 = min (sy2, ty2)

	case OPT_REFLECT:
	    dx1 = QG_AX1(qg, amp) + QG_DX1(qg, amp) - 1
	    dx2 = QG_AX1(qg, amp) + QG_DX2(qg, amp) - 1
	    dy1 = QG_AY1(qg, amp) + QG_DY1(qg, amp) - 1
	    dy2 = QG_AY1(qg, amp) + QG_DY2(qg, amp) - 1

	    switch (QG_AMPTYPE(qg, amp)) {
	    case AMP11:
		x1  = dx1 + xskip
		x2  = x1  + xsize - 1
		y1  = dy1 + yskip
		y2  = y1  + ysize - 1

	    case AMP12:
		x2  = dx2 - xskip
		x1  = x2  - xsize + 1
		y1  = dy1 + yskip
		y2  = y1  + ysize - 1

	    case AMP21:
		x1  = dx1 + xskip
		x2  = x1  + xsize - 1
		y2  = dy2 - yskip
		y1  = y2  - ysize + 1

	    case AMP22:
		x2  = dx2 - xskip
		x1  = x2  - xsize + 1
		y2  = dy2 - yskip
		y1  = y2  - ysize + 1
	    }
		
	    if (x1 > dx2)
		return (overlap)
	    if (x2 < dx1)
		return (overlap)
	    if (y1 > dy2)
		return (overlap)
	    if (y2 < dy1)
		return (overlap)

	    x1 = max (x1, dx1)
	    x2 = min (x2, dx2)
	    y1 = max (y1, dy1)
	    y2 = min (y2, dy2)

	case OPT_DUPLICATE:
	    dx1 = QG_AX1(qg, amp) + QG_DX1(qg, amp) - 1
	    dx2 = QG_AX1(qg, amp) + QG_DX2(qg, amp) - 1
	    dy1 = QG_AY1(qg, amp) + QG_DY1(qg, amp) - 1
	    dy2 = QG_AY1(qg, amp) + QG_DY2(qg, amp) - 1

	    x1  = dx1 + xskip
	    x2  = x1  + xsize - 1
	    y1  = dy1 + yskip
	    y2  = y1  + ysize - 1

	    if (x1 > dx2)
		return (overlap)
	    if (x2 < dx1)
		return (overlap)
	    if (y1 > dy2)
		return (overlap)
	    if (y2 < dy1)
		return (overlap)

	    x1 = max (x1, dx1)
	    x2 = min (x2, dx2)
	    y1 = max (y1, dy1)
	    y2 = min (y2, dy2)

	}

	overlap = true
	return (overlap)

end
