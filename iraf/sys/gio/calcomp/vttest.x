# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <gki.h>
include	<gset.h>
include	<gio.h>
include <ctype.h>
include <math.h>
include "ccp.h"

define	SZ_BUF 2048
define	PIOVER4		(0.25 * PI)
define	THREEPIOVER4	(0.75 * TWOPI)
define	MAXCH	15

# X_VTTEST -- testing task for simulating calcomp kernel routines on vt640

task vttest = t_vttest

# T_VTTEST -- test low-level Calcomp graphics simulation routines on vt640

procedure t_vttest ()

char	lname[SZ_FNAME], tname[SZ_FNAME], devname[SZ_FNAME]
int	ltype, lwidth, npts, n, mtype, i
char	testoption
pointer	x, y, gp, sim_gp
short	p[ARB]

pointer	sp, nambuf, pl, pm
int	clgeti (), strlen ()
real	clgetr ()
char	clgetc ()
pointer	ttygdes (), gopen ()

include "ccp.com"
common	/simulate/ sim_gp

string	fdevice	"vt640"

begin
	call smark (sp)
	call salloc (nambuf, SZ_FNAME, TY_CHAR)

	testoption= clgetc ("option")
	if (testoption == 'l') {
	    call clgstr ("lname", lname, SZ_FNAME)
	    ltype = clgeti ("ltype")
	    lwidth = clgeti ("lwidth")			# width in rel. units
	    g_dashlen = clgetr ("dashlen")
	    g_gaplen  = clgetr ("gaplen")
	    g_plwsep  = clgetr ("plwsep")
	} else if (testoption == 't') {
	    call clgstr ("tname", tname, SZ_FNAME)
	    g_plwsep  = clgetr ("plwsep")
	} else if (testoption == 'm') {
	    mtype  = clgeti ("mtype")
	}
	call clgstr ("device", devname, SZ_FNAME)

	n = strlen (devname)
	if (g_device[1] == EOS) {
	    call achtsc (devname, Memc[nambuf], n)
	    Memc[nambuf+n] = EOS
	}
	iferr (g_tty = ttygdes (Memc[nambuf]))
	    call erract (EA_ERROR)
	g_cc = NULL
	call ccp_init (g_tty, Memc[nambuf])
	call ccp_reset ()

	g_xndcto_p = 1.0	# for testing, raw data is NDC-space (0-32767)
	g_yndcto_p = 1.0	# (that is, after passing through to_short())
	g_ltover   = false
	g_lwover   = true

	pl = CCP_PLAP(g_cc)
	pm = CCP_PMAP(g_cc)

	PL_LTYPE(pl) = ltype
	PL_WIDTH(pl) = GKI_PACKREAL(lwidth)
	PM_LTYPE(pm) = mtype

	gp = gopen (devname, NEW_FILE, STDGRAPH)
	sim_gp = gp
	call gsview (gp, 0.0, 0.63, 0.0, 1.0)		# square viewport
	call gswind (gp, 0.0, 32767.0, 0.0, 32767.0)

	switch (testoption) {

	case 'l':					# polyline

	    call rddata (lname, x, y, npts)		# range 0.0-1.0
	    call to_short (Memr[x], Memr[y], npts, p)	# range 0-32767
	    call ccp_polyline (p, npts)

	case 't':					# text

	    call testtext (gp, tname)			# read, calc, call ccppl

	case 'm':					# polymarker

	    call rddata (lname, x, y, npts)		# x,y array of mrkr pos.
	    do i = 1, npts {
		call calcmarker (32767 * Memr[x+i-1], 32767 * Memr[y+i-1], 
		    mtype, p, npts)
		call ccp_polymarker (p, npts)
	    }
	}
			 
	call gclose (gp)
	call ccp_close ()				# free g descriptors
	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call sfree (sp)
end

# TO_SHORT -- convert x, y real arrays to short integers as NDC coords

procedure to_short (x, y, npts, p)

real	x[ARB], y[ARB]
int	npts
short	p[ARB]

int	i, j

begin
	do i = 1, npts, 1 {
	    j = (i - 1) * 2 + 1
	    p[j]   = x[i] * 32767
	    p[j+1] = y[i] * 32767
	}
	return
end

# CALCMARKER -- calculate and return a pattern of points representing a
# polymarker of the specified type, origined at x, y.

procedure calcmarker (x, y, marktype, p, npts)

real	x,y		# GKI_NDC coordinates of marker origin
int	marktype	# polymarker type, specified in GIO specs
short	p[ARB]		# output array of points defining marker, in GKI_NDC
int	npts		# no. of points; x,y pairs (= 1/2 elements in p)

int	i, j, m, fill
real	xsize, ysize
pointer	tx
int	and()

include "ccp.com"
include	"/iraf/sys/gio/markers.dat"

begin
	tx    = CCP_TXAP(g_cc)
	xsize = CCP_CHARHEIGHT(g_cc,1) * GKI_UNPACKREAL(TX_SIZE(tx))
	ysize = xsize	# for now
	# The point marker type cannot be combined with the other types and
	# is treated as a special case.  The remaining markers are drawn
	# using GUMARK, which draws marks represented as polygons

	if (marktype == GM_POINT || (xsize == 0 && ysize == 0)) {
	    p[1] = x
	    p[2] = y
	    npts = 1

	} else {

	    # The polylines for the standard marks are stored in MPX and MPY 
	    # at offsets MXO and MYO.
	    fill = NO
	    npts = 0
	    do i = GM_FIRSTMARK, GM_LASTMARK
		if (and (marktype, 2 ** i) != 0) {
		    m = i - GM_FIRSTMARK + 1
		    do j = 1, mnpts[m] {
			npts = npts + 1
			p[npts*2-1] = x - 0.5 * xsize + xsize * mpx[moff[m]+j-1]
			p[npts*2]   = y - 0.5 * ysize + ysize * mpy[moff[m]+j-1]
		    }
		}
	}
end


procedure rddata (fname, x, y, npts)

char	fname[ARB]
pointer	x, y
int	npts

int	buflen, n, fd, ncols, lineno, i, status, testint
pointer	sp, lbuf, ip
real	xval, yval, maxy
int	getline(), nscan(), open()
errchk	open, sscan, getline, malloc

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	buflen = SZ_BUF
	iferr {
	    call malloc (x, buflen, TY_REAL)
	    call malloc (y, buflen, TY_REAL)
	} then
	    call erract (EA_FATAL)

	n = 0
	ncols = 0
	lineno = 0

	status = 0
	while (status != EOF) {
	    iferr (status = getline (fd, Memc[lbuf])) {
		call eprintf ("getline error from rddata: status=%d\n")
		    call pargi (status)
		call erract (EA_FATAL)
	    }
	    if (status == EOF) 
		next
	    # Skip comment lines and blank lines.
	    lineno = lineno + 1
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
		call gargr (xval)
		call gargr (yval)

	    # The first line determines whether we have an x,y list or a
	    # y-list.  It is an error if only one value can be decoded when
	    # processing a two column list.

	    if (ncols == 0 && nscan() > 0)
		ncols = nscan()
	    
	    switch (nscan()) {
	    case 0:
		call eprintf ("no args; %s, line %d: %s\n")
		    call pargstr (fname)
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		next
	    case 1:
		yval = xval
	    default:	# normally, ncols=2
		if (ncols != 2) {
		    call eprintf ("weird data; file %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		}
	    }

	    n = n + 1
	    if (n > buflen) {
		buflen = buflen + SZ_BUF
		call realloc (x, buflen, TY_REAL)
		call realloc (y, buflen, TY_REAL)
	    }

	    Memr[x+n-1] = xval
	    Memr[y+n-1] = yval
	    testint = x+n-1
	}

	if (ncols == 1) {
	    maxy = 0.0
	    do i = 1, n 
		maxy = max (Memr[y+i-1], maxy)
	    do i = 1, n
		Memr[x+i-1] = maxy * real(i) / real(n)
	}
	call realloc (x, n, TY_REAL)
	call realloc (y, n, TY_REAL)

	call close (fd)
	call sfree (sp)
	npts = n
end


# RPTHETA4 -- Polar angle, Real precision, 4 arguments; from p1(x,y) to p2(x,y):
# angle between line segment p1-p2 and horizontal +x axis centered on p1; 
# returned in radians; single precision (see pdtheta4).

real procedure rptheta4 (p1x, p1y, p2x, p2y)

real p1x,p1y, p2x,p2y	# x,y of each point

real	dx, dy, ang

begin
	dx = p2x - p1x
	dy = p2y - p1y
	if (dx == 0.0) {
	    if (dy >= 0.0) {
		ang = HALFPI
	    } else {
		ang = THREEPIOVER4
	    }
	} else {
	    ang = atan (dy / dx)
	    if (dx < 0.0) {		# 2nd or 3rd quadrant
		ang = ang + PI
	    } else if (dy < 0.0) {	# 4th quadrant
		ang = ang + TWOPI
	    }
	}
	return (ang)
end

# PLOT -- simulate Calcomp's PLOT routine for testing development version of
# calcomp kernel

procedure plot (x, y, pencode)

real	x,y	# plotter coords (ndc in simulation)
int	pencode

real	lastp_x, lastp_y

pointer	gp
common	/simulate/ gp

begin
	if (pencode == CCP_DOWN) 
	    call gline (gp, lastp_x, lastp_y, x, y)
	if (pencode == CCP_DOWN || pencode == CCP_UP) {
	    lastp_x = x
	    lastp_y = y
	}
end

# PLOTS -- simulate calcomp plots routine for testing ccp code on vt640

procedure plots (dum1, dum2, ldev)

int	dum1, dum2, ldev

begin
	return
end


# NEWPEN -- temporary dummy routine for simulating Calcomp

procedure newpen (whichpen)

int	whichpen

begin
	return
end

# SYMBOL -- simulate Calcomp's SYMBOL routine for testing development version of
# calcomp kernel

procedure symbol (xp, yp, size, ch, orien, nchar)

real	xp,yp	# plotter coords (ndc in simulation)
real	size	# char size in plotter coords
char	ch[ARB]	# chars to be drawn
real	orien	# degrees counterclockwise from +x to rightward vector
int	nchar	# number of chars

pointer	gp
common	/simulate/ gp

string	format ""

begin
	ch[nchar+1] = EOS
	call gseti (gp, G_TXUP, 90 + int(orien))
	call gsetr (gp, G_TXSIZE, size)
	call gtext (gp, xp, yp, ch, format)
end


# TESTTEXT -- read sequential lines from designated file and call ccp_text to
# draw text at specified coordinates in specified format.

procedure testtext (gp, fname) 

pointer	gp			# graphics device 
char	fname[SZ_FNAME]		# name of file from which to extract table

int	fd, textlen, restlen, ip, op
char	lbuf[SZ_LINE], ttext[SZ_LINE], rest[SZ_LINE], tformat[SZ_LINE], quote
short	sttext[SZ_LINE]
real	x,y
int	open (), strlen (), getline (), nscan ()

string	errmsg "unable to open table file                     "
data	quote/34/

begin
	iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
	    call sprintf (errmsg[27], SZ_FNAME, "%s")
		call pargstr (fname)
	    call fatal (EA_FATAL, errmsg)
	}

	while (getline (fd, lbuf) != EOF) {
	    # Skip comment lines and blank lines.
	    if (lbuf[1] == '#')
		next
	    for (ip=1;  IS_WHITE(lbuf[ip]);  ip=ip+1)
		;
	    if (lbuf[ip] == '\n' || lbuf[ip] == EOS)
		next

	    # Decode.
	    call sscan (lbuf[ip])
		call gargr (x)
		call gargr (y)
		call gargstr (rest, SZ_LINE)

	    if (nscan() < 3)	# insufficient fields; ignore line, not nice.
		next
	    
	    restlen = strlen (rest)

	    # Pull out text buffer:
	    for (ip=1; rest[ip] != quote && ip < restlen; ip=ip+1)	#->1st "
		;
	    op = 0
	    for (ip=ip+1; rest[ip] != quote && ip < restlen; ip=ip+1) {
		op        = op + 1
		ttext[op] = rest[ip];
	    }
	    textlen       = op
	    ttext[op+1]   = EOS

	    # Pull out format string:
	    for (ip=ip+1; IS_WHITE(rest[ip]); ip=ip+1)		#-> past whitesp
		;
	    op = 0
	    for (; ip <= restlen && !IS_WHITE(rest[ip]); ip=ip+1) {
		op          = op + 1
		tformat[op] = rest[ip];
	    }
	    tformat[op+1]   = EOS
		
	    # set ccp descriptor text attributes if specified:
	    if (tformat[1] != EOS)
		call testxset (tformat)
	    call achtcs (ttext, sttext, textlen) # ccp_text expects short text
	    sttext[textlen+1] = EOS
	    call ccp_text (nint(x), nint(y), sttext, textlen)
	}
	call close (fd)
end


# TESTXSET -- Parse a text drawing format string and set the values of the text
# attributes in the TX (g_cc) output structure.

procedure testxset (format)

char	format[ARB]		# text attribute format string

pointer tx
char	attribute[MAXCH], value[MAXCH]
real	tempsize
int	ip, op, tip, temp, ch
int	h_v[4], v_v[4], f_v[4], q_v[4], p_v[4]
int	ctoi(), ctor(), stridx()

include	"ccp.com"

define	badformat_ 91

string	h_c	"nclr"
data	h_v	/GT_NORMAL,	GT_CENTER,	GT_LEFT,	GT_RIGHT/
string	v_c	"nctb"
data	v_v	/GT_NORMAL,	GT_CENTER,	GT_TOP,		GT_BOTTOM/
string	f_c	"rgib"
data	f_v	/GT_ROMAN,	GT_GREEK,	GT_ITALIC,	GT_BOLD/
string	q_c	"nlmh"
data	q_v	/GT_NORMAL,	GT_LOW,		GT_MEDIUM,	GT_HIGH/
string	p_c	"lrud"
data	p_v	/GT_LEFT,	GT_RIGHT,	GT_UP,		GT_DOWN/

begin
	# ccp kernel text descriptor:
	tx = CCP_TXAP(g_cc)

	# Parse the format string and set the text attributes.  The code is
	# more general than need be, i.e., the entire attribute name string
	# is extracted but only the first character is used.  Whitespace is
	# permitted and ignored.

	for (ip=1;  format[ip] != EOS;  ip=ip+1) {
	    # Extract the next "attribute=value" construct.
	    while (IS_WHITE (format[ip]))
		ip = ip +1

	    op = 1
	    for (ch=format[ip];  ch != EOS && ch != '=';  ch=format[ip]) {
		if (op <= MAXCH) {
		    attribute[op] = format[ip]
		    op = op + 1
		}
		ip = ip + 1
	    }
	    attribute[op] = EOS

	    if (ch == '=')
		ip = ip + 1

	    op = 1
	    while (IS_WHITE (format[ip]))
		ip = ip +1
	    ch = format[ip]
	    while (ch != EOS && ch != ';' && ch != ',') {
		if (op <= MAXCH) {
		    value[op] = format[ip]
		    op = op + 1
		}
		ip = ip + 1
		ch = format[ip]
	    }
	    value[op] = EOS

	    if (attribute[1] == EOS || value[1] == EOS)
		break

	    # Decode the assignment and set the corresponding text attribute
	    # in the graphics descriptor.

	    switch (attribute[1]) {
	    case 'u':				# character up vector
		tip = 1
		if (ctoi (value, tip, TX_UP(tx)) <= 0) {
		    TX_UP(tx) = 90
		    goto badformat_
		}

	    case 'p':				# path
		temp = stridx (value[1], p_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_PATH(tx) = p_v[temp]

	    case 'c':				# color
		tip = 1
		if (ctoi (value, tip, TX_COLOR(tx)) <= 0) {
		    TX_COLOR(tx) = 1
		    goto badformat_
		}

	    case 's':				# character size scale factor
		tip = 1
		if (ctor (value, tip, tempsize) <= 0) {
		    TX_SIZE(tx) = GKI_PACKREAL(1.0)
		    goto badformat_
		} 
		TX_SIZE(tx) = GKI_PACKREAL(tempsize)

	    case 'h':				# horizontal justification
		temp = stridx (value[1], h_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_HJUSTIFY(tx) = h_v[temp]

	    case 'v':				# vertical justification
		temp = stridx (value[1], v_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_VJUSTIFY(tx) = v_v[temp]

	    case 'f':				# font
		temp = stridx (value[1], f_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_FONT(tx) = f_v[temp]
		
	    case 'q':				# font quality
		temp = stridx (value[1], q_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_QUALITY(tx) = q_v[temp]

	    default:
badformat_	call eprintf ("Warning (testtxset): bad gtext format '%s'\n")
		    call pargstr (format)
	    }

	    if (format[ip] == EOS)
		break
	}
end
