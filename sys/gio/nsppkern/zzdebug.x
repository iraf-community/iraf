# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<mach.h>
include	<fset.h>
include	<gset.h>
include	"font.h"

define	XS	0.216
define	XE	0.719
define	YS	0.214
define	YE	0.929

task	grid	= t_grid,
	grey	= t_grey,
	text	= t_text, 
	seefont = t_seefont,
	txup	= t_txup,
	font	= t_font,
	efont	= t_efont


# GRID -- Test program for graphics plotting.  A labelled grid is output.

procedure t_grid ()

pointer	gp
bool	redir
char	command[SZ_LINE], image[SZ_FNAME], word[SZ_LINE]
char	output[SZ_FNAME], output_file[SZ_FNAME], device[SZ_FNAME]
int	cmd, input_fd, stat, fd

pointer	gopen()
bool	streq()
int	fstati(), open(), getline(), sscan()

begin
	# If the input has been redirected, input is read from the named
	# command file.  If not, each image name in the input template is
	# plotted.
	
	if (fstati (STDIN, F_REDIR) == YES) {
call eprintf ("Input has been redirected\n")
	    redir = true
	    cmd = open (STDIN, READ_ONLY, TEXT_FILE)
	} 

	# Loop over commands until EOF
	repeat {
	    if (redir) {
		if (getline (STDIN, command, SZ_LINE) == EOF)
		    break
		stat = sscan (command)
		    call gargwrd (word, SZ_LINE)
		if (!streq (word, "plot")) {
		    # Pixel window has been stored as WCS 2
		    call gseti (gp, G_WCS, 2)
		    call gscan (command)
		    next
		} else 
		    call gargwrd (image)
	    } 

	    call clgstr ("output", output, SZ_FNAME)
	    if (!streq (output, "")) {
	        call strcpy (output, output_file, SZ_FNAME)
	        fd = open (output_file, NEW_FILE, BINARY_FILE)
	    } else
		fd = open ("dev$crt", NEW_FILE, BINARY_FILE)

	    call clgstr ("device", device, SZ_FNAME)
	    gp = gopen (device, NEW_FILE, fd)
		
	    call gseti (gp, G_XDRAWGRID, 1)
	    call gseti (gp, G_YDRAWGRID, 1)
	    call gseti (gp, G_NMAJOR, 21)
	    call glabax (gp, "TEST", "NDC_X", "NDC_Y")
	    call gline (gp, XS, YS, XE, YS)
	    call gline (gp, XE, YS, XE, YE)
	    call gline (gp, XE, YE, XS, YE)
	    call gline (gp, XS, YE, XS, YS)
	    call gmark (gp, 0.5, 0.5, GM_CROSS, 3.0, 3.0)
	    call gtext (gp, XS, YS-0.1, "DICOMED crtpict film area")
	    call gclose  (gp)
	    call close   (fd)
	}

	call clpcls (input_fd)
end


# GREY -- test code to generate grey scale on plotters

procedure t_grey()

pointer	gp
real	size
int	i, fd, count
short	celldata[1024]
char	output[SZ_FNAME], device[SZ_FNAME]

pointer	gopen()
real	clgetr()
int	open(), clgeti()
string	fmt "hj=c;vj=c"

begin
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	fd = open (output, NEW_FILE, BINARY_FILE)
	gp = gopen (device, NEW_FILE, fd)

	size = clgetr ("size")
		
	call gsetr (gp, G_TXSIZE, size)
	call gtext (gp, .5, .9, "! !\"#$%&'()*+,-./", fmt)
	call gtext (gp, .5, .8, "1234567890", fmt)
	call gtext (gp, .5, .7, "ABCDEFGHIJKLMNOPQR", fmt)
	call gtext (gp, .5, .6, "STUVWXYZ[\\]^_`", fmt)
	call gtext (gp, .5, .5, "abcdefghijklmnopqr", fmt)
	call gtext (gp, .5, .4, "stuvwxyz{}|~", fmt)

	call gtext (gp, .5, .1, "Grey Scale Test", fmt)

	count = clgeti ( "count")
	if (count > 1024)
	    count = 1024
	for (i=1;  i <= count;  i=i+1)
	    celldata[i] = i - 1

	call gpcell (gp, celldata, count, 1, 0.05, 0.2, .95, 0.3)

	call gclose (gp)
	call close (fd)
end


# TEXT -- Test character drawing.

procedure t_text()

char	device[SZ_FNAME]
char	output[SZ_FNAME]
int	fd, open()
pointer	gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	fd = open (output, NEW_FILE, BINARY_FILE)
	gp = gopen (device, NEW_FILE, fd)

	call gsetr (gp, G_TXSIZE, 1.0)

	call gtext (gp, .1, .1,
	    "abcdefghijklmnopqrstuvwxyz", "hj=l,vj=b")
	call gtext (gp, .1, .2,
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "hj=l,vj=b")
	call gtext (gp, .1, .3,
	    "0123456789", "hj=l,vj=b")
	call gtext (gp, .1, .4,
	    " ,./<>?;:'\"\\|[]{}!@#$%^&*()-_=+`~", "hj=l,vj=b")

	call gsetr (gp, G_TXSIZE, 2.0)

	call gtext (gp, .1, .5,
	    "abcdefghijklmnopqrstuvwxyz", "hj=l,vj=b")
	call gtext (gp, .1, .6,
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "hj=l,vj=b")
	call gtext (gp, .1, .7,
	    "0123456789", "hj=l,vj=b")
	call gtext (gp, .1, .8,
	    " ,./<>?;:'\"\\|[]{}!@#$%^&*()-_=+`~", "hj=l,vj=b")

	call gclose (gp)
	call close (fd)
end


# SEEFONT definitions.
define	L	.40
define	R	.60
define	U	.75
define	D	.25
define	W	(R-L)
define	H	(U-D)


# SEEFONT -- Draw a character from the font table.

procedure t_seefont()

char	ch
pointer	gp
real	x, y
int	wcs, key
char	strval[SZ_FNAME]

pointer	gopen()
int	clgcur()

begin
	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)

	call gline (gp, L, D, R, D)
	call gline (gp, R, D, R, U)
	call gline (gp, R, U, L, U)
	call gline (gp, L, U, L, D)

	ch = 'A'
	call gdrwch (gp, L, D, ch, W, H)

	while (clgcur ("gcur", x, y, wcs, key, strval, SZ_FNAME) != EOF) {
	    call gclear (gp)

	    call gline (gp, L, D, R, D)
	    call gline (gp, R, D, R, U)
	    call gline (gp, R, U, L, U)
	    call gline (gp, L, U, L, D)

	    ch = key
	    call gdrwch (gp, L, D, ch, W, H)
	}

	call gclose (gp)
end


# GDRWCH -- Draw a character of the given size and orientation at the given
# position.

procedure gdrwch (gp, x, y, ch, xsize, ysize)

pointer	gp			# graphics descriptor
real	x, y			# lower left NDC coords of character
char	ch			# character to be drawn
real	xsize, ysize		# size of character in NDC units

real	px, py
int	stroke, tab1, tab2, i, pen
int	bitupk()
include	"font.com"
common	/font/ chridx, chrtab

begin
	if (ch < CHARACTER_START || ch > CHARACTER_END)
	    i = '?' - CHARACTER_START + 1
	else
	    i = ch  - CHARACTER_START + 1

	tab1 = chridx[i]
	tab2 = chridx[i+1] - 1

	do i = tab1, tab2 {
	    stroke = chrtab[i]
	    px  = bitupk (stroke, COORD_X_START,   COORD_X_LEN)
	    py  = bitupk (stroke, COORD_Y_START,   COORD_Y_LEN)
	    pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN)

	    px = x + ((px + FONT_LEFT)   / FONT_WIDTH)  * xsize
	    py = y + ((py + FONT_BOTTOM) / FONT_HEIGHT) * ysize

	    if (pen == 0)
		call gamove (gp, px, py)
	    else
		call gadraw (gp, px, py)
	}
end


# TXUP -- Draw text strings with various character up vectors and paths.

procedure t_txup()

char	device[SZ_FNAME]
char	output[SZ_FNAME]
char	text[SZ_LINE]
int	fd, open(), clgeti()
pointer	gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	fd = open (output, NEW_FILE, BINARY_FILE)
	gp = gopen (device, NEW_FILE, fd)

	call clgstr ("text", text, SZ_LINE)

	call gseti (gp, G_TXHJUSTIFY, clgeti("hjustify"))
	call gseti (gp, G_TXVJUSTIFY, clgeti("vjustify"))

	call gmark (gp, .1, .2, GM_CROSS, 3., 3.)
	call gtext (gp, .1, .2, text, "up=0,path=right")
	# --
	call gmark (gp, .2, .2, GM_CROSS, 3., 3.)
	call gtext (gp, .2, .2, text, "up=45,path=right")
	# --
	call gmark (gp, .3, .2, GM_CROSS, 3., 3.)
	call gtext (gp, .3, .2, text, "up=90,path=right")
	# --
	call gmark (gp, .4, .2, GM_CROSS, 3., 3.)
	call gtext (gp, .4, .2, text, "up=135,path=right")
	# --
	call gmark (gp, .5, .2, GM_CROSS, 3., 3.)
	call gtext (gp, .5, .2, text, "up=180,path=right")

	call gmark (gp, .1, .4, GM_CROSS, 3., 3.)
	call gtext (gp, .1, .4, text, "up=90,path=left")
	# --
	call gmark (gp, .2, .4, GM_CROSS, 3., 3.)
	call gtext (gp, .2, .4, text, "up=90,path=right")
	# --
	call gmark (gp, .3, .4, GM_CROSS, 3., 3.)
	call gtext (gp, .3, .4, text, "up=90,path=up")
	# --
	call gmark (gp, .4, .4, GM_CROSS, 3., 3.)
	call gtext (gp, .4, .4, text, "up=90,path=down")

	call gclose (gp)
	call close (fd)
end


# FONT -- Test the font change escapes.

procedure t_font()

char	device[SZ_FNAME]
char	output[SZ_FNAME]
char	text[SZ_LINE], format[SZ_FNAME]
int	fd, i, open()
pointer	gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	fd = open (output, NEW_FILE, BINARY_FILE)
	gp = gopen (device, NEW_FILE, fd)

	do i = 2, 8, 2 {
	    call clgstr ("text", text, SZ_LINE)
	    call clgstr ("format", format, SZ_FNAME)
	    call gtext (gp, .2, i / 10.0, text, format)
	}

	call gclose (gp)
	call close (fd)
end


# EFONT -- Font editor.

procedure t_efont()

char	cmd[SZ_LINE]
real	scale
int	pen, x, y, nw, w1, w2, ch, fcn
int	ip, i, tab1, tab2, stroke, junk

int	bitupk(), ctoi(), ctor(), getline()
short	chridx[96], chrtab[800]
common	/font/ chridx, chrtab
define	decode_ 91

begin
	repeat {
	    # Get command.
	    call clgstr ("cmd", cmd, SZ_FNAME)
	    if (cmd[1] == 'q')
		break

	    # Decode function and integer arguments (range of words).
	    # Format "fcn [scale] ch w1 w2".

	    fcn = cmd[1]
	    ip = 2

	    scale = 0
	    if (fcn == 'x' || fcn == 'y') 
		if (ctor (cmd, ip, scale) <= 0)
		    scale = 1.0

	    while (IS_WHITE(cmd[ip]))
		ip = ip + 1

	    ch = cmd[ip]
	    ip = ip + 1

	    if (ctoi (cmd, ip, w1) < 0)
		w1 = 1
	    if (ctoi (cmd, ip, w2) < 0)
		w2 = w1

	    if (ch < CHARACTER_START || ch > CHARACTER_END)
		next
	    else
		i = ch  - CHARACTER_START + 1

	    tab1 = chridx[i]
	    tab2 = chridx[i+1] - 1

	    nw = tab2 - tab1 + 1
	    w1 = max(1, min(nw, w1))
	    w2 = max(1, min(nw, w2))

call eprintf ("fcn=%c [%g], ch=%c, tab1=%d, tab2=%d, nw=%d, w1=%d, w2=%d\n")
call pargi(fcn); call pargr (scale);
call pargi(ch); call pargi(tab1); call pargi(tab2)
call pargi(nw); call pargi(w1); call pargi(w2)

	    # Functions:
	    #
	    #	w	write codes
	    #	r	read and encode
	    #   x	scale in X
	    #   y       scale in Y

	    do i = w1-1+tab1, w2-1+tab1 {
		stroke = chrtab[i]
		x   = bitupk (stroke, COORD_X_START,   COORD_X_LEN)
		y   = bitupk (stroke, COORD_Y_START,   COORD_Y_LEN)
		pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN)

		switch (fcn) {
		case 'w':
decode_		    call eprintf ("%2d %6d (%6o) %d %3d %3d\n")
			call pargi (i - tab1 + 1)
			call pargi (stroke)
			call pargi (stroke)
			call pargi (pen)
			call pargi (x)
			call pargi (y)
		    next

		case 'r':
		    junk = getline (STDIN, cmd)
		    ip = 1
		    junk = ctoi (cmd, ip, pen)
		    junk = ctoi (cmd, ip, x)
		    junk = ctoi (cmd, ip, y)
		    call bitpak (x,   stroke, COORD_X_START,   COORD_X_LEN)
		    call bitpak (y,   stroke, COORD_Y_START,   COORD_Y_LEN)
		    call bitpak (pen, stroke, COORD_PEN_START, COORD_PEN_LEN)
		    chrtab[i] = stroke
		    goto decode_

		case 'x':
		    x = x * scale
		    call bitpak (x,   stroke, COORD_X_START,   COORD_X_LEN)
		    call bitpak (y,   stroke, COORD_Y_START,   COORD_Y_LEN)
		    call bitpak (pen, stroke, COORD_PEN_START, COORD_PEN_LEN)
		    chrtab[i] = stroke
		    goto decode_

		case 'y':
		    y = (y - FONT_BASE) * scale + FONT_BASE
		    call bitpak (x,   stroke, COORD_X_START,   COORD_X_LEN)
		    call bitpak (y,   stroke, COORD_Y_START,   COORD_Y_LEN)
		    call bitpak (pen, stroke, COORD_PEN_START, COORD_PEN_LEN)
		    chrtab[i] = stroke
		    goto decode_

		default:
		    call eprintf ("unknown function code\n")
		}
	    }
	}
end
