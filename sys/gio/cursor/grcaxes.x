# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include	"gtr.h"
include	"grc.h"

define	LEN_POLYLINE	128		# polyline for axes and ticks
define	NTICKS		6		# default rough ticks on an axis
define	SZ_TICKFORMAT	6		# "%0.Xg"
define	SZ_TICKLABEL	10		# encoded tick label
define	TICKLEN		0.03		# tick length, ndc units
define	LABELOFFSET	1.5		# offset to tick label in ticklen units


# GRC_AXES -- Draw and label the axes of the viewport.  This is a simple
# routine not intended to be competitive with GLABAX.  We draw a box around
# the edge of the screen, find and label the ticks within the plotting area.

procedure grc_axes (stream, sx, sy, raster, rx, ry)

int	stream			#I graphics stream
real	sx, sy			#I screen coords of cursor
int	raster			#I raster number
real	rx, ry			#I raster coords of cursor

char	tickformat[SZ_TICKFORMAT], ticklabel[SZ_TICKLABEL]
pointer tr, w, ap, save_op
int	xt, yt, nwords, nticks, wcs, lt_save
real	xb, xe, x1, dx, x, y, lw_save
real	yb, ye, y1, dy, aspect_ratio, xticklen, yticklen

int	gt_ndigits()
pointer	gtr_init()
real	ttygetr()

errchk	gtr_init, ttygetr, realloc, gax_start
include	"gtr.com"

begin
	tr = gtr_init (stream)

	# Draw the axes with a solid polyline of width 2.0.
	ap = TR_PLAP(tr)
	lt_save = PL_LTYPE(ap);	PL_LTYPE(ap) = GL_SOLID
	lw_save = PL_WIDTH(ap);	PL_WIDTH(ap) = 2.0

	# Select a WCS.
	call grc_scrtowcs (stream, sx, sy, raster, rx, ry, x1, y1, wcs)
	w = TR_WCSPTR(tr,wcs)

	# Get the coordinates of the axes corners and the tick parameters.
	call gax_findticks (w, xb,xe,yb,ye, x1,dx,xt, y1,dy,yt)

	# Mark the position in the frame buffer.  The axes drawing instructions
	# will be appended to the frame buffer by the drawing routines.  When
	# we get all done we will move these instructions to the scratch buffer
	# and reset the frame buffer pointers, since we do not want the axes
	# to be a permanent part of the plot.

	save_op = TR_OP(tr)

	# Compute the X and Y tick lengths in NDC coordinates, corrected for
	# the aspect ratio and workstation transformation.

	aspect_ratio = ttygetr (TR_TTY(tr), "ar")
	if (aspect_ratio < .001)
	    aspect_ratio = 1.0
	xticklen = TICKLEN / xscale * aspect_ratio
	yticklen = TICKLEN / yscale

	# Construct the polyline to draw the first two axes and ticks.  We
	# start at the lower left and draw to the lower right then upper right.

	nticks = int ((xe - xb) / dx)			# Bottom axis.
	call gax_start (xb, yb)
	call gax_draw (x1, yb)
	call gax_tick (0., yticklen)

	for (x=x1+dx;  nticks > 0;  nticks=nticks-1) {
	    call gax_draw (min(x,xe), yb)
	    call gax_tick (0., yticklen)
	    x = x + dx
	}

	nticks = int ((ye - yb) / dy)			# Right axis.
	call gax_draw (xe, yb)
	call gax_draw (xe, y1)
	call gax_tick (-xticklen, 0.)

	for (y=y1+dy;  nticks > 0;  nticks=nticks-1) {
	    call gax_draw (xe, min(y,ye))
	    call gax_tick (-xticklen, 0.)
	    y = y + dy
	}

	call gax_draw (xe, ye)
	call gax_flush (stream)

	# Construct the polyline to draw the second two axes and ticks.  We
	# start at the lower left and draw to the upper left then upper right.

	nticks = int ((ye - yb) / dy)			# Left axis.
	call gax_start (xb, yb)
	call gax_draw (xb, y1)
	call gax_tick (xticklen, 0.)

	for (y=y1+dy;  nticks > 0;  nticks=nticks-1) {
	    call gax_draw (xb, min(y,ye))
	    call gax_tick (xticklen, 0.)
	    y = y + dy
	}

	nticks = int ((xe - xb) / dx)			# Top axis.
	call gax_draw (xb, ye)
	call gax_draw (x1, ye)
	call gax_tick (0., -yticklen)

	for (x=x1+dx;  nticks > 0;  nticks=nticks-1) {
	    call gax_draw (min(x,xe), ye)
	    call gax_tick (0., -yticklen)
	    x = x + dx
	}

	call gax_draw (xe, ye)
	call gax_flush (stream)

	# Label the ticks on the bottom axis.  The tick labels are centered
	# just above each tick.

	nticks = int ((xe - xb) / dx) + 1
	call sprintf (tickformat, SZ_TICKFORMAT, "%%0.%dg")
	    call pargi (max (1, gt_ndigits (xb, xe, dx)) + 1)

	for (x=x1;  nticks > 0;  nticks=nticks-1) {
	    call glb_encode (x, ticklabel, SZ_TICKLABEL, tickformat, dx)
	    call gax_ndc (x, yb, sx, sy)
	    call gax_text (stream, sx, sy + (yticklen * LABELOFFSET),
		ticklabel, GT_CENTER, GT_BOTTOM)
	    x = x + dx
	}

	# Label the ticks on the left axis.  The tick labels are left justified
	# just to the right of each tick.

	nticks = int ((ye - yb) / dy) + 1
	call sprintf (tickformat, SZ_TICKFORMAT, "%%0.%dg")
	    call pargi (max (1, gt_ndigits (yb, ye, dy)) + 1)

	for (y=y1;  nticks > 0;  nticks=nticks-1) {
	    call glb_encode (y, ticklabel, SZ_TICKLABEL, tickformat, dy)
	    call gax_ndc (xb, y, sx, sy)
	    call gax_text (stream, sx + (xticklen * LABELOFFSET), sy,
		ticklabel, GT_LEFT, GT_CENTER)
	    y = y + dy
	}

	# Restore the default polyline attributes.
	PL_LTYPE(ap) = lt_save
	PL_WIDTH(ap) = lw_save

	# Move the axes drawing and labelling instructions to the scratch
	# buffer and fix up the frame buffer pointers.

	nwords = TR_OP(tr) - save_op
	if (nwords > TR_LENSCRATCHBUF(tr)) {
	    call realloc (TR_SCRATCHBUF(tr), nwords, TY_SHORT)
	    TR_LENSCRATCHBUF(tr) = nwords
	}

	call amovs (Mems[save_op], Mems[TR_SCRATCHBUF(tr)], nwords)
	TR_OPSB(tr) = TR_SCRATCHBUF(tr) + nwords
	TR_OP(tr) = save_op
	TR_IP(tr) = save_op
	TR_LASTOP(tr) = save_op
end


# GAX_FINDTICKS -- Get the coordinates of the endpoints of the axes, the first
# tick on each axis, and the tick spacing on each axis.  If log scaling is in
# use on an axis we shall work in log coordinate units, which are linear.

procedure gax_findticks (w, wx1,wx2,wy1,wy2, x1,dx,xt, y1,dy,yt)

pointer	w			# window descriptor
real	wx1,wx2,wy1,wy2		# endpoints of axes
real	x1,dx			# tick start and spacing in X
int	xt			# type of scaling in X
real	y1,dy			# tick start and spacing in Y
int	yt			# type of scaling in Y

pointer	wp
real	ct[LEN_CT]
common	/ftkgcm/ wp, ct

real	sx1, sx2, sy1, sy2
real	elogr()

begin
	wp = w

	# Set up WCS/NDC coordinate transformations.
	call grc_settran (w, ct)

	# Get NDC coords of the corners of the screen.
	call grc_scrtondc (0.001, 0.001, sx1, sy1)
	call grc_scrtondc (0.999, 0.999, sx2, sy2)

	# Move in a bit if the graphics viewport lies within the screen area.
	# This depends upon the workstation transformation, of course.
	sx1 = max (WCS_SX1(w), sx1)
	sx2 = min (WCS_SX2(w), sx2)
	sy1 = max (WCS_SY1(w), sy1)
	sy2 = min (WCS_SY2(w), sy2)

	# Compute world coordinates of the viewport (of the axes to be drawn).
	call grc_ndctowcs (ct, sx1, sy1, wx1, wy1)
	call grc_ndctowcs (ct, sx2, sy2, wx2, wy2)

	# Find the ticks.  If log scaling is in use on an axis we shall find
	# and draw the ticks in log coordinates.

	switch (WCS_XTRAN(w)) {
	case GW_LOG:
	    wx1 = log10 (wx1)
	    wx2 = log10 (wx2)
	case GW_ELOG:
	    wx1 = elogr (wx1)
	    wx2 = elogr (wx2)
	}
	call gtickr (wx1, wx2, NTICKS, NO, x1, dx)

	switch (WCS_YTRAN(w)) {
	case GW_LOG:
	    wy1 = log10 (wy1)
	    wy2 = log10 (wy2)
	case GW_ELOG:
	    wy1 = elogr (wy1)
	    wy2 = elogr (wy2)
	}
	call gtickr (wy1, wy2, NTICKS, NO, y1, dy)

	xt = WCS_XTRAN(w)
	yt = WCS_YTRAN(w)
end


# GAX_NDC -- Convert a pair of world or log-world coordinates to NDC
# coordinates.  GAX_FINDTICKS must be called first to set up transformation.

procedure gax_ndc (wx, wy, sx, sy)

real	wx, wy			# world coords (input)
real	sx, sy			# ndc coords (output)

pointer	wp
real	ct[LEN_CT]
common	/ftkgcm/ wp, ct

real	x, y
real	aelogr()

begin
	# Get X in world coordinates.
	switch (WCS_XTRAN(wp)) {
	case GW_LOG:
	    x = 10.0 ** wx
	case GW_ELOG:
	    x = aelogr (wx)
	default:
	    x = wx
	}

	# Get Y in world coordinates.
	switch (WCS_YTRAN(wp)) {
	case GW_LOG:
	    y = 10.0 ** wy
	case GW_ELOG:
	    y = aelogr (wy)
	default:
	    y = wy
	}

	# Transform to NDC coordinates and return.
	call grc_wcstondc (ct, x, y, sx, sy)
end


# GAX_DRAW -- Add a point to the output polyline for an axis.  The polyline
# is built up in NDC coordinates for output to GTR_POLYLINE.  In addition to
# the draw routine, entry points are provided for start, flush, and tick
# drawing.

procedure gax_draw (wx, wy)

real	wx, wy			# world or log-world coords to draw to
real	sx, sy
pointer	polyline, op
common	/gaxdcm/ polyline, op

begin
	# Transform to NDC coords and add the point to the polyline.
	call gax_ndc (wx, wy, sx, sy)
	Memr[op] = sx
	op = op + 1
	Memr[op] = sy
	op = op + 1
end


# GAX_TICK -- Draw a tick at the current position.  The offsets to draw the
# tick are given in NDC coordinates.

procedure gax_tick (dx, dy)

real	dx, dy			# tick offset in NDC coords for gax_tick
real	x, y
pointer	polyline, op
common	/gaxdcm/ polyline, op

begin
	x = Memr[op-2]
	y = Memr[op-1]

	Memr[op] = x + dx
	op = op + 1
	Memr[op] = y + dy
	op = op + 1

	Memr[op] = x
	op = op + 1
	Memr[op] = y
	op = op + 1
end


# GAX_START -- Start a new polyline at the indicated point in world coords.
# The polyline buffer is of a fixed length with no bounds checking.

procedure gax_start (wx, wy)

real	wx, wy			# world or log-world coords to draw to
pointer	polyline, op

errchk	malloc
common	/gaxdcm/ polyline, op

begin
	call malloc (polyline, LEN_POLYLINE, TY_REAL)
	op = polyline
	call gax_draw (wx, wy)
end


# GAX_FLUSH -- Flush the buffered polyline and free space on the heap.

procedure gax_flush (stream)

int	stream			# graphics stream
pointer	polyline, op
common	/gaxdcm/ polyline, op

begin
	call grc_polyline (stream, Memr[polyline], (op - polyline) / 2)
	call mfree (polyline, TY_REAL)
end


# GAX_TEXT -- Draw a text string (tick label) of size 1.0 with the indicated
# justification.

procedure gax_text (stream, sx, sy, text, hjustify, vjustify)

int	stream			# graphics stream
real	sx, sy			# text coordinates, NDC
char	text[ARB]		# text string to be drawn
int	hjustify		# horizontal justification
int	vjustify		# vertical justification

pointer	tr, tx
int	save_tx[LEN_TX]
errchk	gtr_init
pointer	gtr_init()

begin
	tr = gtr_init (stream)
	tx = TR_TXAP(tr)
	call amovi (Memi[tx], save_tx, LEN_TX)

	TX_UP(tx) = 90
	TX_SIZE(tx) = 1.0
	TX_PATH(tx) = GT_RIGHT
	TX_SPACING(tx) = 0
	TX_HJUSTIFY(tx) = hjustify
	TX_VJUSTIFY(tx) = vjustify
	TX_FONT(tx) = GT_BOLD
	TX_QUALITY(tx) = GT_NORMAL
	TX_COLOR(tx) = 1

	call grc_text (stream, sx, sy, text)
	call amovi (save_tx, Memi[tx], LEN_TX)
end
