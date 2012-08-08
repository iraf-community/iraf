# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

define	MOVE		0
define	DRAW		1
define	LEFT		0
define	RIGHT		1
define	BELOW		0
define	ABOVE		1
define	INSIDE		2
define	FIRSTPT		GKI_POLYLINE_P


# GTR_WSTRAN -- Apply the workstation transformation to an instruction and
# write the transformed instruction to the graphics kernel.  The transformation
# parameters etc. should have been initialized in the gtr common before we
# are called.

procedure gtr_wstran (gki)

short	gki[ARB]		#I metacode instruction to be spooled

long	x, y
pointer	sp, buf
int	length, npts, data
int	gtr_polyclip()
bool	sge_wsenable()
include	"gtr.com"

begin
	# Check with the graphics kernel to see if scaling of graphics
	# instructions is enabled (it is disabled if the graphics device is
	# already doing it for us).

	if (!sge_wsenable()) {
	    call gki_write (tr_stream, gki)
	    return
	}

	switch (gki[GKI_HDR_OPCODE]) {
	case GKI_FILLAREA:
	    npts = gki[GKI_FILLAREA_N]
	    data = GKI_FILLAREA_P
	    length = gki[GKI_HDR_LENGTH]
	    call amovs (gki, pl, length)

	    switch (gtr_polyclip (pl[data], npts, mx1, mx2, my1, my2)) {
	    case 0:
		# Entire instruction out of bounds.
	    case 1:
		# Entire instruction in bounds.
		pl_op = GKI_POLYLINE_P + npts * 2
		call gpt_flush()
	    default:
		# Instruction has been clipped.
		pl_op = GKI_POLYLINE_P + npts * 2
		call gpt_flush()
	    }

	case GKI_POLYLINE, GKI_POLYMARKER:
	    call gtr_polytran (gki)

	case GKI_SETCURSOR:
	    length = gki[GKI_HDR_LENGTH]
	    call smark (sp)
	    call salloc (buf, length, TY_SHORT)

	    # Move cursor to edge of screen if point referenced is out of
	    # bounds.

	    call amovs (gki, Mems[buf], length)
	    x = gki[GKI_SETCURSOR_POS]
	    y = gki[GKI_SETCURSOR_POS+1]
	    call gtr_ctran (x, y, x, y)
	    Mems[buf+GKI_SETCURSOR_POS-1] = x
	    Mems[buf+GKI_SETCURSOR_POS]   = y
	    call gki_write (tr_stream, Mems[buf])

	    call sfree (sp)

	case GKI_TEXT:
	    length = gki[GKI_HDR_LENGTH]
	    call smark (sp)
	    call salloc (buf, length, TY_SHORT)

	    # Discard text drawing instruction if the point referenced is
	    # out of bounds.  If in bounds, transform coordinates and draw
	    # at the transformed point.

	    call amovs (gki, Mems[buf], length)
	    x = gki[GKI_TEXT_P]
	    y = gki[GKI_TEXT_P+1]
	    if (x >= mx1 && x <= mx2 && y >= my1 && y <= my2) {
		call gtr_ctran (x, y, x, y)
		Mems[buf+GKI_TEXT_P-1] = x
		Mems[buf+GKI_TEXT_P]   = y
		call gki_write (tr_stream, Mems[buf])
	    }

	    call sfree (sp)

	case GKI_PUTCELLARRAY:
	    # Just filter these out for now.

	default:
	    call gki_write (tr_stream, gki)
	}
end


# GTR_CTRAN -- Apply the workstation transform to a set of GKI coordinates,
# i.e., transform raw GKI coords to screen coords in GKI units.

procedure gtr_ctran (mx, my, sx, sy)

int	mx, my			# raw GKI coordinates
int	sx, sy			# screen coordinates in GKI units
include	"gtr.com"

begin
	sx = max(0, min(GKI_MAXNDC, nint ((mx - mx1) * xscale + xorigin)))
	sy = max(0, min(GKI_MAXNDC, nint ((my - my1) * yscale + yorigin)))
end


# GTR_POLYTRAN -- Scale a polyline, polymarker, or fill area instruction
# by applying the workstation transformation.  The workstation transformation
# scales vectors in a viewport defined in NDC(GKI) space to fit the full
# device screen.  Vectors or segments of vectors lying outside the viewport
# are clipped at the screen boundary.

procedure gtr_polytran (gki)

short	gki[ARB]		# gki instruction to be transformed
long	mx, my
int	last_ip, opcode, i, ip
bool	inbounds, otherside, points
int	gpt_firstpt()
include	"gtr.com"

begin
	last_ip = gki[GKI_HDR_LENGTH]
	opcode  = gki[GKI_HDR_OPCODE]
	points  = (opcode == GKI_POLYMARKER)

	# In the process of clipping a polyline may be broken into several
	# smaller polylines (or polymarkers or fillareas, all of which are
	# very similar at the instruction level).  We store the GKI header
	# in the first few words of the PL array so that when the transformed
	# polyline is broken it is ready for execution.

	do i = 1, GKI_POLYLINE_P - 1
	    pl[i] = gki[i]
	pl_op = GKI_POLYLINE_P

	# Clip all points until either a point is encountered which is inbounds
	# or which is on the other side of the viewport (in either axis).  This
	# is a fast way of clipping polylines which are mostly out of bounds.
	# Return immediately if the entire vector is out of bounds.

	otherside = true
	ip = FIRSTPT
	if (gpt_firstpt (gki, ip, last_ip) <= 0)
	    return

	# Set initial position.
	cx = gki[ip]
	cy = gki[ip+1]

	# Clip the remaining points.  Clipping is performed in GKI coordinates.
	# The workstation transformation is not applied until the clipped
	# vector is output.

	for (ip=ip+2;  ip < last_ip;  ip=ip+2) {
	    mx = gki[ip]
	    my = gki[ip+1]

	    # Check to see if this is the first point of a new polyline.
	    # If so we must set the first physical point in the output
	    # polyline to the current position, making the current point
	    # the second physical point of the output polyline.

	    if (pl_op <= GKI_POLYLINE_P) {
		# Place the current pen position in the polyline as the
		# first point if it is inbounds.

		if (cy <= my2 && cy >= my1 && cx <= mx2 && cx >= mx1) {
		    last_point_inbounds = true
		    pl[pl_op] = cx
		    pl_op = pl_op + 1
		    pl[pl_op] = cy
		    pl_op = pl_op + 1
		} else {
		    last_point_inbounds = false
		    do i = 1, 4 {
			xs[i] = cx
			ys[i] = cy
		    }
		}
	    }

	    # Update the current position.

	    cx = mx
	    cy = my

	    # Clip at the edge of the device screen.

	    inbounds = (my <= my2 && my >= my1 && mx <= mx2 && mx >= mx1)

	    if (inbounds && (last_point_inbounds || points)) {
		# Add point to polyline (the fast way).
		pl[pl_op] = mx
		pl_op = pl_op + 1
		pl[pl_op] = my
		pl_op = pl_op + 1

	    } else if ((inbounds||last_point_inbounds||otherside) && !points) {
		# Clip at viewport boundary.

		if (last_point_inbounds) {
		    # Update coords of last point drawn (necessary since we did
		    # not use the clipping code for inbounds points).
		    do i = 1, 4 {
			xs[i] = pl[pl_op-2]
			ys[i] = pl[pl_op-1]
		    }
		}
		call gpt_clipl (DRAW, mx, my)
		otherside = false

	    } else {
		# Both points are out of bounds.  Scan along until a point is
		# found which is again in bounds, or which is on the other side
		# of the viewport, requiring clipping across the viewport.

		if (gpt_firstpt (gki, ip, last_ip) > 0) {
		    do i = 1, 4 {
			xs[i] = gki[ip]
			ys[i] = gki[ip+1]
		    }
		    cx = gki[ip]
		    cy = gki[ip+1]
		}

		otherside = true
		inbounds = false
	    }

	    last_point_inbounds = inbounds
	}

	call gpt_flush()
end


# GPT_FIRSTPT -- Scan a vector and return the index of the next good point.
# A good point is a point which is either inbounds or which preceeds a point
# which is either inbounds or on the other side of the viewport, necessitating
# clipping across the viewport.

int procedure gpt_firstpt (gki, ip, last_ip)

short	gki[ARB]			# vector being clipped
int	last_ip				# last legal value of ip
int	ip				# starting index

int	mx, my, i
int	first_ip, new_ip
include	"gtr.com"

begin
	mx = gki[ip]
	my = gki[ip+1]
	first_ip = ip
	new_ip = last_ip

	if (mx < mx1) {
	    do i=ip+2, last_ip, 2
		if (gki[i] >= mx1) {
		    new_ip = i
		    break
		}
	} else if (mx > mx2) {
	    do i=ip+2, last_ip, 2
		if (gki[i] <= mx2) {
		    new_ip = i
		    break
		}
	} else if (my < my1) {
	    do i=ip+3, last_ip, 2
		if (gki[i] >= my1) {
		    new_ip = i - 1
		    break
		}
	} else if (my > my2) {
	    do i=ip+3, last_ip, 2
		if (gki[i] <= my2) {
		    new_ip = i - 1
		    break
		}
	} else
	    return (ip)

	if (new_ip >= last_ip)
	    return (0)			# entire vector is indefinite
	else
	    ip = max (first_ip, new_ip - 2)

	return (ip)
end


# GPT_CLIPL -- Clip at left boundary.

procedure gpt_clipl (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
long	new_my
int	newpen
include	"gtr.com"

begin
	# Does line cross boundary?
	if ((mx >= mx1 && xs[1] < mx1) || (mx <= mx1 && xs[1] > mx1)) {
	    if (mx >= mx1)
		newpen = MOVE
	    else
		newpen = pen
	    new_my = real(my - ys[1]) * real(mx1 - mx) / real(mx - xs[1]) +
		my + 0.5
	    call gpt_clipr (newpen, mx1, new_my)
	}

	xs[1] = mx
	ys[1] = my

	if (mx >= mx1)
	    call gpt_clipr (pen, mx, my)
end


# GPT_CLIPR -- Clip at right boundary.

procedure gpt_clipr (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
long	new_my
int	newpen
include	"gtr.com"

begin
	# Does line cross boundary?
	if ((mx <= mx2 && xs[2] > mx2) || (mx >= mx2 && xs[2] < mx2)) {
	    if (mx <= mx2)
		newpen = MOVE
	    else
		newpen = pen
	    new_my = real(my - ys[2]) * real(mx2 - mx) / real(mx - xs[2]) +
		my + 0.5
	    call gpt_clipb (newpen, mx2, new_my)
	}

	xs[2] = mx
	ys[2] = my

	if (mx <= mx2)
	    call gpt_clipb (pen, mx, my)
end


# GPT_CLIPB -- Clip at bottom boundary.

procedure gpt_clipb (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
long	new_mx
int	newpen
include	"gtr.com"

begin
	# Does line cross boundary?
	if ((my >= my1 && ys[3] < my1) || (my <= my1 && ys[3] > my1)) {
	    if (my >= my1)
		newpen = MOVE
	    else
		newpen = pen
	    new_mx = real(mx - xs[3]) * real(my1 - my) / real(my - ys[3]) +
		mx + 0.5
	    call gpt_clipt (newpen, new_mx, my1)
	}

	xs[3] = mx
	ys[3] = my

	if (my >= my1)
	    call gpt_clipt (pen, mx, my)
end


# GPT_CLIPT -- Clip at top boundary and put the final clipped point(s) in
# the output polyline.  Note that a "move" at this level does not affect
# the current position (cx,cy), since the vector endpoints have been clipped
# and the current position vector follows the unclipped vector points input
# by the user.

procedure gpt_clipt (pen, mx, my)

int	pen			# move or draw
long	mx, my			# point to be clipped
include	"gtr.com"

begin
	# Does line cross boundary?
	if ((my <= my2 && ys[4] > my2) || (my >= my2 && ys[4] < my2)) {
	    if (my <= my2 || pen == MOVE)
		call gpt_flush()
	    pl[pl_op] = real(mx - xs[4]) * real(my2 - my) / real(my - ys[4]) +
		mx + 0.5
	    pl_op = pl_op + 1
	    pl[pl_op] = my2
	    pl_op = pl_op + 1
	}

	xs[4] = mx
	ys[4] = my

	if (my <= my2) {
	    if (pen == MOVE)
		call gpt_flush()
	    pl[pl_op] = mx
	    pl_op = pl_op + 1
	    pl[pl_op] = my
	    pl_op = pl_op + 1
	}
end


# GPT_FLUSH -- Flush the buffered "polyline", i.e., array of transformed and
# clipped points.  For a polyline or fill area polygon there must be at least
# two points (4 cells) or it will be discarded.  A single point polymarker is
# permitted.

procedure gpt_flush()

int	npts, i
long	mx, my
include	"gtr.com"

begin
	if (pl_op >= GKI_POLYLINE_P + 2) {
	    npts = (pl_op - GKI_POLYLINE_P) / 2

	    # Apply the workstation transformation.
	    do i = GKI_POLYLINE_P, pl_op, 2 {
		mx = nint ((pl[i]   - mx1) * xscale + xorigin)
		my = nint ((pl[i+1] - my1) * yscale + yorigin)
		pl[i]   = max(0, min(GKI_MAXNDC, mx))
		pl[i+1] = max(0, min(GKI_MAXNDC, my))
	    }

	    switch (pl[GKI_HDR_OPCODE]) {
	    case GKI_POLYMARKER:
		pl[GKI_POLYMARKER_L] = pl_op - 1
		pl[GKI_POLYMARKER_N] = npts
		call gki_write (tr_stream, pl)

	    case GKI_FILLAREA:
		pl[GKI_FILLAREA_L] = pl_op - 1
		pl[GKI_FILLAREA_N] = npts
		call gki_write (tr_stream, pl)

	    default:
		if (npts >= 2) {
		    pl[GKI_POLYLINE_L] = pl_op - 1
		    pl[GKI_POLYLINE_N] = npts
		    call gki_write (tr_stream, pl)
		}
	    }

	    pl_op = GKI_POLYLINE_P
	}
end
