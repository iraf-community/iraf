# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_POLYLINE -- Draw a polyline.  The polyline is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted but rather defines the start of the polyline.  The remaining
# points define line segments to be drawn.

procedure stg_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pl
bool	tek_encoding
int	lowres_x, lowres_y
int	ip, n, sx, sy, len_p, iop, i
int	stg_encode()
include	"stdgraph.com"

begin
	if (g_enable == NO)
	    call stg_genab()

	len_p = npts * 2

	# Update polyline attributes if necessary.

	pl = SG_PLAP(g_sg)
	if (SG_PLTYPE(g_sg) != PL_LTYPE(pl)) {
	    call stg_ctrl1 ("LT", PL_LTYPE(pl))
	    SG_PLTYPE(g_sg) = PL_LTYPE(pl)
	}
	if (SG_PLWIDTH(g_sg) != PL_WIDTH(pl)) {
	    call stg_ctrl1 ("LW", PL_WIDTH(pl))
	    SG_PLWIDTH(g_sg) = PL_WIDTH(pl)
	}
	if (SG_COLOR(g_sg) != PL_COLOR(pl)) {
	    call stg_ctrl1 ("LC", PL_COLOR(pl))
	    SG_COLOR(g_sg) = PL_COLOR(pl)
	}

	# Transform the first point from GKI coords to device coords and
	# move to the transformed point.

	sx = p[1]; sy = p[2]
	call stg_move (sx, sy)

	# Tektronix encoding is treated as a special case for max efficiency.
	tek_encoding =
	    (Memc[g_xy] == '%' && Memc[g_xy+1] == 't' && Memc[g_xy+2] == EOS)

	# Draw the polyline.  If the device has the "polyline" capability
	# we can encode and output successive points without enclosing each
	# individual point in the startdraw and enddraw strings.

	for (ip=3;  ip <= len_p;  ip=ip+2) {
	    # Output start draw sequence.
	    call ttyputs (g_out, g_tty, Memc[SG_STARTDRAW(g_sg)], 1)

	    # Determine number of points to output.
	    if (SG_POLYLINE(g_sg) == YES)
		n = len_p
	    else
		n = ip + 2

	    # Encode the points of the polyline.

	    g_lastx = -1	# clip unresolved points only in the interior
	    g_lasty = -1	# of the polyline being drawn.

	    g_reg[E_IOP] = 1
	    do i = ip, n, 2 {
		sx = p[i]
		sy = p[i+1]

		# Discard the point if it is not resolved.
		lowres_x = sx / g_dxres
		lowres_y = sy / g_dyres
		if (lowres_x == g_lastx && lowres_y == g_lasty)
		    next

		g_lastx = lowres_x
		g_lasty = lowres_y

		# Transform point into the device window.
		sx = int (sx * g_dx) + g_x1
		sy = int (sy * g_dy) + g_y1

		# Encode the point, appending encoded bytes to g_mem.  Tek
		# encoding is treated as a special case since it is so common;
		# the encoder is used for non-Tek encodings.

		if (tek_encoding) {
		    iop = g_reg[E_IOP] + 4
		    g_mem[iop-4] = g_hixy[sy+1]
		    g_mem[iop-3] = g_loy[sy+1]
		    g_mem[iop-2] = g_hixy[sx+1]
		    g_mem[iop-1] = g_lox[sx+1]
		    g_reg[E_IOP] = iop
		} else {
		    g_reg[1] = sx
		    g_reg[2] = sy
		    if (stg_encode (Memc[g_xy], g_mem, g_reg) != OK)
			break
		}

		# Flush buffer if nearly full.
		if (g_reg[E_IOP] > FLUSH_MEMORY) {
		    call write (g_out, g_mem, g_reg[E_IOP] - 1)
		    g_reg[E_IOP] = 1
		}
	    }
	    ip = n

	    # Flush any output remaining in encoder memory.
	    if (g_reg[E_IOP] > 1) {
		call write (g_out, g_mem, g_reg[E_IOP] - 1)
		g_reg[E_IOP] = 1
	    }

	    # Output end draw sequence.
	    call ttyputs (g_out, g_tty, Memc[SG_ENDDRAW(g_sg)], 1)
	}
end
