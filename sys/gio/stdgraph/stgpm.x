# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_POLYMARKER -- Draw a polymarker.  The polymarker is defined by the array
# of points P, consisting of successive (x,y) coordinate pairs, each of which
# is to be plotted as a point.  If the marker start sequence MS is defined the
# polymarker will be drawn as <markstart> <p1> ... <pN> <markend>, otherwise
# ther marker is draw using the polyline move and draw commands to draw each
# individual point.

procedure stg_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pm
bool	tek_encoding
int	lowres_x, lowres_y
int	ip, n, sx, sy, len_p, iop, i
int	stg_encode()
include	"stdgraph.com"

begin
	if (g_enable == NO)
	    call stg_genab()

	len_p = npts * 2

	# Update polymarker attributes if necessary.

	pm = SG_PMAP(g_sg)
	if (SG_COLOR(g_sg) != PM_COLOR(pm)) {
	    call stg_ctrl1 ("MC", PM_COLOR(pm))
	    SG_COLOR(g_sg) = PM_COLOR(pm)
	}

	# Tektronix encoding is treated as a special case for max efficiency.
	tek_encoding =
	    (Memc[g_xy] == '%' && Memc[g_xy+1] == 't' && Memc[g_xy+2] == EOS)

	# Draw the polymarker.  If the startmark sequence is defined we assume
	# that the device can draw a multipoint polymarker, else low level move
	# and draw sequences are used.

	if (Memc[SG_STARTMARK(g_sg)] != EOS) {
	    for (ip=1;  ip <= len_p;  ip=ip+2) {
		# Output start marker sequence [revised to use the encoder].
		call ttyputs (g_out, g_tty, Memc[SG_STARTMARK(g_sg)], 1)
		n = len_p

		# Encode the points of the polymarker (or move to the single
		# point to be drawn).

		g_lastx = -1	# clip unresolved points only in the interior
		g_lasty = -1	# of the polymarker being drawn.

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

		    # Encode the point, appending encoded bytes to g_mem.
		    # Tek encoding is treated as a special case since it is
		    # so common; the encoder is used for non-Tek encodings.

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

		# Output end polymarker sequence, or draw the point.
		call ttyputs (g_out, g_tty, Memc[SG_ENDMARK(g_sg)], 1)
	    }
	} else {
	    for (ip=1;  ip <= len_p;  ip=ip+2) {
		sx = p[ip];  sy = p[ip+1]
		call stg_move (sx, sy)
		call stg_draw (sx, sy)
	    }
	}
end
