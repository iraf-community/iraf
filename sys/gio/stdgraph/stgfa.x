# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_FILLAREA -- Fill a closed area.  The area is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs outlining the
# area to be filled.

procedure stg_fillarea (p, npts)

short	p[ARB]			#I points defining area outline
int	npts			#I number of points, i.e., (x,y) pairs

pointer	fa
bool	tek_encoding
int	lowres_x, lowres_y
int	ip, n, sx, sy, len_p, iop, i
int	stg_encode()
include	"stdgraph.com"

begin
	if (g_enable == NO)
	    call stg_genab()

	len_p = npts * 2

	# Update fillarea attributes if necessary.

	fa = SG_FAAP(g_sg)
	if (SG_COLOR(g_sg) != FA_COLOR(fa)) {
	    call stg_ctrl1 ("FC", FA_COLOR(fa))
	    SG_COLOR(g_sg) = FA_COLOR(fa)
	}
	if (SG_FASTYLE(g_sg) != FA_STYLE(fa)) {
	    call stg_ctrl1 ("FT", FA_STYLE(fa))
	    SG_FASTYLE(g_sg) = FA_STYLE(fa)
	}

	# Tektronix encoding is treated as a special case for max efficiency.
	tek_encoding =
	    (Memc[g_xy] == '%' && Memc[g_xy+1] == 't' && Memc[g_xy+2] == EOS)

	# Draw the fillarea.  If the startfill sequence is defined we assume
	# that the device can draw a multipoint fillarea.

	if (Memc[SG_STARTFILL(g_sg)] != EOS) {
	    for (ip=1;  ip <= len_p;  ip=ip+2) {
		# Output start fillarea sequence.
		call ttyputs (g_out, g_tty, Memc[SG_STARTFILL(g_sg)], 1)
		n = len_p

		# Encode the points of the fillarea outline (or move to the
		# single point to be drawn).

		g_lastx = -1	# clip unresolved points only in the interior
		g_lasty = -1	# of the area being drawn.

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
		call ttyputs (g_out, g_tty, Memc[SG_ENDFILL(g_sg)], 1)
	    }
	} else {
	    # If can't do a fill area, just draw the area outline.
	    call stg_polyline (p, npts)
	}
end
