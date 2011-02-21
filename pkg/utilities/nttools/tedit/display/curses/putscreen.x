include	"../curses.h"

# PUTSCREEN -- Copy buffer back onto screen
#
# B.Simon	27-Sep-90	Original

procedure putscreen (source, buffer)

int	source[RSIZE]	# i: Rectangle to be restored
pointer	buffer		# i: Buffer 
#--

bool	clear
int	blank, dest[RSIZE]
int	maxcol, maxrow, ncols, nrows, icol, jcol, irow
pointer	sp, buf, ptr, scr

data	blank	/ ' ' /

bool	ps_intersect()
int	ps_width(), ps_height()
pointer	ps_screen()

begin
	# Clip rectangle at screen boundary

	maxcol = ps_width ()
	maxrow = ps_height ()
	if (! ps_intersect (source, maxrow, maxcol, dest))
	    return

	call smark (sp)
	ncols = RWIDTH(dest)
	nrows = RHEIGHT(dest)

	# If the buffer pointer is null, 
	# copy the current screen contents instead

	if (buffer != NULL) {
	    buf = buffer
	} else {
	    call salloc (buf, nrows*ncols, TY_CHAR)

	    ptr = buf
	    scr = ps_screen (RTOP(dest), RLEFT(dest))
	    do irow = 1, nrows {
		call amovc (Memc[scr], Memc[ptr], ncols)
		scr = scr + maxcol
		ptr = ptr + ncols
	    }
	}

	# See if clearing the screen first would be faster 
	# if so, do it

	clear = ncols == maxcol
	if (clear)
	    call ps_fill (dest, blank, A_NORM)

	# Copy buffer to screen using ps_wrtcells

	do irow = RTOP(dest), RBOT(dest) {
	    icol = 1
	    jcol = ncols

	    # If the screen has been cleared, don't write blanks

	    if (clear) {
		while (icol <= ncols && Memc[buf+icol-1] == blank)
		    icol = icol + 1
		while (jcol >= icol && Memc[buf+jcol-1] == blank)
		     jcol = jcol - 1
	    }

	    if (jcol >= icol) {
		call ps_wrtcells (irow, RLEFT(dest)+icol-1, 
				  Memc[buf+icol-1], jcol-icol+1)
	    }

	    buf = buf + ncols
	}

	call sfree (sp)
end
