include "../curses.h"
include	"formfn.h"

# FM_GETFORM -- Get user input from a form
#
# B.Simon	27-Jan-89	Original
# B.Simon	12-Dec-90	Rewritten to use curses

procedure fm_getform (win, redraw, lenvalue, fvalue)

int	win			# i: Window descriptor
bool	redraw			# i: Redraw screen?
int	lenvalue		# i: Declared length of field value
char	fvalue[lenvalue,ARB]	# u: Values in fields
#--
bool	draw
int	ifield, curfield, topfield, botfield, ch
pointer	data, value

bool	fm_check()
int	k_get()

begin
	# Get pointer to form data structure

	call wgetstruct (win, data)

	# Initialize data structure

	FM_FIELD(data) = 1
	FM_CHANGE(data) = NO

	# Copy values to data structure

	value = FM_VALARY(data)
	do ifield = 1, FM_NFIELD(data) {
	    call strcpy (fvalue[1,ifield], Memc[value], lenvalue)
	    value = value + FM_LENVAL(data) + 1
	}

	# Let user update form

	draw = redraw

	curfield = 1
	topfield = 1
	botfield = min (FM_NFIELD(data), topfield + FM_NPAGE(data) - 1)

	repeat {

	    # Redraw form and move cursor

	    if (draw) {
		topfield = max (1, curfield - FM_NPAGE(data) / 2)
		botfield = min (FM_NFIELD(data), topfield + FM_NPAGE(data) - 1)
		call fm_redraw (win, topfield)
	    }

	    call wmove (win, curfield-topfield+1, FM_LENNAM(data)+4)

	    value = FM_VALPTR(data,curfield)
	    call weditstr (win, Memc[value], FM_LENVAL(data))

	    ch = k_get ()

	    if (FM_CHANGE(data) == YES) {
		if (! fm_check (FM_TYPE(data,curfield), 
				Memc[FM_VALPTR(data,curfield)])) {
		    call ps_beep
		    ch = EOS
		    next
		}
	    }

	    curfield = FM_FIELD(data)
	    FM_CHANGE(data) = NO
	    draw = curfield < topfield || curfield > botfield

	} until (ch == K_EXIT)

	# Copy form values to output array

	value = FM_VALARY(data)
	do ifield = 1, FM_NFIELD(data) {
	    call strcpy (Memc[value], fvalue[1,ifield], lenvalue)
	    value = value + FM_LENVAL(data) + 1
	}

end
