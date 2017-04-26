include	"formfn.h"
define	SZ_ESCSEQ	20

# FM_REDRAW -- Redraw the form

procedure fm_redraw (win, topfield)

int	win			# i: Window descriptor
int	topfield		# i: Top field on the form to draw
#--
int	nrows, ncols, botfield, ifield
pointer	sp, field, footer, escseq, data, name, value

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (field, SZ_LINE, TY_CHAR)
	call salloc (footer, SZ_LINE, TY_CHAR)
	call salloc (escseq, SZ_ESCSEQ, TY_CHAR)

	# Get form data structure

	call wgetstruct (win, data)

	# Get the dimensions of the form

	call wdimen (win, nrows, ncols)

	# Clear screen

	call werase (win)

	# Write the form

	name = FM_NAMPTR(data,topfield)
	value = FM_VALPTR(data,topfield)
	botfield = min (FM_NFIELD(data), topfield + FM_NPAGE(data) - 1)

	call wmove (win, 1, 1)
	do ifield = topfield, botfield {
	    call sprintf (Memc[field], SZ_LINE, "%*s = %s\n")
		call pargi (FM_LENNAM(data))
		call pargstr (Memc[name])
		call pargstr (Memc[value])

	    call waddstr (win, Memc[field])

	    name = name + FM_LENNAM(data) + 1
	    value = value + FM_LENVAL(data) + 1
	}

	# Write the footer line

	call k_eseq ("GET_HELP", Memc[escseq], SZ_ESCSEQ)

	call sprintf (Memc[footer], ncols, "%4w%s%*tHelp: %s%*t")
	call pargstr (FM_TITLE(data))
	call pargi (ncols-SZ_ESCSEQ)
	call pargstr (Memc[escseq])
	call pargi (ncols+1)

	call wmove (win, nrows, 1)
	call wstandout (win)
	call waddstr (win, Memc[footer])
	call wstandend (win)

	call sfree (sp)
end
