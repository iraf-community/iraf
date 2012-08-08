include <tbset.h>
include <lexnum.h>
include	<mach.h>
include <ctype.h>
include	"display/curses.h"
include "screen.h"
include "table.h"
include "field.h"

# FIELD -- Procedures which manipulate a single field (table element)

# ADD_FIELD -- Add a new field to the table and screen

procedure add_field (scr, tabrow, tabcol)

pointer	scr		# u: screen descriptor
int	tabrow		# i: new table row
int	tabcol		# i: new table column
#--
char	blank
int	win, nrows, ncols, row, col, irow, icol, junk, hitab, ic
pointer	tab, tptr, sp, line, temp

data	blank	/ ' ' /

int	tbpsta(), itoc()

begin
	# Get window and table pointers from screen descriptor

	win = TED_WINDOW(scr)
	tab = TED_TABLE(scr)
	tptr = TED_TABPTR(tab)

	# Get dimensions of window and table

	call wdimen (win, nrows, ncols)
	call pos_field (scr, 1, row, col)
	hitab = tbpsta (tptr, TBL_NROWS)

	# Add new row to table

	call tbtwer (tptr, tabrow)
	TED_DIRTY(tab) = YES

	# Update current row and column

	TED_CURROW(scr) = tabrow
	TED_CURCOL(scr) = tabcol

	# If new row is off screen, redraw screen

	if (row >= nrows || col >= ncols) {
	    call move_screen (scr, LEFT, YES)
	    return
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (line, ncols+1,TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Copy table elements to text line 
	# and write the line on the screen

	call wmove (win, row+1, 1)
	do irow = hitab+1, tabrow {
	    junk = itoc (irow, Memc[temp], SZ_LINE)
	    call align_field (RIGHT, blank, Memc[temp], 
			      Memc[line], TED_LABWIDTH(tab))

	    ic = TED_LABWIDTH(tab)
	    do icol = TED_LOCOL(scr), TED_HICOL(scr) {
		Memc[line+ic] = ' '
		ic = ic + 1

		call tbegtt (tptr, TED_COLPTR(tab,icol), irow, 
			     Memc[temp], SZ_LINE)

		call align_field (LEFT, blank, Memc[temp], Memc[line+ic], 
				  TED_COLLEN(tab,icol))
		ic = ic + TED_COLLEN(tab,icol)
	    }

	    Memc[line+ic] = '\n'
	    Memc[line+ic+1] = EOS
	    call waddstr (win, Memc[line])
	}

	# Move cursor to current field and refresh window

	TED_HIROW(scr) = tabrow
	call move_field (scr)

	call wrefresh (win)
	call sfree (sp)

end

# ALIGN_FIELD -- Align a string within a larger field

procedure align_field (align, fill, instr, outstr, outlen)

int	align		# i: alignment of output string
char	fill		# i: fill character
char	instr[ARB]	# i: input string
char	outstr[ARB]	# o: output string
int	outlen		# i: length of output string
#--
int	ibeg, iend, inlen, jbeg, jend, ic, jc, kc

begin
	# Get the first and last characters in the string 
	# which are not fill characters

	ibeg = 0
	iend = 0
	for (ic = 1; instr[ic] != EOS; ic = ic + 1) {
	    if (instr[ic] != fill) {
		if (ibeg == 0)
		    ibeg = ic
		iend = ic
	    }
	}
	ibeg = max (ibeg, 1)
	inlen = (iend - ibeg) + 1

	# Check the length of the input string to see 
	# if the alignment problem is trivial

	if (inlen >= outlen) {
	    call strcpy (instr[ibeg], outstr, outlen)
	    return
	}

	# Calculate the number of fill characters to add at
	# the beginning and end of the string

	switch (align) {
	case LEFT:
	    jbeg = 0
	    jend = outlen - inlen

	case CENTER:
	    jbeg = (outlen - inlen) / 2
	    jend = outlen - inlen - jbeg

	case RIGHT:
	    jbeg = outlen - inlen
	    jend = 0
	}

	# Create the output string

	for (jc = 1; jc <= jbeg; jc = jc + 1)
	    outstr[jc] = fill

	for (ic = ibeg; ic <= iend; ic = ic + 1) {
	    outstr[jc] = instr[ic]
	    jc = jc + 1
	}

	for (kc = 1; kc <= jend; kc = kc + 1) {
	    outstr[jc] = fill
	    jc = jc + 1
	}

	outstr[jc] = EOS
end

# CHECK_FIELD -- Check string to see if it has the correct data type

bool procedure check_field (datatype, field)

int	datatype	# i: Datatype to check
char	field[ARB]	# i: String to be checked
#--
bool	match
double	fieldval
int	ic, nc, lextype, fieldtype
pointer	sp, temp

string	yorn  "|yes|no|"

bool	streq()
int	strlen(), lexnum(), ctod(), strdic()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	if (datatype < 0)

	    # The only check on string types is that they not exceed their
	    # maximum length

	    match = strlen (field) <= -(datatype)

	else {

	    # Get the data type of the string
	    # Reduce this to character, integer or real
	    # Get the value of the string if it is not character

	    if (streq (field, "INDEF")) {
		fieldtype = datatype
		fieldval = 0.0

	    } else {
		ic = 1
		lextype = lexnum (field, ic, nc)

		for (ic = ic + nc; IS_WHITE(field[ic]); ic = ic + 1)
		    ;
		if (field[ic] != EOS)
		    lextype = LEX_NONNUM

		if (lextype == LEX_HEX || lextype == LEX_NONNUM) {
		    fieldtype = TY_CHAR
		    fieldval = 0.0
		} else {
		    if (lextype == LEX_REAL)
			fieldtype = TY_REAL
		    else
			fieldtype = TY_INT

		    ic = 1
		    nc = ctod (field, ic, fieldval)
		    fieldval = abs (fieldval)
		}
	    }

	    # See if the string matches the expected datatype

	    switch (datatype) {
	    case TY_BOOL:
		match = strdic (field, Memc[temp], SZ_LINE, yorn) > 0
	    case TY_CHAR:
		match = strlen (field) <= 1
	    case TY_SHORT:
		match = fieldtype == TY_INT && fieldval <= MAX_SHORT
	    case TY_INT:
		match = fieldtype == TY_INT && fieldval <= MAX_INT
	    case TY_LONG:
		match = fieldtype == TY_INT && fieldval <= MAX_LONG
	    case TY_REAL:
		match = fieldtype != TY_CHAR && fieldval <= MAX_REAL
	    case TY_DOUBLE:
		match = fieldtype != TY_CHAR && fieldval <= MAX_DOUBLE
	    default:
		match = true
	    }
	}

	call sfree (sp)
	return (match)
end

# DRAW_FIELD -- Draw a single field on the screen

procedure draw_field (scr)

pointer	scr		# i: screen descriptor
#--
char	blank
pointer	sp, value, field, tab
int	win, tabrow, tabcol, row, col, orow, ocol

data	blank	/ ' ' /

int	winstat()

begin
	# Allocate dynamic memory to hold string

	call smark (sp)
	call salloc (value, SZ_FIELD, TY_CHAR)
	call salloc (field, SZ_FIELD, TY_CHAR)

	# Get current value

	win = TED_WINDOW(scr)
	tab = TED_TABLE(scr)

	tabrow = TED_CURROW(scr)
	tabcol = TED_CURCOL(scr)

	call tbegtt (TED_TABPTR(tab), TED_COLPTR(tab,tabcol), 
		     tabrow, Memc[value], SZ_FIELD)

	call align_field (LEFT, blank, Memc[value], 
			  Memc[field], TED_COLLEN(tab,tabcol))

	# Redraw field and reposition cursor

	orow = winstat (win, W_CURROW)
	ocol = winstat (win, W_CURCOL)

	call pos_field (scr, 1, row, col)
	call wmove (win, row, col)

	call waddstr (win, Memc[field])
	call wmove (win, orow, ocol)

	call sfree (sp)
end

# EDIT_FIELD -- Interactively edit a table field

procedure edit_field (win, field, maxch)

int	win		# i: Window descriptor
char	field[ARB]	# u: Table field
int	maxch		# i: Maximum line length
#--
int	row, col, ch, ic, jc, mc, nc
pointer	sp, data, buffer

string	nowrite "Cannot change field: table is read only"
string	fullfld "Cannot undelete word: not enough room in field"

int	strlen(), k_get(), winstat()

begin
	call wgetstruct (win, data)

	nc = strlen (field)
	ic = min (TED_FINDEX(data) - 1, nc)

	row = winstat (win, W_CURROW)
	col = winstat (win, W_CURCOL) - ic

	call smark (sp)
	call salloc (buffer, SZ_FIELD, TY_CHAR)
	Memc[buffer] = EOS

	TED_COMMAND(data) = NO

	repeat {

	    # Read character from keyboard

	    call ps_synch
	    ch = k_get ()

	    if (ch < K_BASE) {
		# Move to next field down

		if (ch == '\r') {
		    TED_DIRECT(data) = LEFT
		    TED_NXTROW(data) = TED_NXTROW(data) + 1
		    break

		# Move to next field across

		} else if (ch == '\t') {
		    if (TED_NXTCOL(data) < TED_LSTCOL(data)) {
			TED_DIRECT(data) = LEFT
			TED_NXTCOL(data) = TED_NXTCOL(data) + 1
			break
		    }

		# Insert character at current position

		} else if (TED_RDOFLD(data) == NO) {

		    # Check to see if field is full, if so, truncate

		    if (nc >= maxch) {
			nc = maxch - 1
			field[nc+1] = EOS
			if (ic > nc) {
			    ic = nc
			    call wmove (win, row, col+ic)
			}
		    }
			
		    ic = ic + 1
		    nc = nc + 1
		    TED_MRKFLD(data) = YES

		    if (ic == nc) {
			field[ic] = ch
			field[ic+1] = EOS
			call waddstr (win, field[ic])

		    } else {
			do jc = nc, ic, -1
			    field[jc+1] = field[jc]

			field[ic] = ch
			call waddstr (win, field[ic])
			call wmove (win, row, col+ic)
		    }

		# Print warning message, read_only field

		} else {
		    call warn1_prompt (TED_FSCREEN(data), nowrite)
		}

	    } else {
		switch (ch) {
		case K_UP:  # Move up one field
		    if (TED_NXTROW(data) > 1) {
			TED_DIRECT(data) = CENTER
			TED_NXTROW(data) = TED_NXTROW(data) - 1
			break
		    }

		case K_DOWN:  # Move down one field
		    if (TED_NXTROW(data) < TED_LSTROW(data)) {
			TED_DIRECT(data) = CENTER
			TED_NXTROW(data) = TED_NXTROW(data) + 1
			break
		    }
		case K_RIGHT:  # Move right one column
		    if (ic < nc) {
			ic = ic + 1
			call wmove (win, row, col+ic)

		    } else if (TED_NXTCOL(data) < TED_LSTCOL(data)) {
			TED_DIRECT(data) = LEFT
			TED_NXTCOL(data) = TED_NXTCOL(data) + 1
			break
		    }

		case K_LEFT:  # Move left one column
		    if (ic > 0) {
			ic = ic - 1
			call wmove (win, row, col+ic)

		    } else if (TED_NXTCOL(data) > 1) {
			TED_DIRECT(data) = RIGHT
			TED_NXTCOL(data) = TED_NXTCOL(data) - 1
			break
		    }

		case K_NEXTW:  # Move forwards one field
		    if (TED_NXTCOL(data) < TED_LSTCOL(data)) {
			TED_DIRECT(data) = LEFT
			TED_NXTCOL(data) = TED_NXTCOL(data) + 1
			break
		    }

		case K_PREVW:  # Move backwards one field
		    if (TED_NXTCOL(data) > 1) {
			TED_DIRECT(data) = LEFT
			TED_NXTCOL(data) = TED_NXTCOL(data) - 1
			break
		    }

		case K_NEXTP:  # Move forwards one screen
		    if (TED_NXTROW(data) < TED_LSTROW(data)) {
			TED_DIRECT(data) = LEFT
			TED_NXTROW(data) = min (TED_LSTROW(data), 
					   TED_NXTROW(data) + TED_PGSIZE(data))
			break
		    }

		case K_PREVP:  # Move backwards one page
		    if (TED_NXTROW(data) > 1) {
			TED_DIRECT(data) = LEFT
			TED_NXTROW(data) = max (1, 
					   TED_NXTROW(data) - TED_PGSIZE(data))
			break
		    }

		case K_HOME:  # Move to first row
		    if (TED_NXTROW(data) > 1) {
			TED_DIRECT(data) = LEFT
			TED_NXTROW(data) = 1
			break
		    }

		case K_END:  # Move to last row and column
		    if (TED_NXTROW(data) < TED_LSTROW(data)) {
			TED_DIRECT(data) = LEFT
			TED_NXTROW(data) = TED_LSTROW(data)
			break
		    }

		case K_BOL:  # Move to first column in table
		    if (TED_NXTCOL(data) == 1) {
			ic = 0
			call wmove (win, row, col)

		    } else {
			TED_DIRECT(data) = LEFT
			TED_NXTCOL(data) = 1
			break
		    }

		case K_EOL:  # Move to last column in table
		    if (TED_NXTCOL(data) == TED_LSTCOL(data)) {
			ic = nc
			call wmove (win, row, col+ic)

		    } else {
			TED_DIRECT(data) = RIGHT
			TED_NXTCOL(data) = TED_LSTCOL(data)
			break
		    }

		case K_DEL:  # Delete character underneath cursor
		    if (TED_RDOFLD(data) == NO) {
			if (ic < nc) {
			    TED_MRKFLD(data) = YES
			    mc = strlen (Memc[buffer])

			    Memc[buffer+mc] = field[ic+1]
			    Memc[buffer+mc+1] = EOS


			    do jc = ic+1, nc
				field[jc] = field[jc+1]
			    field[nc] = ' '
			    field[nc+1] = EOS

			    call waddstr (win, field[ic+1])
			    field[nc] = EOS
			    nc = nc - 1

			    call wmove (win, row, col+ic)
			}
		    } else {
			call warn1_prompt (TED_FSCREEN(data), nowrite)
		    }

		case K_BS:  # Delete character to left of cursor
		    if (TED_RDOFLD(data) == NO) {
			if (ic > 0) {
			    TED_MRKFLD(data) = YES
			    mc = strlen (Memc[buffer])

			    do jc = mc, 0, -1
				Memc[buffer+jc+1] = Memc[buffer+jc]
			    Memc[buffer] = field[ic]

			    ic = ic - 1
			    call wmove (win, row, col+ic)

			    do jc = ic+1, nc
				field[jc] = field[jc+1]
			    field[nc] = ' '
			    field[nc+1] = EOS

			    call waddstr (win, field[ic+1])
			    field[nc] = EOS
			    nc = nc - 1

			    call wmove (win, row, col+ic)
			}
		    } else {
			call warn1_prompt (TED_FSCREEN(data), nowrite)
		    }

		case K_DWORD:  # Delete entire field
		    if (TED_RDOFLD(data) == NO) {
			if (nc > 0) {
			    TED_MRKFLD(data) = YES
			    call strcpy (field[ic+1], Memc[buffer], nc-ic)

			    do jc = ic+1, nc 
				field[jc] = ' '
			    field[nc+1] = EOS

			    call wmove (win, row, col+ic)
			    call waddstr (win, field[ic+1])

			    field[ic+1] = EOS
			    nc = ic

			    call wmove (win, row, col+ic)
			}
		    } else {
			call warn1_prompt (TED_FSCREEN(data), nowrite)
		    }

		case K_DLINE:  # Delete entire line (not supported)
		    call ring_bell

		case K_UNDCHR: # Undelete a character
		    mc = strlen (Memc[buffer])
		    if (mc > 0) {
			do jc = nc+1, ic+1, -1
			    field[jc+1] = field[jc]

			field[ic+1] = Memc[buffer+mc-1]
			Memc[buffer+mc-1] = EOS

			call waddstr (win, field[ic+1])

			ic = ic + 1
			nc = nc + 1
			call wmove (win, row, col+ic)
		    }

		case K_UNDWRD: # Undelete a word
		    mc = strlen (Memc[buffer])
		    if ((mc + nc) > maxch) {
			call warn1_prompt (TED_FSCREEN(data), fullfld)

		    } else if (mc > 0) {
			call amovc (field[ic+1], field[ic+mc+1], nc-ic+1)
			call amovc (Memc[buffer], field[ic+1], mc)

			Memc[buffer] = EOS
			call waddstr (win, field[ic+1])

			ic = ic + mc
			nc = nc + mc
			call wmove (win, row, col+ic)
		    }

		case K_UNDLIN: # Undelete a line (not supported)
		    call ring_bell

		case K_HELP:  # Display help screen
		    call help_screen (win)

		case K_PAINT:  # Force screen redraw
		    call clearok (STDSCR, true)
		    call wrefresh (STDSCR)
		    call focus_window (win)

		case K_EXIT:  # Exit procedure
		    TED_COMMAND(data) = YES
		    break

		default:
		    call ring_bell
		}
	    }
	}

	# Terminate field with EOS and push back character 
	# that terminated input

	if (nc >= maxch)
	    ch = EOS

	field[nc+1] = EOS
	call k_pushbk (ch)

	TED_FINDEX(data) = ic + 1

end

procedure move_field (scr)

pointer	scr		# i: screen descriptor
#--
int	row, col

begin
	# Calculate the screen row and column and move the cursor there

	call pos_field (scr, TED_SCRIDX(scr), row, col)
	call wmove (TED_WINDOW(scr), row, col)

end

# POS_FIELD -- Find the cursor position in a field

procedure pos_field (scr, index, row, col)

pointer	scr		# i: screen descriptor
int	index		# i: index of character in current field
int	row		# o: window row
int	col		# o: window column
#--
int	icol
pointer	tab

begin
	# Get the table assoicated with this screen

	tab = TED_TABLE(scr)

	# Calculate the screen row and column

	row = TED_LABHEIGHT(tab) + 1 + TED_CURROW(scr) - TED_LOROW(scr)

	col = TED_LABWIDTH(tab) + 1
	do icol = TED_LOCOL(scr), TED_CURCOL(scr) - 1
	    col = col + TED_COLLEN(tab,icol) + 1

	col = col + index
end

# TRIM_FIELD -- Remove leading and trailing blanks from field

procedure trim_field (scr, rdonly, coltype, collen, field)

pointer	scr		# i: screen descriptor
int	rdonly		# i: read only table
int	coltype		# i: column type
int	collen		# i: column length
char	field[ARB]	# u: field to trim
#--
int	ibeg, iend, ic, jc

string	toolong  "Field too long to display, editing will cause loss of data"
int	strlen()

begin
	if (coltype < 0) {
	    # Get the start and end of the string

	    ibeg = 1
	    iend = strlen (field)
	    if (iend > collen) {
		iend = collen
		if (rdonly == NO)
		    call write_prompt (scr, NO, toolong)
	    }

	} else {
	    # Get the first and last characters in the string 
	    # which are not blank

	    ibeg = 0
	    iend = 0
	    for (ic = 1; field[ic] != EOS; ic = ic + 1) {
		if (field[ic] != ' ') {
		    if (ibeg == 0)
			ibeg = ic
		    iend = ic
		}
	    }
	}

	# Trim the field to the correct length

	if (ibeg <= 1) {
	    jc = iend + 1
	} else {
	    jc = 1
	    for (ic = ibeg; ic <= iend; ic = ic + 1) {
		field[jc] = field[ic]
		jc = jc + 1
	    }
	}

	field[jc] = EOS

end
