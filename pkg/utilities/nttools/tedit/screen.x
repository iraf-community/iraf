include	<tbset.h>
include "display/curses.h"
include	"screen.h"
include "table.h"
include "field.h"

define	HELP_FILE	"ttools$tedit/tedit.key"

# SCREEN -- Procedures which manipulate screens

# ADD_SCREEN -- Add a new screen 

procedure add_screen (scr, table, columns, rdonly, inplace)

pointer	scr		# u: Screen descriptor
char	table[ARB]	# i: SDAS table name
char	columns[ARB]	# i: list of columns to edit
bool	rdonly		# i: edit table read only
bool	inplace		# i: edit table in place 
#--
extern	edit_field
int	iscr, jscr
pointer	oldscr, tab, win, field

bool	streq()
int	get_window()
pointer	map_table()

begin
	# See if the table is already bound to a screen

	jscr = 0
	for (iscr = 1; get_window (iscr, oldscr) != EOF; iscr = iscr + 1) {
	    tab = TED_TABLE(oldscr)
	    if (tab != NULL) {
		if (streq (TED_TABNAME(tab), table)) {
		    jscr = iscr
		    break
		}
	    }
	}

	# Get table structure from old screen, or create new structure

	if (jscr != 0) {
	    TED_TABLE(scr) = TED_TABLE(oldscr)

	    TED_LOROW(scr) = TED_LOROW(oldscr)
	    TED_HIROW(scr) = TED_HIROW(oldscr)

	    TED_LOCOL(scr) = TED_LOCOL(oldscr)
	    TED_HICOL(scr) = TED_HICOL(oldscr)

	    TED_CURROW(scr) = TED_CURROW(oldscr)
	    TED_CURCOL(scr) = TED_CURCOL(oldscr)

	} else {
	    TED_TABLE(scr) = map_table (scr, table, columns, rdonly, inplace)

	    TED_LOROW(scr) = 1
	    TED_HIROW(scr) = 1

	    TED_LOCOL(scr) = 1
	    TED_HICOL(scr) = 1

	    TED_CURROW(scr) = 1
	    TED_CURCOL(scr) = 1
	}

	TED_SCRIDX(scr) = 1
	TED_PASTE(scr) = NULL

	# Draw the new screen

	call move_screen (scr, LEFT, YES)

	# Create field structure and bind to window

	call malloc (field, TED_FLDLEN, TY_STRUCT)

	win = TED_WINDOW(scr)
	call wbindstruct (win, edit_field, field)

end

# DEL_SCREEN -- Delete screen

procedure del_screen (scr, force)

pointer	scr		# i: Screen descriptor
int	force		# i: Force table to be written
#--
int	iscr, jscr
pointer	sp, msg, tab1, tab2, scr2, tptr

bool	bool_prompt()
int	get_window()

begin
	# Allocate dynamic memory for message

	call smark (sp)
	call salloc (msg, SZ_LINE, TY_CHAR)

	# Close the paste table

	call cls_paste (scr)

	# Take no further action if no table is associated with this screen

	if (TED_TABLE(scr) == NULL)
	    return

	# See if this screen's table is associated with any other screen

	jscr = 0
	tab1 = TED_TABLE(scr)
	tptr = TED_TABPTR(tab1)
	TED_TABLE(scr) = NULL

	jscr = 0
	for (iscr = 1; get_window (iscr, scr2) != EOF; iscr = iscr + 1) {
	    tab2 = TED_TABLE(scr2)
	    if (tab2 != NULL && scr != scr2) {
		if (tptr == TED_TABPTR(tab2)) {
		    jscr = iscr
		    break
		}
	    }
	}

	# If not, close the table

	if (jscr == 0) {
	    if (force == YES) {
		call wrt_table (scr, tab1)

	    } else if (TED_DIRTY(tab1) == YES){
		call sprintf (Memc[msg], SZ_LINE, "Write %s?")
		call pargstr (TED_TABNAME(tab1))

		if (bool_prompt (Memc[msg]))
		    call wrt_table (scr, tab1)
	    }

	    call unmap_table (scr, tab1, force)
	}

	call sfree (sp)
end

# DRAW_SCREEN -- Draw the screen on the terminal

procedure draw_screen (scr)

pointer	scr		# i: Screen descriptor
#--
char	blank, uscore
int	height, width, ic, icol, irow, junk
pointer	sp, win, tab, tptr, line, temp

data	blank	/ ' ' /
data	uscore	/ '_' /

string	notable  "No table associated with this screen"

int	gstrcpy(), itoc()

begin
	win = TED_WINDOW(scr)
	call wdimen (win, height, width)

	if (TED_TABLE(scr) == NULL)
	    call err1_prompt (notable)

	tab = TED_TABLE(scr)
	tptr = TED_TABPTR(tab)

	call smark (sp)
	call salloc (line, width+1,TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Erase screen window and move cursor to start of window

	call werase (win)
	call wmove (win, 1, 1)

	# Write top line of screen label

	ic = gstrcpy ("Column", Memc[line], TED_LABWIDTH(tab))
	do icol = TED_LOCOL(scr), TED_HICOL(scr) {
	    Memc[line+ic] = ' '
	    ic = ic + 1

	    junk = itoc (icol, Memc[temp], SZ_LINE)
	    call align_field (CENTER, blank, Memc[temp], 
			      Memc[line+ic], TED_COLLEN(tab,icol))
	    ic = ic + TED_COLLEN(tab,icol)
	}

	Memc[line+ic] = '\n'
	Memc[line+ic+1] = EOS
	call waddstr (win, Memc[line])

	# Write second line of screen label

	ic = gstrcpy ("Label ", Memc[line], TED_LABWIDTH(tab))
	do icol = TED_LOCOL(scr), TED_HICOL(scr) {
	    Memc[line+ic] = ' '
	    ic = ic + 1

	    call tbcigt (TED_COLPTR(tab,icol), TBL_COL_NAME, 
			 Memc[temp], SZ_LINE)

	    call align_field (CENTER, uscore, Memc[temp], 
			      Memc[line+ic], TED_COLLEN(tab,icol))
	    ic = ic + TED_COLLEN(tab,icol)
	}

	Memc[line+ic] = '\n'
	Memc[line+ic+1] = EOS
	call waddstr (win, Memc[line])

	# Write the table elements a row at a time

	do irow = TED_LOROW(scr), TED_HIROW(scr) {
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

	# Clear the prompt window

	call clear_prompt (NULL)

	# Move cursor to current field and refresh window

	call move_field (scr)
	call wrefresh (win)
	call sfree (sp)
end

# EDIT_SCREEN -- Interactively edit the table bound to this screen

procedure edit_screen (scr)

pointer	scr		# i: Screen descriptor
#--
int	win, row, col, ch
pointer	sp, field, tab, data

string	notable  "No table associated with this screen"
string	badtype  "Illegal data type for this field"
string	notadded "Cannot add row to read only table"

bool	check_field()
int	tbpsta(), strlen(), k_get()

begin
	# Allocate dynamic memory for table field

	call smark (sp)
	call salloc (field, SZ_FIELD, TY_CHAR)
	Memc[field] = EOS

	# Get window and table associated with screen

	if (TED_TABLE(scr) == NULL)
	    call err1_prompt (notable)

	win = TED_WINDOW(scr)
	tab = TED_TABLE(scr)

	# Initialize the field data structure

	call wgetstruct (win, data)
	TED_FSCREEN(data) = scr
	TED_RDOFLD(data) = TED_READONLY(tab)
	TED_PGSIZE(data) = TED_HIROW(scr) - TED_LOROW(scr) + 1
	TED_LSTROW(data) = tbpsta (TED_TABPTR(tab), TBL_NROWS)
	TED_LSTCOL(data) = TED_NCOLS(tab)
	TED_NXTROW(data) = TED_CURROW(scr)
	TED_NXTCOL(data) = TED_CURCOL(scr)
	TED_DIRECT(data) = LEFT
	TED_FINDEX(data) = 1
	TED_MRKFLD(data) = NO
	TED_COMMAND(data) = NO

	# Edit fields until user presses the command key

	while (TED_COMMAND(data) == NO) {

	    # Read the new field from the table

	    row = TED_NXTROW(data)
	    col = TED_NXTCOL(data)

	    call tbegtt (TED_TABPTR(tab), TED_COLPTR(tab,col), row, 
			 Memc[field], SZ_FIELD)
	    call trim_field (scr, TED_READONLY(tab), TED_COLTYPE(tab,col),
			     TED_COLLEN(tab,col), Memc[field])

	    # Set the current row, column, and character

	    TED_CURROW(scr) = row
	    TED_CURCOL(scr) = col
	    if (TED_DIRECT(data) == LEFT)
		TED_SCRIDX(scr) = 1
	    else if (TED_DIRECT(data) == RIGHT)
		TED_SCRIDX(scr) = strlen(Memc[field]) + 1
	    else
		TED_SCRIDX(scr) = min (TED_FINDEX(data), 
				       strlen(Memc[field]) + 1)

	    call move_screen (scr, TED_DIRECT(data), NO)

	    # Get the new field value

	    TED_MRKFLD(data) = NO
	    TED_FINDEX(data) = TED_SCRIDX(scr)
	    call weditstr (win, Memc[field], TED_COLLEN(tab,col))

	    ch = k_get ()

	    # If the field has changed, check it and write it to the table

	    if (TED_MRKFLD(data) == YES) {

		TED_DIRTY(tab) = YES
		call tbeptt (TED_TABPTR(tab), TED_COLPTR(tab,col), 
			     row, Memc[field])

		# Redraw the field if it does not match the data type

		if (! check_field (TED_COLTYPE(tab,col), Memc[field]))
		    call draw_field (scr)

	    }

	    # If the cursor has moved beyond the end of the table, 
	    # add new rows

	    if (TED_NXTROW(data) > TED_LSTROW(data)) {
		# Check for read only table
		if (TED_READONLY(tab) == NO) {
		    call add_field (scr, TED_NXTROW(data), TED_NXTCOL(data))
		    TED_LSTROW(data) = TED_NXTROW(data)

		} else {
		    call warn1_prompt (scr, notadded)

		    TED_NXTROW(data) = row
		    TED_NXTCOL(data) = col
		    TED_DIRECT(data) = LEFT
		}
	    }
	}

	call sfree (sp)

end

# END_SCREEN -- Free all screens and their associated windows

procedure end_screen ()

#--

begin
	call endwin
end

# HELP_SCREEN -- Display the help screen

procedure help_screen (win)

int	win		# i: window that currently contains the cursor
#--
bool	flag
int	fd, ic, helpwin, nrows, ncols, row, ihelp, key
pointer	sp, ch, eseq, label, name, msg, text

int	open(), getline(), newwin(), k_get()

string	title1  "The following commands are available after typing %s\n\n"
string	title2  "The following editing commands are available\n\n"
string	footer1 "\nPress any key to continue displaying commands\n"
string	footer2 "\nPress any key to resume editing\n"
string	hformat "%4w%-12.12s = %-12.12s"

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (eseq, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_LINE/2, TY_CHAR)
	call salloc (name, SZ_LINE/2, TY_CHAR)
	call salloc (msg, SZ_LINE, TY_CHAR)

	# Create help window

	helpwin = newwin (GIANT, GIANT, 1, 1)
	call wdimen (helpwin, nrows, ncols)

	# Display list of commands

	ifnoerr {
	    fd = open (HELP_FILE, READ_ONLY, TEXT_FILE)

	} then {
	    call k_eseq ("EXIT_UPDATE", Memc[eseq], SZ_FNAME)
	    call sprintf (Memc[msg], SZ_LINE, title1)
	    call pargstr (Memc[eseq])

	    call waddstr (helpwin, Memc[msg])

	    # Read a line at a time from the file, pausing
	    # to page after a window full of information
	    # has been displayed

	    row = 3
	    while (getline (fd, Memc[msg]) != EOF) {
		row = row + 1
		if (row > nrows - 2) {
		    call waddstr (helpwin, footer1)

		    row = 1
		    call refresh
		    key = k_get ()
		    call werase (helpwin)
		}
		call waddstr (helpwin, Memc[msg])
	    }
	    call close (fd)

	    call waddstr (helpwin, footer1)
	    call refresh
	    key = k_get ()
	}

	# Construct the list of editing commands

	call k_help (text)

	call werase (helpwin)
	call waddstr (helpwin, title2)

	row = 3
	ihelp = 0
	flag = true

	# Retrieve the command name and control sequence from the help
	# structure. Write the information to the help window.

	for (ch = text; Memc[ch] != EOS; ch = ch + 1) {
	    if (flag) {	
		if (Memc[ch] != '=') {
		    Memc[label+ic] = Memc[ch]
		    ic = ic + 1
		} else {
		    Memc[label+ic] = EOS
		    flag = false
		    ic = 0
		}
	    } else {
		if (Memc[ch] != '\n') {
		    Memc[name+ic] = Memc[ch]
		    ic = ic + 1
		} else {
		    Memc[name+ic] = EOS
		    ihelp = ihelp + 1
		    flag = true
		    ic = 0

		    call sprintf (Memc[msg], SZ_LINE, hformat)
		    call pargstr (Memc[label])
		    call pargstr (Memc[name])

		    call waddstr (helpwin, Memc[msg])

		    if (mod (ihelp, 2) == 0) {
			call waddstr (helpwin, "\n")

			row = row + 1
			if (row >= nrows - 2) {
			    call waddstr (helpwin, footer1)

			    row = 1
			    call refresh
			    key = k_get ()
			    call werase (helpwin)
			}
		    }
		}
	    }
	}

	if (mod (ihelp, 2) == 1)
	    call waddstr (helpwin, "\n")
	call waddstr (helpwin, footer2)

	call refresh
	key = k_get ()

	# Delete the help window and restore cursor to current window

	call delwin (helpwin)
	call focus_window (win)

	call sfree (sp)
end

# INIT_SCREEN -- Initialize the screen handling routines

procedure init_screen (table, columns, rdonly, inplace, scr)

char	table[ARB]	# i: SDAS table name
char	columns[ARB]	# i: list of columns to edit
bool	rdonly		# i: edit table read only
bool	inplace		# i: edit table in place 
pointer	scr		# o: initial screen
#--

begin
	# Initialize the window handling routines

	call initscr

	# Create the first (and default) screen

	call init_window (scr)
	call add_screen (scr, table, columns, rdonly, inplace)

end

# JOIN_SCREEN -- Remove this screen from the terminal display

procedure join_screen (scr, force)

pointer	scr		# u: Screen descriptor
int	force		# i: Force screen to be written
#--

int	count_window()

begin
	if (count_window () < 2)
	    return

	call del_screen (scr, force)
	call join_window (scr)
	
end
 
# MOVE_SCREEN -- Move the table (scroll) within the screen

procedure move_screen (scr, align, force)

pointer	scr		# i: screen descriptor
int	align		# i: column alignment
int	force		# i: force redraw
#--
int	row, col, tabrows, tabcols, nrows, ncols, newrow, newcol, icol
pointer	sp, tab, errmsg

string	notable    "No table associated with this screen"
string	badsize    "Screen size error: t= %d b= %d l= %d r= %d"

int	tbpsta()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	if (TED_TABLE(scr) == NULL)
	    call err1_prompt (notable)

	# First check whether field is currently on the screen

	row = TED_CURROW(scr)
	col = TED_CURCOL(scr)

	if (row >= TED_LOROW(scr) && row <= TED_HIROW(scr) &&
	    col >= TED_LOCOL(scr) && col <= TED_HICOL(scr) && force == NO) {
	    call move_field (scr)
	    return
	}

	# Get dimensions of table and window

	tab = TED_TABLE(scr)
	tabrows = tbpsta (TED_TABPTR(tab), TBL_NROWS)
	call wdimen (TED_WINDOW(scr), nrows, ncols)

	if (row < TED_LOROW(scr) || row > TED_HIROW(scr)) {
	    newrow = max (1, row - (nrows - TED_LABHEIGHT(tab))/ 2)
	} else {
	    newrow = TED_LOROW(scr)
	}

	if (col < TED_LOCOL(scr) || col > TED_HICOL(scr)) {
	    newcol = col
	} else if (align == LEFT) {
	    newcol = TED_LOCOL(scr)
	} else {
	    newcol = TED_HICOL(scr)
	}

	# Update screen descriptor

	TED_LOROW(scr) = max (newrow, 1)
	TED_HIROW(scr) = (nrows - TED_LABHEIGHT(tab)) + (newrow - 1)
	TED_HIROW(scr) = min (TED_HIROW(scr), tabrows)

	tabcols = TED_LABWIDTH(tab)
	if (align == LEFT) {
	    TED_LOCOL(scr) = max (newcol, 1)
	    TED_HICOL(scr) = TED_NCOLS(tab)
	    do icol = newcol, TED_NCOLS(tab) {
		tabcols = tabcols + TED_COLLEN(tab,icol) + 1
		if (tabcols >= ncols) {
		    TED_HICOL(scr) = icol - 1
		    break
		}
	    }

	} else {
	    TED_LOCOL(scr) = 1
	    TED_HICOL(scr) = min (newcol, TED_NCOLS(tab))
	    do icol = newcol, 1, -1 {
		tabcols = tabcols + TED_COLLEN(tab,icol) + 1
		if (tabcols >= ncols) {
		    TED_LOCOL(scr) = icol + 1
		    break
		}
	    }
	}

	# Sanity check for new descriptor values

	if (TED_LOROW(scr) > TED_HIROW(scr) ||
	    TED_LOCOL(scr) > TED_HICOL(scr)   ) {

	    call sprintf (Memc[errmsg], SZ_LINE, badsize)
	    call pargi (TED_LOROW(scr))
	    call pargi (TED_HIROW(scr))
	    call pargi (TED_LOCOL(scr))
	    call pargi (TED_HICOL(scr))

	    call err1_prompt (Memc[errmsg])
	}

	# Redraw screen

	call draw_screen (scr)
	call sfree (sp)
end

procedure split_screen (scr1, scr2, table, columns, rdonly, inplace)

pointer	scr1		# u: current screen
pointer	scr2		# o: new screen
char	table[ARB]	# i: SDAS table name
char	columns[ARB]	# i: list of columns to edit
bool	rdonly		# i: edit table read only
bool	inplace		# i: edit table in place 
#--

begin
	# Create new window

	call split_window (scr1, scr2)
	if (scr2 == NULL)
	    return

	# Fill in the descriptor fields

	call add_screen (scr2, table, columns, rdonly, inplace)

end

