include <tbset.h>
include "screen.h"
include "table.h"
include "field.h"

# CREATE_TABLE -- Create a new table

procedure create_table (name, tptr)

char	name[ARB]	# i: Table name
pointer	tptr		# o: Table pointer
#--
int	code, type
pointer	sp, cp, cname, cunits, ftnfmt, sppfmt, ctype, errmsg

string	newtable  "Table does not exist. Create it?"
string	notable   "Task exit. Table does not exist."
string	nocreate  "Could not create table"
string	nocolumn  "Could not create column"
string	colprompt "Column name? (Press return to exit)"

bool	bool_prompt()
int	errget()
pointer	tbtopn()

begin
	if (! bool_prompt (newtable))
	    call err1_prompt (notable)

	call smark (sp)
	call salloc (cname, SZ_COLNAME, TY_CHAR)
	call salloc (cunits, SZ_COLUNITS, TY_CHAR)
	call salloc (ftnfmt, SZ_COLFMT, TY_CHAR)
	call salloc (sppfmt, SZ_COLFMT, TY_CHAR)
	call salloc (ctype, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	iferr {
	    tptr = tbtopn (name, NEW_FILE, NULL)
	} then {
	    code = errget (Memc[errmsg], SZ_LINE)
	    call err2_prompt (nocreate, Memc[errmsg])
	}

	# Get parameters defining each column

	repeat {
	    call read_prompt (colprompt, Memc[cname], SZ_COLNAME)
	    if (Memc[cname] == EOS)
		break

	    repeat {
		call read_prompt ("Column type? (r,d,i,b,ch*n)", 
				  Memc[ctype], SZ_FNAME)

		iferr (call tbbtyp (Memc[ctype], type)) {
		    call ring_bell
		} else {
		    break
		}
	    } 

	    repeat {
		call read_prompt ("Column format?", Memc[ftnfmt], SZ_COLFMT)
		call tbbftp (Memc[ftnfmt], Memc[sppfmt])

		if (Memc[sppfmt] == EOS && Memc[ftnfmt] != EOS) {
		    call ring_bell
		} else {
		    break
		}
	    }

	    call read_prompt ("Column units?", Memc[cunits], SZ_COLUNITS)

	    # Add new column to table

	    iferr {
		call tbcdef (tptr, cp, Memc[cname], Memc[cunits], 
			     Memc[sppfmt], type, 1, 1)
	    } then {
		code = errget (Memc[errmsg], SZ_LINE)
		call err2_prompt (nocolumn, Memc[errmsg])
	    }
	}

	iferr {
	    call tbtcre (tptr)
	} then {
	    code = errget (Memc[errmsg], SZ_LINE)
	    call err2_prompt (nocreate, Memc[errmsg])
	}

	# Add an empty row to the table

	call tbtwer (tptr, 1)
	call sfree (sp)
end

# MAP_TABLE -- Map a table into a descriptor so that it can be edited

pointer procedure map_table (scr, table, columns, rdonly, inplace)

pointer	scr		# i: Screen descriptor
char	table[ARB]	# i: SDAS table name
char	columns[ARB]	# i: list of columns to edit
bool	rdonly		# i: edit table read only
bool	inplace		# i: edit table in place 
#--
bool	place, new
int	dirty, numcol, numrow, numptr, iptr
int	height, width, maxcol, clen
pointer	sp, tabname, filename, ext, cname, tptr, tab

string	notable  "Table does not exist" 
string	nowrite  "No write access to table" 
string	nocols   "Column names not found in table"
string	emptytab "Table is empty" 

int	access(), tbtacc(), tbpsta()
int	tbcigi(), strlen(), btoi()

begin
	# Allocate dynamic memory for temporary names

	call smark (sp)
	call salloc (tabname, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_COLNAME, TY_CHAR)
	call salloc (cname, SZ_COLNAME, TY_CHAR)

	# Get filename from table name

	call tbfile (table, Memc[tabname], Memc[filename], Memc[ext], SZ_FNAME)

	# Check table permissions

	if (tbtacc (table) == NO) {
	    dirty = YES
	    place = true
	    new = true
	    call create_table (table, tptr)
	    call strcpy (table, Memc[filename], SZ_FNAME)

	} else {
	    dirty = NO
	    place = inplace
	    new = false

	    if (access (Memc[filename], READ_WRITE, 0) == NO && ! rdonly)
		call err2_prompt (nowrite, table)

	    call tu_open (table, "ted", rdonly, inplace, 
			  tptr, Memc[filename], SZ_FNAME)
	}

	# Allocate descriptor

	numcol = tbpsta (tptr, TBL_NCOLS)
	numrow = tbpsta (tptr, TBL_NROWS)

	call malloc (tab, TED_TABLEN, TY_STRUCT)
	call malloc (TED_NAMEPTR(tab), SZ_FNAME, TY_CHAR)
	call malloc (TED_COLARY(tab), numcol, TY_INT)
	call malloc (TED_TYPARY(tab), numcol, TY_INT)
	call malloc (TED_LENARY(tab), numcol, TY_INT)

	# Fill in scalar fields

	TED_READONLY(tab) = btoi (rdonly)
	TED_NEWTAB(tab) = btoi (new)
	TED_INPLACE(tab) = btoi (place)
	TED_DIRTY(tab) = dirty
	TED_TABPTR(tab) = tptr

	# Set the width and height of the label area surrounding
	# the table display

	TED_LABWIDTH(tab) = log10 (real(numrow + 1000)) + 1.0
	TED_LABWIDTH(tab) = max (6, TED_LABWIDTH(tab))
	TED_LABHEIGHT(tab) = 2

	call strcpy (Memc[filename], TED_TABNAME(tab), SZ_FNAME)

	# Get vector fields (column pointers, types, and lengths)

	call tctexp (tptr, columns, numcol, numptr, TED_COLPTR(tab,1))

	TED_NCOLS(tab) = numptr
	if (numptr == 0) {
	    call err2_prompt (nocols, columns)
	} else if (numcol == numptr) {
	    TED_ALLCOLS(tab) = YES
	} else {
	    TED_ALLCOLS(tab) = NO
	}

	call wdimen (TED_WINDOW(scr), height, width)
	maxcol = width - (TED_LABWIDTH(tab) + 2)

	do iptr = 1, numptr {
	    TED_COLTYPE(tab,iptr) = tbcigi (TED_COLPTR(tab,iptr),
					   TBL_COL_DATATYPE)

	    clen = tbcigi (TED_COLPTR(tab,iptr), TBL_COL_FMTLEN)
	    call tbcigt (TED_COLPTR(tab,iptr), TBL_COL_NAME,
			 Memc[cname], SZ_COLNAME)

	    TED_COLLEN(tab,iptr) = min (maxcol, max (clen, 
					strlen (Memc[cname])))
	}

	# Check to see if table is empty
	# Write a single blank row unless table is read only

	if (numrow < 1 ) {
	    if (rdonly) {
		call err2_prompt (emptytab, table)
	    } else {
		TED_DIRTY(tab) = YES
		call tbtwer (tptr, 1)
	    }
	}

	# Return pointer to descriptor

	call sfree (sp)
	return (tab) 
	end

# UNMAP_TABLE -- Release table descriptor

procedure unmap_table (scr, tab, force)

pointer	scr		# i: Currently active screen
pointer	tab		# i: Table descriptor
int	force		# i: Force unmap if table is dirty
#--
bool	quit
pointer	sp, table

begin
	# Allocate memory for strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)

	# Close table if still open and delete if 
	# it was a new table that was not written or not in place

	if (TED_TABPTR(tab) != NULL) {
	    quit = TED_NEWTAB(tab) == YES || TED_INPLACE(tab) == NO
	    call tbtnam (TED_TABPTR(tab), Memc[table], SZ_FNAME)

	    call tu_close (TED_TABPTR(tab), TED_INPLACE(tab), 
			   quit, Memc[table])
	}

	# Free descriptor

	call mfree (TED_NAMEPTR(tab), TY_CHAR)
	call mfree (TED_COLARY(tab), TY_INT)
	call mfree (TED_TYPARY(tab), TY_INT)
	call mfree (TED_LENARY(tab), TY_INT)

	call mfree (tab, TY_STRUCT)
	call sfree (sp)
end

# WRT_TABLE -- Close table and write back to original file

procedure wrt_table (scr, tab)

pointer	scr		# i: Currently active screen
pointer	tab		# i: Table descriptor 
#-- 
bool	quit
pointer	sp, table

string	noname "Could not rename new table, it has this temporary name"

begin
	# Return if table already written

	if (TED_TABPTR(tab) == NULL)
	    return

	# Allocate memory for strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)

	# Get current file name, then close table

	quit = TED_INPLACE(tab) == NO && TED_DIRTY(tab) == NO
	call strcpy (TED_TABNAME(tab), Memc[table], SZ_FNAME)

	iferr {
	    call tu_close (TED_TABPTR(tab), TED_INPLACE(tab), 
			   quit, Memc[table])
	} then {
	    call warn2_prompt (scr, noname, Memc[table])
	    TED_INPLACE(tab) = YES
	}

	# Set table pointer to NULL and set newtab to NO, to mark it as written

	TED_TABPTR(tab) = NULL
	TED_NEWTAB(tab) = NO
	call sfree (sp)
end

