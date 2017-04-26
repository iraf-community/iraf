include <imhdr.h>
include <tbset.h>
include "keyselect.h"

#* HISTORY *
#* B.Simon	12-Mar-92	Original

# CPY_TABLE -- Copy keywords from header to table row

procedure cpy_table (im, tp, colptr, keywords)

pointer	im		# i: image descriptor
pointer	tp		# i: table descriptor
pointer	colptr		# i: pointer to array of column descriptors
char	keywords	# i: list of header keywords
#--
char	cat, sep
int	row, dtype, ic, jc, kc
pointer	sp, cp, nlist, vlist, name, value

data	cat	/ CONCAT_CHAR /
data	sep	/ SEP_CHAR /
 
string	nocolumn  "cpy_table: not enough columns to store keywords"

int	tbpsta(), brk_list(), stridx(), gstrcpy()

begin
	call smark(sp)
	call salloc (nlist, SZ_LINE, TY_CHAR)
	call salloc (vlist, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)
	
	ic = 1
	cp = colptr
	row = tbpsta (tp, TBL_NROWS) + 1

	# Extract each keyword from the list of keywords

	while (brk_list (keywords, ic, sep, Memc[nlist], SZ_LINE) > 0) {

	    # If the keyword is not a list of concatenated keywords
	    # copy its value into a string

	    if (stridx (cat, Memc[nlist]) == 0) {
		call get_keyword (im, Memc[nlist], dtype, 
				   Memc[vlist], SZ_LINE)

	    # Otherwise break the list of concatenated keywords
	    # and concatenate their values into a string

	    } else {
		jc = 1
		kc = 0
		while (brk_list (Memc[nlist], jc, cat, 
				 Memc[name], SZ_LINE) > 0){

		    call get_keyword (im, Memc[name], dtype, 
				      Memc[value], SZ_LINE)

		    if (dtype != 0) {
			kc = kc + gstrcpy (Memc[value], Memc[vlist+kc], 
					   SZ_LINE-kc)
			Memc[vlist+kc] = SEP_CHAR
			kc = kc + 1
		    }
		}
		kc = max (kc, 1)
		Memc[vlist+kc-1] = EOS
	    }

	    # Write the value into the table

	    if (Memi[cp] == NULL)
		call error (1, nocolumn)

	    call tbeptt (tp, Memi[cp], row, Memc[vlist])
	    cp = cp + 1
	}

	call sfree(sp)
end

# FMT_TABLE -- Retrieve column format from column description file

procedure fmt_table (cd, col, units, fmt, dtype)

int	cd		# i: file descriptor of column description file
char	col[ARB]	# i: name of column to retrieve information for
char	units[ARB]	# o: column units
char	fmt[ARB]	# o: column format
int	dtype		# o: column data type
#--
char	star, comment
bool	match
int	idx, junk, length, typevals[5]
pointer	sp, line, input, name, type, ftnfmt, errmsg

string	typestr  "rdibc"
string	badtype  "Illegal datatype for column %s (%s)"
string	badname  "Warning: column not found in column description file (%s)\n"

data	star	 / '*' /
data	comment  / '#' /
data	typevals /TY_REAL, TY_DOUBLE, TY_INT, TY_BOOL, TY_CHAR /

bool	streq()
int	getline(), stridx(), ctoi

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (input, SZ_COLNAME, TY_CHAR)
	call salloc (name, SZ_COLNAME, TY_CHAR)
	call salloc (type, SZ_COLFMT, TY_CHAR)
	call salloc (ftnfmt, SZ_COLFMT, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call strcpy (col, Memc[input], SZ_COLNAME)
	call strlwr (Memc[input])

	match = false
	call seek (cd, BOF)
	while (! match && getline (cd, Memc[line]) != EOF) {

	    # Remove trailing comments from line

	    idx = stridx (comment, Memc[line])
	    if (idx > 0)
		Memc[line+idx-1] = EOS

	    # Column name is the first word on the line

	    call sscan (Memc[line])
	    call gargwrd (Memc[name], SZ_COLNAME)
	    call strlwr (Memc[name])

	    # If the name matches the procedure argument 
	    # read the remaining fields on the line

	    match = streq (Memc[input], Memc[name])
	    if (match) {
		call gargwrd (Memc[type], SZ_COLFMT)
		call gargwrd (Memc[ftnfmt], SZ_COLFMT)
		call gargwrd (units, SZ_COLUNITS)

		call strlwr (Memc[type])
		call tbbftp (Memc[ftnfmt], fmt)

		# Convert the type string to the corresponding integer value

		if (Memc[type] == EOS) {
		    dtype = 0

		} else {
		    idx = stridx (Memc[type], typestr)
		    if (idx == 0) {
			call sprintf (Memc[errmsg], SZ_LINE, badtype)
			call pargstr (Memc[name])
			call pargstr (Memc[type])
			call error (1, Memc[errmsg])
		    }
		    
		    dtype = typevals[idx]
		    if (dtype == TY_CHAR) {
			idx = stridx (star, Memc[type])
			if (idx > 0) {
			    idx = idx + 1
			    junk = ctoi (Memc[type], idx, length)
			    if (length > 0)
				dtype = - length
			}
		    }
		}

	    }
	}

	# Send warning message and set defaults if no match

	if (! match) {
	    dtype = 0
	    fmt[1] = EOS
	    units[1] = EOS

	    call eprintf (badname)
	    call pargstr (col)
	}

	call sfree (sp)
end

# OP_TABLE -- Open the output table

pointer procedure op_table(im, output, keywords, columns, cdfile)

pointer	im		# i: image descriptor
char	output[ARB]	# i: table name
char	keywords[ARB]	# i: list of header keywords
char	columns[ARB]	# i: list of column names
char	cdfile[ARB]	# i: optional column description file
#--
bool	append
char	sep, cat
int	ic, jc, dtype
pointer	sp, tp, cp, cd, col, key, units, fmt, errmsg

data	sep	/ SEP_CHAR /
data	cat	/ CONCAT_CHAR /

string	nocolumn  "Column not found in existing output table (%s)"
string	nokeyword "op_table: no matching keyword for column"
string	notfound  "Warning: keyword not found when creating table (%s)\n"

bool	isblank()
int	open(), stridx(), imgftype(), brk_list(), type_keyword()
pointer	tbtopn()

begin
	call smark (sp)
	call salloc (col, SZ_COLNAME, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_COLUNITS, TY_CHAR)
	call salloc (fmt, SZ_COLFMT, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Open column description file

	if (isblank (cdfile)) {
	    cd = NULL
	} else {
	    cd = open (cdfile, READ_ONLY, TEXT_FILE)
	}

	# Append rows to the table if the table already exists
	# otherwise create a new table

	append = true
	iferr (tp = tbtopn (output, READ_WRITE, NULL)) {
	    append = false
	    tp = tbtopn (output, NEW_FILE, NULL)
	}

	ic = 1
	jc = 1
	while (brk_list (columns, ic, sep, Memc[col], SZ_COLNAME) > 0) {

	    if (brk_list (keywords, jc, sep, Memc[key], SZ_LINE) == 0)
		call error (1, nokeyword)

	    # Verify that the columns exist if we are in append mode
	    # Define the new columns if we are not

	    if (append) {
		call tbcfnd (tp, Memc[col], cp, 1)
		if (cp == NULL) {
		    call sprintf (Memc[errmsg], SZ_LINE, nocolumn)
		    call pargstr (Memc[col])
		    call error (1, Memc[errmsg])
		}

	    } else {
		# Get column characteristics from the column description file
		# or use defaults and image header keyword type

		if (cd != NULL)
		    call fmt_table (cd, Memc[col], 
				    Memc[units], Memc[fmt], dtype)

		if (cd == NULL || dtype == 0) {
		    Memc[units] = EOS
		    Memc[fmt] = EOS

		    if (stridx (cat, Memc[key]) != 0) {
			dtype = - SZ_BIGCOL

		    } else if (Memc[key] == '$'){
			dtype = type_keyword (Memc[key])

		    } else {
			iferr {
			    dtype = imgftype (im, Memc[key])
			} then {
			    dtype = 0
			    call eprintf (notfound)
			    call pargstr (Memc[key])
			}
		    }
		}

		if (dtype == 0 || dtype == TY_CHAR)
		    dtype = - SZ_STRCOL
		if (dtype == TY_SHORT || dtype == TY_LONG)
		    dtype = TY_INT
		if (dtype == TY_REAL)
		    dtype = TY_DOUBLE

		call tbcdef (tp, cp, Memc[col], Memc[units], Memc[fmt], 
			     dtype, 1, 1)
	    }
	}

	# Create the new table if not in append mode

	if (! append)
	    call tbtcre (tp)

	call sfree (sp)
	return (tp)
end

# RD_TABLE -- Create an array of column pointers from the list of column names

procedure rd_table (columns, tp, colptr)

char	columns[ARB]	# i: list of column names
pointer	tp		# i: table descriptor
pointer	colptr		# o: pointer to array of column names
#--
char	sep
int	nptr, ic
pointer	sp, cp, col, errmsg

data	sep	/ SEP_CHAR /
string	nocolumn "rd_table: column not found (%s)"

int	cnt_list(), brk_list()

begin
	call smark (sp)
	call salloc (col, SZ_LINE, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	nptr = cnt_list (columns) + 1
	call malloc (colptr, nptr, TY_POINTER)

	ic = 1
	cp = colptr
	while (brk_list (columns, ic, sep, Memc[col], SZ_LINE) > 0) {
	    call tbcfnd (tp, Memc[col], Memi[cp], 1)
	    if (cp == NULL) {
		call sprintf (Memc[errmsg], SZ_LINE, nocolumn)
		call pargstr (Memc[col])

		call error (1, Memc[errmsg])
	    }
	    cp = cp + 1
	}

	Memi[cp] = NULL
	call sfree(sp)
end
