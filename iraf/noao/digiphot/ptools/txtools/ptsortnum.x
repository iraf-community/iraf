include <ctype.h>
include "../../lib/ptkeysdef.h"

# PT_SORTNUM -- Decode the column used for the sort, compute the file
# record structure map,  sort on the extracted column, reorder the
# input file and write the output file.

int procedure pt_sortnum (tp_in, tp_out, column, ascend)

pointer	tp_in				# input Table descriptor
pointer	tp_out				# output Table descriptor
char	column[ARB]			# column name for sort
int	ascend				# forward sort

int	coltype, colwidth, nrecs
pointer	key, colptr, colindex, recmap
int	pt_colmap()

begin
	# Initialize.
	call pt_kyinit (key)
	colptr = NULL
	colindex = NULL
	recmap = NULL

	# Decode the sort column and map the record structure.
	nrecs = pt_colmap (key, tp_in, tp_out, column, colptr, colindex,
	    coltype, colwidth, recmap)  

	# Sort the column and write the output file.
	if (nrecs > 0) {
	    call pt_colsort (colptr, Memi[colindex], nrecs, coltype)
	    if (ascend == NO)
		call pt_flipi (Memi[colindex], nrecs)
	    call pt_reorder (tp_in, tp_out, Memi[recmap], Memi[colindex], nrecs)
	}

	# Free space.
	if (colptr != NULL)
	    call mfree (colptr, coltype)
	if (colindex != NULL)
	    call mfree (colindex, TY_INT)
	if (recmap != NULL)
	    call mfree (recmap, TY_INT)
	call pt_kyfree (key)

	return (nrecs)
end


define	BUFSIZE		1000

# PT_COLMAP -- Decode the column to be sorted and compute the record
# structure of the file.

int procedure pt_colmap (key, tp_in, tp_out, column, colptr, colindex, coltype,
	bufwidth, recmap)  

pointer	key		# pointer to the database structure
int	tp_in		# the input text file descriptor
int	tp_out		# the output text file descriptor
char	column[ARB]	# column to be sorted
pointer	colptr		# pointer to extracted column array
pointer	colindex	# pointer to index array for extracted column
int	coltype		# data type of the column to be sorted
int	bufwidth	# column width if chars
pointer	recmap		# pointer to the record structure map

int	first_rec, nunique, uunique, funique, record
int	ncontinue, recptr, nchars, szbuf, colwidth, field, element
long	loffset, roffset
pointer	sp, line, name, value
int	getline(), strncmp(), pt_kstati()
long	note()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)

	# Initialize the file read.
	first_rec = YES
	nunique = 0
	uunique = 0
	funique = 0
	record = 0
	szbuf = 0

	# Initilize the record read.
	ncontinue = 0
	recptr = 1

	# Loop over the text file records.
	repeat  {

	    # Read in a line of the text file.
	    loffset = note (tp_in) 
	    nchars = getline (tp_in, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] == KY_CHAR_POUND) {

	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
		    if (first_rec == YES)
		        call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_NAME,
		    KY_LEN_STR) == 0) {
		    nunique = nunique + 1
		    call pt_kname (key, Memc[line], nchars, nunique)
		    call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_UNITS,
		    KY_LEN_STR) == 0) {
		    uunique = uunique + 1
		    call pt_knunits (key, Memc[line], nchars, uunique)
		    call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_FORMAT,
		    KY_LEN_STR) == 0) {
		    funique = funique + 1
		    call pt_knformats (key, Memc[line], nchars, funique)
		    call putline (tp_out, Memc[line])
	        } else {
		    # skip lines beginning with # sign
		    call putline (tp_out, Memc[line])
	        }

	    } else if (Memc[line] == KY_CHAR_NEWLINE) {
		# skip blank lines
		call putline (tp_out, Memc[line])

	    } else {

		# Get the variable index.
		if (first_rec == YES) {
		    call pt_kid (column, Memc[name], element)
		    field = pt_kstati (key, Memc[name], KY_INDEX)
		    if (field <= 0)
			break
		}

		# Save the offset of the beginning of the current record.
		if (recptr == 1)
		    roffset = loffset

		# Construct the data record.
		call pt_kgfield (key, field, element, Memc[line], nchars,
		    Memc[value], first_rec, recptr, ncontinue) 

	        # Decode the selected fields.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Select the appropriate column, get its datatype and
		    # allocate the appropriate space.

		    if (first_rec == YES) {
			element = pt_kstati (key, column, KY_ELEMENT)
			if (IS_INDEFI(element))
			    break
			coltype = pt_kstati (key, column, KY_DATATYPE)
			if (IS_INDEFI(coltype))
			    break
			colwidth = pt_kstati (key, column, KY_LENGTH)
			if (coltype == TY_CHAR)
			    bufwidth = colwidth + 1
			else
			    bufwidth = 1
		    }

		    # Reallocate buffer space if necessary.
		    if (record  >= szbuf) {
			szbuf = szbuf + BUFSIZE
			if (coltype == TY_CHAR)
			    call realloc (colptr, szbuf * bufwidth, TY_CHAR)
			else
			    call realloc (colptr, szbuf, coltype)
			call realloc (colindex, szbuf, TY_INT)
			call realloc (recmap, szbuf, TY_INT)
		    }

		    # Decode the selected column.
		    record = record + 1
		    Memi[colindex+record-1] = 1 + (record - 1) * bufwidth
		    Memi[recmap+record-1] = roffset
		    call pt_gsrt (Memc[value], colptr, coltype, bufwidth,
		        record)
		    first_rec = NO

		    # Reinitialize the record read.
		    ncontinue = 0
		    recptr = 1
	        }
	    }

	}

	# Cleanup.
	call sfree (sp)
	return (record)
end


# PT_KGFIELD -- Fetch a single fields from the input file.

procedure pt_kgfield (key, field, element, line, nchars, value, first_rec,
	recptr, ncontinue)

pointer	key		# pointer to record structure
int	field		# field to be fetched
int	element		# field array element
char	line[ARB]	# input line
int	nchars		# length of line array
char	value[ARB]	# the field value
int	first_rec	# first record
int	recptr		# line per record index
int	ncontinue	# number of unique lines per record

int	len, i, cip, nper_line, nokeys, nckeys, nkeys

begin
	# Fetch the value if it is a #K parameter as this will already
	# be sorted in the key structure.

	if ((recptr == 1) && (field <= KY_NPKEYS(key))) {
	    len = Memi[KY_KINDICES(key)+field-1]
	    call amovc (Memc[Memi[KY_PTRS(key)+field-1]], value, len)
	    value[len+1] = EOS
	}

	# The number of header columns defined by #K at the beginning of
	# the file is nokeys.
	if (recptr == 1)
	    nokeys = KY_NPKEYS(key)

	# Increment the continuation statement counter or reset to 0.
	if (line[nchars-2] == '*')
	    ncontinue = ncontinue + 1
	else
	    ncontinue = 0

	# Fill in the record.
	cip = 1
	if (ncontinue < 1) {

	    nper_line = Memi[KY_NPLINE(key)+recptr-1]
	    nkeys = nokeys + nper_line
	    call amovki (int(1), Memi[KY_NELEMS(key)+nokeys], nper_line)

	    do i = nokeys + 1, nkeys {
	    	len = Memi[KY_KINDICES(key)+i-1]
		if (i == field) {
		    call amovc (line[cip], value, len)
	    	    value[len+1] = EOS
		}
		cip = cip + len
	    }

	    recptr = recptr + 1
	    nokeys = nkeys

	} else if (ncontinue == 1) {

	    nckeys = nokeys + 1
	    nkeys = nokeys + Memi[KY_NPLINE(key)+recptr-1]

	    if (first_rec == YES) {
		Memi[KY_NCONTINUE(key)+recptr-1] = KY_NLINES
	        do i = nckeys, nkeys
		    call malloc (Memi[KY_PTRS(key)+i-1], KY_NLINES *
			Memi[KY_KINDICES(key)+i-1], TY_CHAR)
	    }

	    do i = nckeys, nkeys {
	    	len = Memi[KY_KINDICES(key)+i-1]
		if ((i == field) && (element == 1)) {
		    call amovc (line[cip], value, len)
	    	    value[len+1] = EOS
		}
		cip = cip + len
	    }

	    nokeys = nkeys
	    recptr = recptr + 1

	} else {

	    if (ncontinue > Memi[KY_NCONTINUE(key)+recptr-2]) {
		Memi[KY_NCONTINUE(key)+recptr-2] = 
		    Memi[KY_NCONTINUE(key)+recptr-2] + KY_NLINES
		do i = nckeys, nokeys
		    call realloc (Memi[KY_PTRS(key)+i-1],
		        Memi[KY_NCONTINUE(key)+recptr-2] *
		        Memi[KY_KINDICES(key)+i-1], TY_CHAR)
	    }

	    do i = nckeys, nkeys {
	    	len = Memi[KY_KINDICES(key)+i-1]
		if ((i == field) && (element == ncontinue)) {
		    call amovc (line[cip], value, len)
	    	    value[len+1] = EOS
		}
		Memi[KY_NELEMS(key)+i-1] = ncontinue
		cip = cip + len
	    }
	}
end


# PT_COLSORT -- Sort the column.

procedure pt_colsort (colptr, colindex, nrecs, coltype)

pointer	colptr			# array of column pointers
int	colindex[ARB]		# column indices
int	nrecs			# number of records
int	coltype			# column type

begin
	# Sort the column.
	switch (coltype) {
	case TY_INT:
	    call pt_qsorti (Memi[colptr], colindex, colindex, nrecs)
	case TY_REAL:
	    call pt_qsortr (Memr[colptr], colindex, colindex, nrecs)
	case TY_CHAR:
	    call strsrt (colindex, Memc[colptr], nrecs)
	case TY_BOOL:
	    call pt_qsortb (Memb[colptr], colindex, colindex, nrecs)
	}
end


# PT_REORDER -- Reorder the input file and write it to the output file.

procedure pt_reorder (tp_in, tp_out, recmap, colindex, nrecs)

int	tp_in		# input table file descriptor
int	tp_out		# output file descriptor
int	recmap[ARB]	# record strucuture map
int	colindex[ARB]	# column index
int	nrecs		# number of records

int	i
long	lptr
pointer	sp, line

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	do i = 1, nrecs {
	    lptr = recmap[colindex[i]]
	    call seek (tp_in, lptr)
	    call pt_rwrecord (tp_in, tp_out, Memc[line])
	}

	call sfree (sp)
end


# PT_GSRT  -- Decode the column to be sorted.

procedure pt_gsrt (value,  colptr, coltype, colwidth, record)

char	value[ARB]	# value to be decoded
pointer	colptr		# pointer to the decode column
int	coltype		# the data type of the sort column
int	colwidth	# width of the column
int	record		# the current record number

int	ip
int	ctoi(), ctor(), ctowrd()

begin
	# Decode the output value.
	ip = 1
	switch (coltype) {
	case TY_INT: 
	    if (ctoi (value, ip, Memi[colptr+record-1]) <= 0)
		Memi[colptr+record-1] = INDEFI
	case TY_REAL:
	    if (ctor (value, ip, Memr[colptr+record-1]) <= 0)
		Memr[colptr+record-1] = INDEFR
	case TY_BOOL:
	    while (IS_WHITE(value[ip]))
		ip = ip + 1
	    switch (value[ip]) {
	    case 'Y', 'y':
		Memb[colptr+record-1] = true
	    case 'N', 'n':
		Memb[colptr+record-1] = false
	    default:
		Memb[colptr+record-1] = false
	    }
	case TY_CHAR:
	    if (ctowrd (value, ip, Memc[colptr+(record-1)*colwidth],
		colwidth) <= 0)
		Memc[colptr+(record-1)*colwidth] = EOS
	default:
	    ;
	}
end


# PT_FLIPI -- Filp an integer array in place.

procedure pt_flipi (a, npix)

int	a[ARB]		# array to be flipped
int	npix		# number of pixels

int	i, nhalf, ntotal, itemp

begin
	nhalf = npix / 2
	ntotal = npix + 1
	do i = 1, nhalf {
	    itemp = a[i]
	    a[i] = a[ntotal-i]
	    a[ntotal-i] = itemp
	}
end


# PT_RWRECORD -- Read a text record and write it out to the output file.

procedure pt_rwrecord (tp_in, tp_out, line)

int	tp_in		# input file descriptor
int	tp_out		# output file descriptor
char	line[ARB]	# line buffer

int	nchars
int	getline()

begin
	nchars = getline (tp_in, line)
	while (nchars != EOF) {
	    call putline (tp_out, line)
	    if (line[nchars-1] != KY_CHAR_CONT)
		break
	    nchars = getline (tp_in, line)
	}
end
