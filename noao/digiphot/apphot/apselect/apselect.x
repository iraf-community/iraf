include <fset.h>
include <error.h>
include <evexpr.h>
include "../lib/apkeysdef.h"

define	MAX_LINE	80
define	MAX_NRANGES	50

# AP_SELECT -- Procedure to select records from a text file in pseudo list
# format. This procedure reads through an apphot output file line by line.
# The first lines preceded by #K define the task parameters. These are
# fields which do not often change during the execution of an apphot
# task. The lines beginning with #N describe the fields in the output
# record for each star. Those beginning with #U describe the units of each
# field while those beginning with #F describe the format of each field.
# Blank lines and lines beginning with # are ignored.

procedure ap_select (fd, fields, expr, savepars, format)

int	fd		# text file descriptor
char	fields[ARB]	# fields to be output
char	expr[ARB]	# boolean expression to be evaluated
int	savepars	# save the parameters
int	format		# format the output file

int	nchars, nunique, uunique, funique, ncontinue, first_rec, recptr, nmove
pointer	sp, line, key, outline, o
extern	ap_getop()
int	fstati(), getline(), strmatch(), ap_fmt(), ap_ffmt()
pointer	ap_choose(), evexpr(), locpr()
errchk	evexpr (), ap_getop()

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary space and space for the key structure.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call ap_kyinit (key)

	# Initialize counters.
	nunique = 0
	uunique = 0
	funique = 0

	# Setup record pointers. The first record parameter is set to
	# NO after the first record is read. A record usually consists
	# of several lines of text. Furthermore in some cases the fields
	# of a record may have more than one value, for example the magnitude
	# field. The variable recptr maintains a pointer to the number of
	# unique lines there are in a given record while ncontinue maintains
	# a count of the number of values of a given record field.

	first_rec = YES
	recptr = 1
	ncontinue = 0

	# Set output buffer pointer to NULL.
	outline = NULL

	# Loop over the text file records.
	nchars = getline (fd, Memc[line])
	while (nchars != EOF) {

	    # Determine the type of record. Add keywords to the database.
	    if (strmatch (Memc[line], "^\#K ") != 0) {
		call ap_kyadd (key, Memc[line], nchars)
		if (savepars == YES)
		    call putline (STDOUT, Memc[line])
	    } else if (strmatch (Memc[line], "^\#N ") != 0) {
		nunique = nunique + 1
		call ap_kname (key, Memc[line], nchars, nunique)
	    } else if (strmatch (Memc[line], "^\#U ") != 0) {
		uunique = uunique + 1
		call ap_knunits (key, Memc[line], nchars, uunique)
	    } else if (strmatch (Memc[line], "^\#F ") != 0) {
		funique = funique + 1
		call ap_knformats (key, Memc[line], nchars, funique)
	    } else if (strmatch (Memc[line], "^\#") != 0) {
		# Skip lines beginning with # sign
	    } else if (strmatch (Memc[line], "^\n") != 0) {
		# Skip lines beginning with a newline
	    } else {

		# Construct the record. This routine is called repeatedly
		# until a record without the continuation character \\
		# is encountered.

		call ap_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct output record when there is no continuation char.
	        if (Memc[line+nchars-2] != '\\') {

		    # Select the appropriate records and compute the size of
		    # the output buffer. This routine only needs to be called
		    # once.

		    if (outline == NULL) {
		        outline = ap_choose (key, fields)
			if (outline == NULL)
			    break
		    } else
			Memc[outline] = EOS

		    # Evaluate the expression.
		    iferr {
		    	call ap_getset (key)
		        o = evexpr (expr, locpr (ap_getop), 0)
		    } then {
			call erract (EA_WARN)
			break
		    }
		    if (O_TYPE(o) != TY_BOOL)
			call error (0, "Expression must be a boolean")

		    # Construct the output record.
		    if (O_VALB(o)) {
		        if (format == YES)
		            nmove = ap_fmt (key, Memc[outline], MAX_LINE)
		        else
		            nmove = ap_ffmt (key, Memc[outline], MAX_LINE)
		        call putline (STDOUT, Memc[outline])
		    }

		    # Get ready for next record.
		    first_rec = NO
		    recptr = 1
		    ncontinue = 0
		    call xev_freeop (o)
		    call mfree (o, TY_STRUCT)
	        }
	    }

	    # Read the next line.
	    nchars = getline (fd, Memc[line])
	}

	# Free space.
	call ap_kyfree (key)
	if (outline != NULL)
	    call mfree (outline, TY_CHAR)
	call sfree (sp)
end


# AP_MKREC -- Construct the output record.

procedure ap_mkrec (key, line, nchars, first_rec, recptr, ncontinue)

pointer	key		# pointer to record structure
char	line[ARB]	# input line
int	nchars		# length of line array
int	first_rec	# first record read
int	recptr		# line per record index
int	ncontinue	# number of unique lines per record

int	i, count, cip, nokeys, nckeys, nkeys
pointer	op

begin
	# Compute the starting point for the object records and
	# for the presence of the array field continuation character.
	if (recptr == 1)
	    nokeys = KY_NOKEYS(key)
	if (line[nchars-2] == '*')
	    ncontinue = ncontinue + 1
	else
	    ncontinue = 0

	# Reset the memory allocate counter if this is the first record
	# and this is either the first field or the ncontinue character
	if (first_rec == YES && (recptr == 1 || ncontinue == 1))
	    count = NLINES

	# Fill in the record.
	cip = 1
	if (ncontinue < 1) {
	    nkeys = nokeys + Memi[KY_NPLINE(key)+recptr-1]
	    do i = nokeys + 1, nkeys {
	        Memi[KY_NELEMS(key)+i-1] = 1
		op = Memi[KY_PTRS(key)+i-1]
	        call amovc (line[cip], Memc[op], Memi[KY_KINDICES(key)+i-1]) 
		cip = cip + Memi[KY_KINDICES(key)+i-1]
	    }
	    recptr = recptr + 1
	    nokeys = nkeys
	} else if (ncontinue == 1) {
	    nckeys = nokeys + 1
	    nkeys = nokeys + Memi[KY_NPLINE(key)+recptr-1]
	    do i = nckeys, nkeys {
		if (first_rec == YES)
		    call malloc (Memi[KY_PTRS(key)+i-1], count *
		        Memi[KY_KINDICES(key)+i-1], TY_CHAR)
		call amovc (line[cip], Memc[Memi[KY_PTRS(key)+i-1]],
		    Memi[KY_KINDICES(key)+i-1])
		cip = cip + Memi[KY_KINDICES(key)+i-1]
	    }
	    nokeys = nkeys
	    recptr = recptr + 1
	} else {
	    if (first_rec == YES && ncontinue > count) {
		count = count + NLINES
		do i = nckeys, nkeys
		    call realloc (Memi[KY_PTRS(key)+i-1], count *
		        Memi[KY_KINDICES(key)+i-1], TY_CHAR)
	    }
	    do i = nckeys, nkeys {
		op = Memi[KY_PTRS(key)+i-1] + (ncontinue - 1) *
		    Memi[KY_KINDICES(key)+i-1]
		call amovc (line[cip], Memc[op], Memi[KY_KINDICES(key)+i-1])
		Memi[KY_NELEMS(key)+i-1] = ncontinue
		cip = cip + Memi[KY_KINDICES(key)+i-1]
	    }
	}
end


# AP_CHOOSE -- Determine which fields are to be selected.

pointer procedure ap_choose (key, fields)

pointer	key		# pointer to key structure
char	fields[ARB]	# fields to be evaluated

int	max_nkeys, nkeys, index, nelems, element, szbuf
pointer	list, sp, kname, aranges, ranges, rangeset, buf
int	ap_gnfn(), ap_ranges(), strdic(), decode_ranges(), get_next_number()
pointer	ap_ofnl()
real	asumi()

begin
	# Allocate buffer space
	call smark (sp)
	call salloc (kname, SZ_PAR, TY_CHAR)
	call salloc (aranges, SZ_FNAME, TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)
	call salloc (rangeset, 3 * MAX_NRANGES + 1, TY_INT)

	# Allocate space for the select buffer. Space equal to the number
	# of keys in the database is allocated. Allowance must be made for
	# array subsripts.
	max_nkeys = int (asumi (Memi[KY_NELEMS(key)], KY_NKEYS(key))) + 1
	if (KY_SELECT(key) != NULL)
	    call mfree (KY_SELECT(key), TY_INT)
	call malloc (KY_SELECT(key), max_nkeys, TY_INT)
	if (KY_LEN_SELECT(key) != NULL)
	    call mfree (KY_LEN_SELECT(key), TY_INT)
	call malloc (KY_LEN_SELECT(key), max_nkeys, TY_INT)

	# Loop through the fields list.
	nkeys = 0
	szbuf = 0
	list = ap_ofnl (key, fields)
	while (ap_gnfn (list, Memc[kname], Memc[aranges], SZ_PAR) != EOF) {

	    # Find the field name and the ranges.
	    index = strdic (Memc[kname], Memc[kname], SZ_PAR,
	        Memc[KY_WORDS(key)])
	    if (ap_ranges (Memc[aranges], Memc[ranges], element,
	        SZ_FNAME) == ERR)
		call error (0, "Cannot decode apphot range string")

	    # Load the fields.
	    if (index == 0)
		next
	    else if (Memi[KY_NELEMS(key)+index-1] == 1) {
	        Memi[KY_SELECT(key)+nkeys] = Memi[KY_PTRS(key)+index-1]
		Memi[KY_LEN_SELECT(key)+nkeys] = Memi[KY_KINDICES(key)+index-1]
	        nkeys = nkeys + 1
	        szbuf = szbuf + Memi[KY_KINDICES(key)+index-1] + 2
	    } else {
		if (Memc[ranges] == EOS) {
		    call sprintf (Memc[ranges], SZ_FNAME, "1-%d")
			call pargi (Memi[KY_NELEMS(key)+index-1])
		}
		if (decode_ranges (Memc[ranges], Memi[rangeset], MAX_NRANGES,
		    nelems) == ERR)
		    call error (0, "Cannot decode ranges string")
		nelems = 0
		while (get_next_number (Memi[rangeset], nelems) != EOF) {
		    if (nelems < 1 || nelems > Memi[KY_NELEMS(key)+index-1])
			break
		    Memi[KY_SELECT(key)+nkeys] = Memi[KY_PTRS(key)+index-1] +
			(nelems - 1) * Memi[KY_KINDICES(key)+index-1]
		    Memi[KY_LEN_SELECT(key)+nkeys] = Memi[KY_KINDICES(key)+
		        index-1]
		    nkeys = nkeys + 1
	            szbuf = szbuf + Memi[KY_KINDICES(key)+index-1] + 2
		}
	    }
	}

	# Reallocate and initialize the select buffer space.
	if (nkeys > 0) {
	    KY_NSELECT(key) = nkeys
	    call realloc (KY_SELECT(key), KY_NSELECT(key), TY_INT)
	    call realloc (KY_LEN_SELECT(key), KY_NSELECT(key), TY_INT)
	    call malloc (buf, szbuf + 4, TY_CHAR)
	    Memc[buf] = EOS
	} else
	    buf = NULL

	# Free the space.
	call ap_cfnl (list)
	call sfree (sp)

	return (buf)
end


# AP_FFMT -- Procedure to free format an apphot output record.

int procedure ap_ffmt (key, line,  maxline)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	maxline		# add new line every max lines

int	i, op, kip, nk, maxch

pointer	sp, str
begin

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	op = 1
	do i = 1, KY_NSELECT(key) {

	    # Find the key.
	    kip = Memi[KY_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1] 

	    # Trim leading whitespace.
	    for (nk = 0; Memc[kip] == ' ' && nk < maxch; nk = nk + 1)
		kip = kip + 1

	    # Trim trailing whitesapce.
	    for (nk = 0; Memc[kip+nk] != ' ' && nk < maxch; nk = nk + 1)
		;

	    # Copy value to output buffer.
	    call amovc (Memc[kip], line[op], nk)
	    op = op + nk
	    line[op] = ' '
	    op = op + 1
	    line[op] = ' '
	    op = op + 1
	}

	line[op-2] = '\n'
	line[op-1] = EOS
	call sfree (sp)
	return (op - 2)
end


# AP_FMT -- Procedure to format an apphot output record.

int procedure ap_fmt (key, line,  maxline)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	maxline		# add new line every max lines

int	i, op, kip, maxch

begin
	# Add leading three blanks.
	call strcpy ("   ", line[1], 3)

	# Move records.
	op = 4
	do i = 1, KY_NSELECT(key) {

	    # Find the key.
	    kip = Memi[KY_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1] 

	    # Copy value to output buffer.
	    call amovc (Memc[kip], line[op], maxch)
	    op = op + maxch
	}

	line[op] = '\n'
	line[op+1] = EOS
	return (op)
end


# AP_RANGES -- Procedure to convert apphot ranges to the format expected
# by the xtools ranges package.

int procedure ap_ranges (aranges, ranges, element, maxch)

char	aranges[ARB]	# input ranges
char	ranges[ARB]	# output ranges
int	element		# range element
int	maxch		# maximum number of characters in ranges

char	left_bkt, right_bkt
int	findex, lindex, nchars, ip
int	stridx(), ctoi()
data	left_bkt /'['/, right_bkt /']'/

begin
	# Test for existence of ranges.
	element = 1
	ranges[1] = EOS
	if (aranges[1] == EOS)
	    return (OK)

	# Test for range delimiters.
	findex = stridx (left_bkt, aranges)
	lindex = stridx (right_bkt, aranges)
	if (findex == 0 || lindex == 0 || (lindex <= findex + 1))
	    return (ERR)

	# Compute the element selection.
	ip = 1
	nchars = ctoi (aranges[findex+1], ip, element)
	if (nchars == 0)
	    element = 1

	# Copy the ranges portion.
	call strcpy (aranges[findex+1], ranges, lindex - findex - 1)
	return (OK)
end
