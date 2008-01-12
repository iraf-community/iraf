include <fset.h>
include <error.h>
include <evexpr.h>
include "../../lib/ptkeysdef.h"

# PT_XDUMP -- Procedure to select records from a text file in apphot/daophot
# format. This procedure reads through an apphot/daophot output file line by
# line. The lines preceded by #K define the task parameters. These are
# fields which do not often change during the execution of an apphot/daophot
# task. The lines beginning with #N describe the fields in the output
# record which change for each star. The lines beginning with #U describe
# the units of each field, while those beginning with #F describe the format
# of each field. Blank lines and lines beginning with # are ignored.

procedure pt_xdump (fd, fields, expr, headers, parameters)

int	fd		# text file descriptor
char	fields[ARB]	# fields to be output
char	expr[ARB]	# boolean expression to be evaluated
int	headers		# format the output file ?
int	parameters	# dump the parameters ?

bool	oexpr
int	nchars, nunique, uunique, funique, ncontinue, first_rec, recptr
int	nselect, szbuf, nmove, printall
pointer	sp, line, key, outline, o

bool	streq()
extern	pt_getop()
int	fstati(), getline(), strncmp(), pt_kszselbuf, pt_fmt(), pt_ffmt()
pointer	pt_choose(), evexpr(), locpr()
errchk	evexpr (), pt_getop()

begin
	# If the output has been redirected do not flush on a newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary space and space for the keyword structure.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call pt_kyinit (key)

	# Initialize counters.
	nunique = 0
	uunique = 0
	funique = 0

	# Setup the record counters. First_rec is set to NO after the first
	# record is read. A record usually consists of several lines of text.
	# A '\' in column 80 indicats that the current line is a part of
	# the previous record. If there is a blank in column 80 the current
	# line terminates the current record. In some cases the fields of a
	# record may have more than one value, for example the magnitude field.
	# These lines are marked by a '*\' in columns 79 and 80 respectively.

	# The variable first_rec is set to NO after the first record is read.
	# The variable recptr maintains a counter of the number of unique
	# lines there are in a given record while ncontinue maintains a count
	# of the number of times a given portion of the record is repeated.

	# Initialize the record reading.
	outline = NULL
	first_rec = YES
	recptr = 1
	ncontinue = 0

	# Initialize the expression decoding.
	o = NULL
	if (streq (expr, "yes")) {
	    oexpr = true
	    printall = YES
	} else {
	    oexpr = false
	    printall = NO
	}

	# Loop over the text file records.
	nchars = getline (fd, Memc[line])
	while (nchars != EOF) {

	    # Determine the type of record. Add keywords to the database.
	    if (Memc[line] ==  KY_CHAR_POUND) {

	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
		    if (headers == YES && parameters == YES)
		        call putline (STDOUT, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_NAME,
		    KY_LEN_STR) == 0) {
		    nunique = nunique + 1
		    call pt_kname (key, Memc[line], nchars, nunique)
	        } else if (strncmp (Memc[line], KY_CHAR_UNITS,
		    KY_LEN_STR) == 0) {
		    uunique = uunique + 1
		    call pt_knunits (key, Memc[line], nchars, uunique)
	        } else if (strncmp (Memc[line], KY_CHAR_FORMAT,
		    KY_LEN_STR) == 0) {
		    funique = funique + 1
		    call pt_knformats (key, Memc[line], nchars, funique)
	        } else if (headers == YES && parameters == YES) {
		    if (nunique == 0 || nunique != uunique || nunique !=
			    funique)
		    call putline (STDOUT, Memc[line])
	        }

	    } else if (Memc[line] ==  KY_CHAR_NEWLINE) {
		# Skip lines beginning with a newline

	    } else {

		# Construct the record. This routine is called repeatedly
		# until a record without the continuation character is
		# encountered.

		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct the output record when there is no terminating
		# continuation char.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Select the appropriate records and compute the size of
		    # the output buffer. This routine only needs to be called
		    # once.

		    if (outline == NULL) {
		        nselect = pt_choose (key, fields)
			if (nselect <= 0)
			    break
			szbuf = pt_kszselbuf (key)
			call malloc (outline, szbuf, TY_CHAR)
		    } else
			Memc[outline] = EOS

		    # Evaluate the expression.
		    iferr {
			if (printall == NO) {
		    	    call pt_apset (key)
		            o = evexpr (expr, locpr (pt_getop), 0)
		            if (O_TYPE(o) != TY_BOOL)
			        call error (0, "Expression must be a boolean")
			    oexpr = O_VALB(o)
			} 
		    } then {
			call erract (EA_WARN)
			call error (0, "Error evaluating selection expression")
		    }

		    if (first_rec == YES && headers == YES) {
			call pt_fnstr (key, Memc[outline], szbuf)
		        call putline (STDOUT, Memc[outline])
			call pt_fustr (key, Memc[outline], szbuf)
		        call putline (STDOUT, Memc[outline])
			call pt_ffstr (key, Memc[outline], szbuf)
		        call putline (STDOUT, Memc[outline])
			call putline (STDOUT, "#\n")
		    }

		    # Construct the output record.
		    if (oexpr) {
		        if (headers == YES)
		            nmove = pt_fmt (key, Memc[outline], szbuf)
		        else
		            nmove = pt_ffmt (key, Memc[outline], szbuf)
			if (nmove > 0)
		            call putline (STDOUT, Memc[outline])
		    }

		    # Get ready for next record.
		    first_rec = NO
		    recptr = 1
		    ncontinue = 0
		    if (o != NULL) {
			call xev_freeop (o)
		        call mfree (o, TY_STRUCT)
		    }
	        }
	    }

	    # Read the next line.
	    nchars = getline (fd, Memc[line])
	}

	# Free space.
	call pt_kyfree (key)
	if (outline != NULL)
	    call mfree (outline, TY_CHAR)
	call sfree (sp)
end


# PT_FMT -- Procedure to format an apphot/daophot output record.

int procedure pt_fmt (key, line, szbuf)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	szbuf		# maximum buffer size

char	blank
int	i, op, kip, nk, index, elem, maxch
data	blank /' '/

begin
	# Add leading three blanks.
	call strcpy ("   ", line[1], 3)

	# Move records.
	op = 4
	do i = 1, KY_NSELECT(key) {

	    # Find the key.
	    index = Memi[KY_SELECT(key)+i-1]
	    elem = Memi[KY_ELEM_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1] 
	    kip = Memi[KY_PTRS(key)+index-1] + (elem - 1) * maxch

	    # Trim leading whitespace.
	    for (nk = 0; Memc[kip] == ' ' && nk < maxch; nk = nk + 1)
		kip = kip + 1

	    # Check the buffer size.
	    if ((op + maxch) >= szbuf)
	        break

	    # Copy value to output buffer.
	    call amovc (Memc[kip], line[op], maxch - nk)
	    op = op + maxch - nk
	    call amovkc (blank, line[op], nk)
	    op = op + nk
	}

	line[op] = '\n'
	line[op+1] = EOS
	return (op)
end


# PT_FFMT -- Procedure to free format an apphot output record.

int procedure pt_ffmt (key, line, szbuf)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	szbuf		# size of the output buffer

int	i, op, kip, nk, index, elem, maxch

begin
	op = 1
	do i = 1, KY_NSELECT(key) {

	    # Find the key.
	    index = Memi[KY_SELECT(key)+i-1]
	    elem = Memi[KY_ELEM_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1] 
	    kip = Memi[KY_PTRS(key)+index-1] + (elem - 1) * maxch

	    # Trim leading whitespace.
	    for (nk = 0; Memc[kip] == ' ' && nk < maxch; nk = nk + 1)
		kip = kip + 1

	    # Trim trailing whitesapce.
	    for (nk = 0; Memc[kip+nk] != ' ' && nk < maxch; nk = nk + 1)
		;

	    # Check buffer space.
	    if ((op + nk + 2) >= szbuf)
		break

	    # Copy value to output buffer.
	    call amovc (Memc[kip], line[op], nk)
	    op = op + nk
	    line[op] = ' '
	    op = op + 1
	    line[op] = ' '
	    op = op + 1
	}

	if (op > 1) {
	    line[op-2] = '\n'
	    line[op-1] = EOS
	    return (op - 2)
	} else {
	    line[1] = EOS
	    return (0)
	}
end


# PT_FNSTR -- Format an apphot/daophot selected name string.

procedure pt_fnstr (key, line,  maxline)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	maxline		# add new line every max lines

int	op, nchars
int	gstrcpy()

begin
	# Add leading three characters.
	call strcpy (KY_CHAR_NAME, line[1], KY_LEN_STR)

	# Copy the selected name string.
	op = KY_LEN_STR + 1
	nchars = gstrcpy (Memc[KY_NAME_SELECT(key)], line[op], maxline -
	    KY_LEN_STR)

	# Add the newline and EOS character.
	op = op + nchars
	line[op] = '\n'
	line[op+1] = EOS
end


# PT_FUSTR -- Format an apphot/daophot selected units string.

procedure pt_fustr (key, line,  maxline)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	maxline		# add new line every max lines

int	op, nchars
int	gstrcpy()

begin
	# Add leading three blanks.
	op = 1
	call strcpy (KY_CHAR_UNITS, line[op], KY_LEN_STR)

	# Copy the selected name string.
	op = KY_LEN_STR + 1
	nchars = gstrcpy (Memc[KY_UNIT_SELECT(key)], line[op], maxline -
	    KY_LEN_STR)

	# Add the newline and EOS character.
	op = op + nchars
	line[op] = '\n'
	line[op+1] = EOS
end


# PT_FFSTR -- Format an apphot selected format string.

procedure pt_ffstr (key, line,  maxline)

pointer	key		# pointer to keys strucuture
char	line[ARB]	# output line
int	maxline		# add new line every max lines

char	ctype
int	fwidth, prec, type, op, nchars
pointer	sp, format 
int	pt_kyfstr(), gstrcpy()

begin
	fwidth = Memi[KY_LEN_SELECT(key)]
	call smark (sp)
	call salloc (format, fwidth, TY_CHAR)

	# Adjust the format of the first field to correct for the three
	# blanks.
	call strcpy (Memc[KY_FMT_SELECT(key)], Memc[format], fwidth)
	if (pt_kyfstr (Memc[format], fwidth, prec, type, ctype) != ERR) {
	    if (type == TY_REAL) {
		Memc[format] = '%'
	        call sprintf (Memc[format+1], fwidth - 1, "%d.%d%c%*t") 
		    call pargi (-(fwidth+KY_LEN_STR))
		    call pargi (prec)
		    call pargc (ctype)
		    call pargi (fwidth)
	    } else {
		Memc[format] = '%'
	        call sprintf (Memc[format+1], fwidth - 1, "%d%c%*t") 
		    call pargi (-(fwidth+KY_LEN_STR))
		    call pargc (ctype)
		    call pargi (fwidth)
	    }
	    call amovc (Memc[format], Memc[KY_FMT_SELECT(key)], fwidth)
	}

	# Add leading three blanks.
	op = 1
	call strcpy (KY_CHAR_FORMAT, line[op], KY_LEN_STR)

	# Copy the selected name string.
	op = KY_LEN_STR + 1
	nchars = gstrcpy (Memc[KY_FMT_SELECT(key)], line[op], maxline -
	    KY_LEN_STR)

	# Add the newline and EOS character.
	op = op + nchars
	line[op] = '\n'
	line[op+1] = EOS

	call sfree (sp)
end


# PT_KSZSELBUF -- Compute the buffer size required to hold the output selected
# line.

int procedure pt_kszselbuf (key)

pointer	key			# pointer to the keyword structure

int	i, szbuf

begin
	szbuf = 0

	do i = 1, KY_NSELECT(key)
	    szbuf = szbuf + Memi[KY_LEN_SELECT(key)+i-1] + 2
	szbuf = szbuf + 4

	return (szbuf)
end
