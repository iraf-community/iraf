include "../../lib/ptkeysdef.h"

# PT_RENUMBER -- Renumber the input file.

int procedure pt_renumber (tp_in, tp_out, idoffset, id)

int	tp_in		# the input text file descriptor
int	tp_out		# the output text file descriptor
int	idoffset	# the id number offset
char	id[ARB]		# the name of the id column

int	first_rec, nunique, uunique, funique, record
int	ncontinue, recptr, nchars, field
pointer	key, line
int	getline(), strncmp(), pt_kstati()

begin
	# Initialize keyword structure.
	call pt_kyinit (key)

	# Initialize the file read.
	first_rec = YES
	nunique = 0
	uunique = 0
	funique = 0
	record = 0
	call malloc (line, SZ_LINE, TY_CHAR)

	# Initilize the record read.
	ncontinue = 0
	recptr = 1

	# Loop over the text file records.
	repeat  {

	    # Read in a line of the text file.
	    nchars = getline (tp_in, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] == KY_CHAR_POUND) {

	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
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

		# Check that the ID column exists and its datatype is
		# integer.

		if (first_rec == YES) {
		    field = pt_kstati (key, id, KY_INDEX)
		    if (field <= 0)
			break
		    if (pt_kstati (key, id, KY_DATATYPE) != TY_INT)
			break
		}

		# Replace the ID string.
		call pt_idreplace (key, field, 1, Memc[line], nchars,
		    record + idoffset + 1, first_rec, recptr, ncontinue) 

		# Write the output record.
		call putline (tp_out, Memc[line])

	        # Do the record book-keeping.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Increment the record counter.
		    record = record + 1
		    first_rec = NO

		    # Reinitialize the record read.
		    ncontinue = 0
		    recptr = 1
	        }
	    }

	}

	# Cleanup.
	call mfree (line, TY_CHAR)
	call pt_kyfree (key)

	return (record)
end


# PT_IDREPLACE -- Replace the id with the current record number.

procedure pt_idreplace (key, field, element, line, nchars, record, first_rec,
	recptr, ncontinue)

pointer	key		# pointer to record structure
int	field		# field to be fetched
int	element		# field array element
char	line[ARB]	# input line
int	nchars		# length of line array
int	record		# current record number
int	first_rec	# first record
int	recptr		# line per record index
int	ncontinue	# number of unique lines per record

int	len, i, cip, nper_line, nokeys, nckeys, nkeys
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# The number of header columns defined by #K at the beginning of
	# the file is nokeys.
	if (recptr == 1)
	    nokeys = KY_NPKEYS(key)

	# Increment the continuation statement counter or reset to 0.
	if (line[nchars-2] == '*')
	    ncontinue = ncontinue + 1
	else
	    ncontinue = 0

	# Replace the current id with the record number.
	cip = 1
	if (ncontinue < 1) {

	    nper_line = Memi[KY_NPLINE(key)+recptr-1]
	    nkeys = nokeys + nper_line
	    call amovki (int(1), Memi[KY_NELEMS(key)+nokeys], nper_line)

	    do i = nokeys + 1, nkeys {
	    	len = Memi[KY_KINDICES(key)+i-1]
		if (i == field) {
		    call sprintf (Memc[str], SZ_FNAME, "%*s")
			call pargi (-len)
			call pargi (record)
		    call amovc (Memc[str], line[cip], len)
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
		    call sprintf (Memc[str], SZ_FNAME, "%*s")
			call pargi (-len)
			call pargi (record)
		    call amovc (Memc[str], line[cip], len)
		}
		cip = cip + len
	    }

	    nokeys = nkeys
	    recptr = recptr + 1

	} else {

	    if (ncontinue > Memi[KY_NCONTINUE(key)+recptr-2]) {
		Memi[KY_NCONTINUE(key)+recptr-2] =
		    Memi[KY_NCONTINUE(key)+recptr-2] + KY_NLINES
		do i = nckeys, nkeys
		    call realloc (Memi[KY_PTRS(key)+i-1],
		        Memi[KY_NCONTINUE(key)+recptr-2] *
		        Memi[KY_KINDICES(key)+i-1], TY_CHAR)
	    }

	    do i = nckeys, nkeys {
	    	len = Memi[KY_KINDICES(key)+i-1]
		if ((i == field) && (element == ncontinue)) {
		    call sprintf (Memc[str], SZ_FNAME, "%*s")
			call pargi (-len)
			call pargi (record)
		    call amovc (Memc[str], line[cip], len)
		}
		Memi[KY_NELEMS(key)+i-1] = ncontinue
		cip = cip + len
	    }
	}

	call sfree (sp)
end
