include "../../lib/ptkeysdef.h"
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

# DP_GNSTPSF -- Procedure to initialize for reading the group file fields from
# a photometry text file . The group file fields are ID, GROUP, X, Y, MAG, ERR,
# and SKY.

procedure dp_gnstpsf (fields, sel_fields, max_nfields)

int	fields[ARB]		# array of selected fields
char	sel_fields[ARB]		# names of selected containing fields
int	max_nfields		# maximum number of fields selected

int	i
int	strlen()

begin
	# Initialize the fields string.
	sel_fields[1] = EOS

	# Encode the fields.
	do i = 1, max_nfields {
	    switch (fields[i]) {
	    case DP_PAPID:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	            call pargstr (ID)
	    case DP_PAPXCEN:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	    	    call pargstr (XCENTER)
	    case DP_PAPYCEN:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	    	    call pargstr (YCENTER)
	    case DP_PAPSKY:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	    	    call pargstr (SKY)
	    case DP_PAPMAG1:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	            call pargstr (MAG)
	    case DP_PAPGROUP:
		call sprintf (sel_fields[strlen(sel_fields)+1], SZ_LINE, "%s ")
	    	    call pargstr (GROUP)
	    }
	}

	# Backspace over the terminating blank character.
	if (sel_fields[1] != EOS)
	    sel_fields[strlen(sel_fields)] = EOS
end


# DP_TNSTINIT -- Procedure to initialize for reading the group file fields from
# a photometry table. The group file fields are ID, GROUP, X, Y, MAG, ERR,
# and SKY.

procedure dp_tnstinit (tp, colpoint)

pointer	tp			# the table descriptor
pointer	colpoint[ARB]		# the column descriptor

begin
	# Get the id.
	# First the ID
	call tbcfnd (tp, ID, colpoint[1], 1)
	if (colpoint[1] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (ID)
	}

	# Get the x position.
	call tbcfnd (tp, XCENTER, colpoint[2], 1)
	if (colpoint[2] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (XCENTER)
	}

	# Get the y position.
	call tbcfnd (tp, YCENTER, colpoint[3], 1)
	if (colpoint[3] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (YCENTER)
	}

	# Get the magnitude.
	call tbcfnd (tp, MAG, colpoint[4], 1)
	if (colpoint[4] == NULL)		# No column
	    call tbcfnd (tp, APMAG, colpoint[4], 1)
	if (colpoint[4] == NULL) {
	    call eprintf ("Column %s not found\n")
	    call pargstr (APMAG)
	}

	# Get the sky.
	call tbcfnd (tp, SKY, colpoint[5], 1)
	if (colpoint[5] == NULL)
	    call tbcfnd (tp, SKY, colpoint[5], 1)
	if (colpoint[5] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (SKY)
	}

	# Get the group number.
	call tbcfnd (tp, GROUP, colpoint[6], 1)
	if (colpoint[6] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (GROUP)
	}
end


# DP_GGROUP -- Read in a single group.

int procedure dp_ggroup (dao, tp, key, fields, indices, colpoint, max_row,
	max_group, in_record, curr_group)

pointer	dao				# pointer to daophot structure
int	tp				# input file/table descriptor
pointer	key				# pointer to text database structure
char	fields[ARB]			# nstar fields to be read
int	indices[ARB]			# array of text file field pointers
int	colpoint[ARB]			# array of column pointers
int	max_row				# number of rows in table
int	max_group			# maximum group size
int	in_record			# pointer to current input record
int	curr_group			# current group number

bool	nullflag
int	istar, group, buf_size
pointer	apsel
int	dp_nstsel()

begin
	# If the current input record is set to zero we are at EOF. In_record
	# is initialized to one on entry to this routine.

	if (in_record == 0)
	    return (0)

	# Get the next group number. Note that the last star read on the
	# previous call is the first star in the new group.

	apsel = DP_APSEL(dao)
	if (in_record == 1) {
	    buf_size = max_group
	    group = 0
	    istar = 0
	} else {
	    Memi[DP_APID(apsel)] = Memi[DP_APID(apsel)+istar-1]
	    Memr[DP_APXCEN(apsel)] = Memr[DP_APXCEN(apsel)+istar-1]
	    Memr[DP_APYCEN(apsel)] = Memr[DP_APYCEN(apsel)+istar-1]
	    Memr[DP_APMAG(apsel)] = Memr[DP_APMAG(apsel)+istar-1]
	    Memr[DP_APMSKY(apsel)] = Memr[DP_APMSKY(apsel)+istar-1]
	    istar = 1
	}
	
	# Loop over the stars in a single group.
	repeat {

	    # Set the current group.
	    curr_group = group

	    # Read in the photometry for a single star.

	    # In this case we have a text database file.
	    if (key != NULL) {

	        if (dp_nstsel (key, tp, fields, indices, Memi[DP_APID(apsel)+
		    istar], group, Memr[DP_APXCEN(apsel)+istar],
		    Memr[DP_APYCEN(apsel)+istar], Memr[DP_APMSKY(apsel)+
		    istar], Memr[DP_APMAG(apsel)+ istar]) == EOF) {
		    in_record = 0
		    break
		}

	    # In this case we have a table.
	    } else {

		if (in_record > max_row) {
		    in_record = 0
		    break
		} else {
		    call tbrgti (tp, colpoint[1], Memi[DP_APID(apsel)+istar],
		        nullflag, 1, in_record)
		    call tbrgtr (tp, colpoint[2], Memr[DP_APXCEN(apsel)+istar],
		        nullflag, 1, in_record)
		    call tbrgtr (tp, colpoint[3], Memr[DP_APYCEN(apsel)+istar],
		        nullflag, 1, in_record)
		    call tbrgtr (tp, colpoint[4], Memr[DP_APMAG(apsel)+istar],
		        nullflag, 1, in_record)
		    call tbrgtr (tp, colpoint[5], Memr[DP_APMSKY(apsel)+istar],
		        nullflag, 1, in_record)
	            call tbrgti (tp, colpoint[6], group, nullflag, 1, in_record)
		}
	    }

	    # Increment the record and star counters.
	    in_record = in_record + 1
	    istar = istar + 1

	    # Allocate more memory as needed.
	    if (istar == buf_size) {
		buf_size = buf_size + max_group
		call dp_rmemapsel (dao, indices, NAPPAR, buf_size + 1)
	    }

	} until ((group != curr_group) && (curr_group != 0))

	# Return the number of stars in the group.
	if (in_record == 0) {
	    if (curr_group == 0)
		curr_group = group
	    return (istar)
	} else
	    return (istar - 1)
end


# DP_NSTSEL --  Read in the required photometry records from a text file.

int procedure dp_nstsel (key, fd, fields, indices, id, group, x, y, sky, mag)

pointer key		# pointer to key structure
int	fd		# text file descriptor
char	fields[ARB]	# fields to be output
int	indices[ARB]	# indices array
int	id		# star id number
int	group		# group number
real	x		# x center
real	y		# y center
real	sky		# sky value
real	mag		# magnitude

int	nchars, nunique, uunique, funique, ncontinue, recptr
int 	first_rec, nselect, record
pointer	line
int	getline(), strncmp(), pt_choose()
data	first_rec /YES/

begin
	# Initialize the file read.
	if (first_rec == YES) {
	    nunique = 0
	    uunique = 0
	    funique = 0
	    nselect = 0
	    record = 0
	    call malloc (line, SZ_LINE, TY_CHAR)
	}

	# Initialize the record read.
	ncontinue = 0
	recptr = 1

	# Loop over the text file records.
	repeat  {

	    # Read in a line of the text file.
	    nchars = getline (fd, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] == KY_CHAR_POUND) {

	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
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
	        }

	    } else if (Memc[line] == KY_CHAR_NEWLINE) {
		# skip blank lines

	    } else {

		# Construct the text file record.
		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct output record when there is no continuation char.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Select the appropriate records.
		    if (nselect <= 0) {
		        nselect = pt_choose (key, fields)
			if (nselect < NAPGROUP) {
			    call eprintf (
			    "The group file does not have the correct format\n")
			    break
			}
		    }

		    # Construct the output record by moving the selected fields
		    # into the data structures.

		    call dp_gnst (key, indices, id, group, x, y, sky, mag)
		    record = record + 1
		    first_rec = NO

		    # Record is complete so exit the loop.
		    break
	        }
	    }

	}

	# Return EOF or the record number.
	if (nchars == EOF || (nselect < NAPGROUP)) {
	    first_rec = YES
	    nunique = 0
	    uunique = 0
	    funique = 0
	    nselect = 0
	    call mfree (line, TY_CHAR)
	    return (EOF)
	} else
	    return (record)
end


# DP_GNST  -- Decode the standard GROUP text file fields into the appropriate
# arrays.

procedure dp_gnst (key, fields, id, group, x, y, sky, mag)

pointer	key		# pointer to keys strucuture
int	fields[ARB]	# fields array
int	id		# star id
int	group		# group id
real	x		# x position
real	y		# y position
real	sky		# sky value
real	mag		# magnitude

int	i, index, elem, maxch, kip, ip
int	ctoi(), ctor()
char	buffer[SZ_LINE]

begin
	do i = 1, KY_NSELECT(key) {

	    # Find the key.
	    index = Memi[KY_SELECT(key)+i-1]
	    elem = Memi[KY_ELEM_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1]
	    kip = Memi[KY_PTRS(key)+index-1] + (elem - 1) * maxch
	    call amovc (Memc[kip], buffer, maxch)
	    buffer[maxch+1] = EOS

	    # Decode the output value.
	    ip = 1
	    switch (fields[i]) {
	    case DP_PAPID: 
		if (ctoi (buffer, ip, id) <= 0)
		    call error (0, "ERROR: Error reading ID field.")
	    case DP_PAPGROUP: 
		if (ctoi (buffer, ip, group) <= 0)
		    call error (0, "ERROR: Error reading GROUP field.")
	    case DP_PAPXCEN:
		if (ctor (buffer, ip, x) <= 0)
		    call error (0, "ERROR: Error reading XCENTER field.")
	    case DP_PAPYCEN:
		if (ctor (buffer, ip, y) <= 0)
		    call error (0, "ERROR: Error reading YCENTER field.")
	    case DP_PAPSKY:
	        if (ctor (buffer, ip, sky) <= 0)
		    call error (0, "ERROR: Error reading MSKY field.")
	    case DP_PAPMAG1:
		if (ctor (buffer, ip, mag) <= 0)
		    call error (0, "ERROR: Error reading MAG field.")
	    default:
		call printf ("Error reading the photometry file.\n")
	    }

	}
end
