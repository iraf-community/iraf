include	<tbset.h>
include "../../lib/ptkeysdef.h"
include	"../lib/daophotdef.h"
include	"../lib/apseldef.h"

# DP_WGETAPERT -- Read the aperture photometry results and transform the
# input coordinates to the appropriate coordinate system. Works with
# either the "old" APPHOT files or the new ST Tables.

procedure dp_wgetapert (dao, im, apd, max_nstars, old_ap)

pointer	dao			# pointer to the DAOPHOT structure
pointer	im			# the input image descriptor
int	apd			# input photometry file descriptor
int	max_nstars		# maximum number of stars
bool	old_ap			# YES indicates old APPHOT file

pointer	apsel
int	dp_stati()

begin
	# Get the stars.
	call dp_getapert (dao, apd, max_nstars, old_ap)

	# Transform the coordinates if necessary.
	apsel = DP_APSEL (dao)
	if (dp_stati (dao, WCSIN) != WCS_LOGICAL)
	    call dp_win (dao, im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APXCEN(apsel)],
		Memr[DP_APYCEN(apsel)], DP_APNUM(apsel))
end


# DP_GETAPERT -- Read the aperture photometry results. Works with
# either the "old" APPHOT files or the new ST Tables.

procedure dp_getapert (dao, apd, max_nstars, old_ap)

pointer	dao			# pointer to the DAOPHOT structure
int	apd			# input Photometry file descriptor
int	max_nstars		# maximum number of stars
bool	old_ap			# YES indicates old APPHOT file

int	nstars
pointer	apsel
int	tbpsta(), dp_goldap(), dp_gtabphot()

begin
	# Get APSEL pointer.
	apsel = DP_APSEL (dao)

	# Get the required memory.
	Memi[DP_APRESULT(apsel)] = DP_PAPID
	Memi[DP_APRESULT(apsel)+1] = DP_PAPXCEN
	Memi[DP_APRESULT(apsel)+2] = DP_PAPYCEN
	Memi[DP_APRESULT(apsel)+3] = DP_PAPMAG1
	Memi[DP_APRESULT(apsel)+4] = DP_PAPSKY

	# Get the results.
	if (old_ap) {
	    call dp_memapsel (dao, Memi[DP_APRESULT(apsel)], NAPRESULT,
	        max_nstars)
	    nstars = dp_goldap (apd, dao, max_nstars)
	} else {
	    call dp_memapsel (dao, Memi[DP_APRESULT(apsel)], NAPRESULT,
	        tbpsta (apd, TBL_NROWS))
	    nstars = dp_gtabphot (apd, dao, tbpsta (apd, TBL_NROWS))
	}

	# Reallocate to save space if appropopriate.
	if (nstars < max_nstars)
	    call dp_rmemapsel (dao, Memi[DP_APRESULT(apsel)], NAPRESULT,
	        nstars)
end


# DP_MEMAPSEL -- Procedure to allocate memory for the apselect strucuture.

procedure dp_memapsel (dao, fields, nfields, max_nstars)

pointer	dao		# pointer to the daophot strucuture
int	fields[ARB]	# array of fields
int	nfields		# number of fields to allocate space for
int	max_nstars	# maximum number of stars

int	i
pointer	apsel

begin
	apsel = DP_APSEL(dao)

	# Allocate space for results.
	do i = 1, nfields {
	    switch (fields[i]) {
	    case DP_PAPID:
	        if (DP_APID(apsel) != NULL)
	            call mfree (DP_APID(apsel), TY_INT)
	        call malloc (DP_APID(apsel), max_nstars, TY_INT)

	    case DP_PAPXCEN:
	        if (DP_APXCEN(apsel) != NULL)
	            call mfree (DP_APXCEN(apsel), TY_REAL)
	        call malloc (DP_APXCEN(apsel), max_nstars, TY_REAL)

	    case DP_PAPYCEN:
	        if (DP_APYCEN(apsel) != NULL)
	            call mfree (DP_APYCEN(apsel), TY_REAL)
	        call malloc (DP_APYCEN(apsel), max_nstars, TY_REAL)

	    case DP_PAPSKY:
	        if (DP_APMSKY(apsel) != NULL)
	            call mfree (DP_APMSKY(apsel), TY_REAL)
	        call malloc (DP_APMSKY(apsel), max_nstars, TY_REAL)

	    case DP_PAPMAG1:
		if (DP_APMAG(apsel) != NULL)
	    	    call mfree (DP_APMAG(apsel), TY_REAL)
		call malloc (DP_APMAG(apsel), max_nstars, TY_REAL)

	    case DP_PAPGROUP:
		if (DP_APGROUP(apsel) != NULL)
	    	    call mfree (DP_APGROUP(apsel), TY_INT)
		#call malloc (DP_APGROUP(apsel), max_nstars, TY_INT)

	    case DP_PAPMERR1:
		if (DP_APERR(apsel) != NULL)
	    	    call mfree (DP_APERR(apsel), TY_REAL)
		call malloc (DP_APERR(apsel), max_nstars, TY_REAL)

	    case DP_PAPNITER:
		if (DP_APNITER(apsel) != NULL)
	    	    call mfree (DP_APNITER(apsel), TY_INT)
		#call malloc (DP_APNITER(apsel), max_nstars, TY_INT)

	    case DP_PAPCHI:
		if (DP_APCHI(apsel) != NULL)
	    	    call mfree (DP_APCHI(apsel), TY_REAL)
		call malloc (DP_APCHI(apsel), max_nstars, TY_REAL)

	    case DP_PAPSHARP:
		if (DP_APSHARP(apsel) != NULL)
	    	    call mfree (DP_APSHARP(apsel), TY_REAL)
		call malloc (DP_APSHARP(apsel), max_nstars, TY_REAL)
	    }
	}
end


# DP_GOLDAP -- Read in the photometry from an old style APPHOT file

int procedure  dp_goldap (apd, dao, max_nstars)

int	apd		# the input file descriptor
pointer	dao		# pointer to the daophot structure
int	max_nstars	# maximum number of stars

int	nstars, bufsize, stat
pointer	apsel, apkey, sp, fields
int	dp_apsel()

begin
	# Define the point to the apselect structure.
	apsel = DP_APSEL (dao)

	# Allocate some temporary space.
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)

	# Initialize the keyword structure.
	call pt_kyinit (apkey)

	# Set up the fields to be retrieved.
	call dp_gappsf (Memi[DP_APRESULT(apsel)], Memc[fields], NAPRESULT)

	# Now read in the results.
	nstars = 0
	bufsize = max_nstars
	repeat {

	    # Read in a group of stars.
	    while (nstars < bufsize) {
	        stat = dp_apsel (apkey, apd, Memc[fields],
		    Memi[DP_APRESULT(apsel)], Memi[DP_APID(apsel)+nstars],
	            Memr[DP_APXCEN(apsel)+nstars],
		    Memr[DP_APYCEN(apsel)+nstars],
	            Memr[DP_APMSKY(apsel)+nstars],
		    Memr[DP_APMAG(apsel)+nstars])
		if (stat == EOF)
		    break
	        nstars = nstars + 1
	    }

	    # Check the buffer size.
	    if (stat == EOF)
		break
	    bufsize = bufsize + max_nstars
	    call dp_rmemapsel (dao, Memi[DP_APRESULT(apsel)], NAPRESULT,
	        bufsize)

	}
	DP_APNUM(apsel) = nstars

	# Free the keyword structure.
	call pt_kyfree (apkey)
	call sfree (sp)

	return (nstars)
end


# DP_GTABPHOT -- Read in the complete photometry from an ST table.

int procedure dp_gtabphot (tp, dao, max_nstars)

pointer	tp			# table descriptor
pointer	dao			# pointer to daophot structure
int	max_nstars		# maximum number of stars

bool	nullflag
int	record, index, nrow
pointer	apsel, idpt, xcenpt, ycenpt, magpt, skypt
int	tbpsta()

begin
	# Define the point to the apselect structure.
	apsel = DP_APSEL (dao)

	# Find the column pointers
	call tbcfnd (tp, ID, idpt, 1)
	if (idpt == NULL)
	    call tbcfnd (tp, "ID", idpt, 1)
	if (idpt == NULL)
	    call printf ("Error reading ID.\n")

	call tbcfnd (tp, XCENTER, xcenpt, 1)
	if (xcenpt == NULL)
	    call tbcfnd (tp, "XCENTER", xcenpt, 1)
	if (xcenpt == NULL)
	    call printf ("Error reading XCENTER.\n")

	call tbcfnd (tp, YCENTER, ycenpt, 1)
	if (ycenpt == NULL)
	    call tbcfnd (tp, "YCENTER", ycenpt, 1)
	if (ycenpt == NULL)
	    call printf ("Error reading YCENTER.\n")

	call tbcfnd (tp, MAG, magpt, 1)
	if (magpt == NULL)
	    call tbcfnd (tp, APMAG, magpt, 1)
   	if (magpt == NULL)
	    call printf ("Error reading MAG.\n")

	call tbcfnd (tp, SKY, skypt, 1)
	if (skypt == NULL)
	    call tbcfnd (tp, SKY, skypt, 1)
	if (skypt == NULL)
	    call printf ("Error reading SKY.\n")


	# Get the results ignoring any record with ID = NULL.
	nrow = min (tbpsta (tp, TBL_NROWS), max_nstars)
	index = 0
	do record = 1, nrow {

	    # Check the ID record.
	    call tbrgti (tp, idpt, Memi[DP_APID(apsel)+index], nullflag, 1,
	        record)
	    if (nullflag)
		next

	    # Read the remaining records.
	    call tbrgtr (tp, xcenpt, Memr[DP_APXCEN(apsel)+index], nullflag, 1,
	        record)
	    call tbrgtr (tp, ycenpt, Memr[DP_APYCEN(apsel)+index], nullflag, 1,
	        record)
	    call tbrgtr (tp, magpt, Memr[DP_APMAG(apsel)+index], nullflag, 1,
	        record)
	    call tbrgtr (tp, skypt, Memr[DP_APMSKY(apsel)+index], nullflag, 1,
	        record)
	    index = index + 1
	}

	DP_APNUM(apsel) = index

	return (index)
end


# DP_RMEMAPSEL -- Procedure to reallocate memory for the apselect strucuture.

procedure dp_rmemapsel (dao, fields, nfields, max_nstars)

pointer	dao		# pointer to the daophot strucuture
int	fields[ARB]	# integer fields
int	nfields		# number of fields
int	max_nstars	# maximum number of stars

int	i
pointer	apsel

begin
	# Reallocate space for results.
	apsel = DP_APSEL(dao)

	do i = 1,  nfields {
	    switch (fields[i]) {
	    case DP_PAPID:
	        call realloc (DP_APID(apsel), max_nstars, TY_INT)
	    case DP_PAPXCEN:
	        call realloc (DP_APXCEN(apsel), max_nstars, TY_REAL)
	    case DP_PAPYCEN:
	        call realloc (DP_APYCEN(apsel), max_nstars, TY_REAL)
	    case DP_PAPSKY:
	        call realloc (DP_APMSKY(apsel), max_nstars, TY_REAL)
	    case DP_PAPGROUP:
		#call realloc (DP_APGROUP(apsel), max_nstars, TY_INT)
	    case DP_PAPMAG1:
	        call realloc (DP_APMAG(apsel), max_nstars, TY_REAL)
	    case DP_PAPMERR1:
	        call realloc (DP_APERR(apsel), max_nstars, TY_REAL)
	    case DP_PAPNITER:
	        #call realloc (DP_APNITER(apsel), max_nstars, TY_INT)
	    case DP_PAPSHARP:
	        call realloc (DP_APSHARP(apsel), max_nstars, TY_REAL)
	    case DP_PAPCHI:
	        call realloc (DP_APCHI(apsel), max_nstars, TY_REAL)
	    }
	}
end


# DP_GAPPSF -- Set up the structures necessary for retrieving the
# aperture phtometery results needed for the PSF task.

procedure dp_gappsf (fields, sel_fields, max_nfields)

int	fields[ARB]		# array of selected fields
char	sel_fields[ARB]		# names of selected containing fields
int	max_nfields		# maximum number of fields selected

int	i
int	strlen()

begin
	sel_fields[1] = EOS

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
	            call pargstr (APMAG)
	    }
	}

	if (sel_fields[1] != EOS)
	    sel_fields[strlen(sel_fields)] = EOS
end


# DP_APSEL -- Select records from an apphot/daophot text file.

int procedure dp_apsel (key, fd, fields, indices, id, x, y, sky, mag)

pointer key		# pointer to key structure
int	fd		# text file descriptor
char	fields[ARB]	# fields to be output
int	indices[ARB]	# indices of fields
int	id		# star id number
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

		# Construct the table record.
		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct output record when there is no continuation char.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Select the appropriate records.
		    if (nselect <= 0) {
		        nselect = pt_choose (key, fields)
			if (nselect <= 0)
			    break
		    }

		    # Construct the output record by moving selected fields
		    # into data structures.
		    call dp_getap (key, indices, id, x, y, sky, mag)
		    record = record + 1
		    first_rec = NO

		    # Record is complete so exist the loop.
		    break
	        }
	    }

	}

	if (nchars == EOF || nselect <= 0) {
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


# DP_GETAP  -- Decode the selected daophot/apphot values.

procedure dp_getap (key, indices, id, x, y, sky, mag)

pointer	key		# pointer to keys strucuture
int	indices[ARB]	# index array
int	id		# star id
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

	    # Extract the appropriate field.
	    call amovc (Memc[kip], buffer, maxch)
	    buffer[maxch+1] = EOS

	    # Decode the output value.
	    ip = 1
	    switch (indices[i]) {
	    case DP_PAPID: 
		if (ctoi (buffer, ip, id) <= 0)
		    id = 0
	    case DP_PAPXCEN:
		if (ctor (buffer, ip, x) <= 0)
		    x = INDEFR
	    case DP_PAPYCEN:
		if (ctor (buffer, ip, y) <= 0)
		    y = INDEFR
	    case DP_PAPSKY:
	        if (ctor (buffer, ip, sky) <= 0)
		    sky = INDEFR
	    case DP_PAPMAG1:
		if (ctor (buffer, ip, mag) <= 0)
		    mag = INDEFR
	    default:
		call printf ("Error reading the APPHOT results.\n")
	    }

	}
end
