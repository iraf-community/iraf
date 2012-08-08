include	<tbset.h>
include	"../../lib/ptkeysdef.h"
include	"pexamine.h"

define	NAPRESULT	10

# PT_GETPHOT -- Read the specified columns out of the photometry catalog.
# PT_GETPHOT works with either the "old" APPHOT files or the new ST Tables.

int procedure pt_getphot (px, apd, key, max_nstars, first_star)

pointer	px			# pointer to the pexamine structure
int	apd			# input photometry file descriptor
pointer	key			# pointer to key structure for text files
int	max_nstars		# the maximum number of stars
int	first_star		# first star to load

int	i, nstars
int	pt_goldap(), pt_gtabphot(), strdic()
errchk	pt_goldap(), pt_gtabphot()

begin
	# Allocate the required memory for the photometry, user and
	# dummy columns and fill with INDEFR.
	do i = 1, PX_MAXNCOLS {
	    call realloc (Memi[PX_COLPTRS(px)+i-1], max_nstars, TY_REAL)
	    call amovkr (INDEFR, Memr[Memi[PX_COLPTRS(px)+i-1]], max_nstars)
	}

	# Get the results.
	if (key == NULL)
	    nstars = pt_gtabphot (apd, px, max_nstars, first_star)
	else
	    nstars = pt_goldap (apd, px, key, max_nstars, first_star)

	# Reallocate space if necessary.
	if (nstars < max_nstars) {
	    do i = 1, PX_MAXNCOLS {
		if (Memi[PX_COLPTRS(px)+i-1] == NULL)
		    next
		if (i > PX_NCOLS(px)) {
	            call mfree (Memi[PX_COLPTRS(px)+i-1], TY_REAL)
		    Memi[PX_COLPTRS(px)+i-1] = NULL
		} else
	            call realloc (Memi[PX_COLPTRS(px)+i-1], nstars, TY_REAL)
	    }
	}

	# Get the x and y columns.
	if (strdic (PX_RXCOLNAME(px), PX_XCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)]) <= 0)
	    PX_XCOLNAME(px) = EOS
	if (strdic (PX_RYCOLNAME(px), PX_YCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)]) <= 0)
	    PX_YCOLNAME(px) = EOS

	# Get the x and y coordinate columns.
	if (strdic (PX_RXPOSNAME(px), PX_XPOSNAME(px), PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)]) <= 0)
	    PX_XPOSNAME(px) = EOS
	if (strdic (PX_RYPOSNAME(px), PX_YPOSNAME(px), PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)]) <= 0)
	    PX_YPOSNAME(px) = EOS

	# Get the histogram column names.
	if (strdic (PX_RHCOLNAME(px), PX_HCOLNAME(px), PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)]) <= 0)
	    PX_HCOLNAME(px) = EOS

	return (nstars)
end


# PT_GOLDAP -- Read in the photometry from an old style APPHOT file.

int procedure pt_goldap (apd, px, apkey, max_nstars, first_star)

int	apd		# pointer to the input file descriptor
pointer	px		# pointer to the pexamine structure
pointer	apkey		# pointer to the key structure
int	max_nstars	# maximum number of stars
int	first_star	# first star to load

char	lbracket
int	i, ip, index, nselect, nphot, nptr, nstars
pointer	sp, data, rcolname, colname
int	pt_photsel(), pt_getnames(), strdic(), stridx()
data	lbracket /'['/

begin
	call smark (sp)
	call salloc (data, PX_MAXNCOLS, TY_REAL)
	call salloc (rcolname, PX_SZCOLNAME, TY_CHAR)
	call salloc (colname, PX_SZCOLNAME, TY_CHAR)

	# Rewind the text file.
	call seek (apd, BOF)

	# Now read in the results.
	nptr = 0
	nstars = 0
	while (pt_photsel (apkey, apd, Memc[PX_RCOLNAMES(px)], first_star +
	    max_nstars - 1, Memc[PX_COLNAMES(px)], Memr[data]) != EOF) {
	    nselect = KY_NSELECT(apkey)
	    if (nselect <= 0)
		break
	    nstars = nstars + 1
	    if (nstars < first_star)
		next
	    do i = 1, nselect
		Memr[Memi[PX_COLPTRS(px)+i-1]+nptr] = Memr[data+i-1]
	    nptr = nptr + 1
	}

	# Count the fields.
	ip = 1
	nselect = 0
	while (pt_getnames (Memc[PX_COLNAMES(px)], ip, Memc[rcolname],
	    PX_SZCOLNAME) != EOF)
	    nselect = nselect + 1
	PX_NCOLS(px) = nselect

	# Count the photometry fields.
	ip = 1
	nselect = 0
	nphot = 0
	while (pt_getnames (Memc[PX_RCOLNAMES(px)], ip, Memc[rcolname],
	    PX_SZCOLNAME) != EOF) {
	    nselect = nselect + 1
	    if (nselect > PX_RNPHOT(px))
		break
	    if (strdic (Memc[rcolname], Memc[colname], PX_SZCOLNAME,
	        Memc[PX_COLNAMES(px)]) <= 0) {
		index = stridx (lbracket, Memc[rcolname])
		if (index <= 1)
		    next
		call strcpy (Memc[rcolname], Memc[colname], index - 1)
		if (strdic (Memc[colname], Memc[colname], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) <= 0)
		    next
	    }
	    nphot = nphot + 1
	}
	PX_NPHOT(px) = nphot

	# Count the user fields.
	PX_NUSER(px) = PX_NCOLS(px) - PX_NPHOT(px)

	call sfree (sp)

	return (nptr)
end


# PT_GTABPHOT -- Read in the photometry from an ST table. It may be possible
# to do this more efficiently depending on how the table ir organized.

int procedure pt_gtabphot (tp, px, max_nstars, first_star)

pointer	tp			# table descriptor
pointer	px			# pointer to the pexamine structure
int	max_nstars		# maximum number of stars
int	first_star		# first star to load

bool	nullflag
int	ntot, ncount, record, ip, col, nrow, ival
pointer	sp, colname, colptrs, cptr, dptr 
int	pt_getnames(), tbpsta(), tbcigi()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (colname, PX_SZCOLNAME, TY_CHAR)
	call salloc (colptrs, PX_MAXNCOLS + 2, TY_POINTER)

	# Define the column pointers for the preset columns.
	ip = 1
	ncount = 0
	Memc[PX_COLNAMES(px)] = EOS
	ntot = 0
	while (pt_getnames (Memc[PX_RCOLNAMES(px)], ip, Memc[colname],
	    PX_SZCOLNAME) != EOF) {

	    ncount = ncount + 1
	    call tbcfnd (tp, Memc[colname], Memi[colptrs+ntot], 1)
	    if (Memi[colptrs+ntot] == NULL)
		call strcat ("[1]", Memc[colname], PX_SZCOLNAME)
	    call tbcfnd (tp, Memc[colname], Memi[colptrs+ntot], 1)
	    if (Memi[colptrs+ntot] == NULL)
		next

	    call strcat (",", Memc[PX_COLNAMES(px)], (PX_MAXNCOLS + 1) *
	        PX_SZCOLNAME)
	    call strcat (Memc[colname], Memc[PX_COLNAMES(px)],
	        (PX_MAXNCOLS + 1) * PX_SZCOLNAME)

	    ntot = ntot + 1
	    if (ncount <= PX_RNPHOT(px))
		PX_NPHOT(px) = ntot
	}
	PX_NCOLS(px) = ntot
	PX_NUSER(px) = PX_NCOLS(px) - PX_NPHOT(px)

	# Get the results filling in any record that can not be interpreted
	# as a real number with INDEFR.

	nrow = tbpsta (tp, TBL_NROWS)
	if (first_star > nrow) {
	    call sfree (sp)
	    return (0)
	}

	nrow = min (nrow - first_star + 1, max_nstars)

	do col = 1, PX_NCOLS(px) {

	    cptr = Memi[colptrs+col-1]
	    if (cptr == NULL)
	        next
	    dptr = Memi[PX_COLPTRS(px)+col-1]
	    if (dptr == NULL)
		next

	    if (tbcigi (cptr, TBL_COL_DATATYPE) == TY_REAL) {
	        do record = first_star, nrow + first_star - 1
	    	    call tbrgtr (tp, cptr, Memr[dptr+record-first_star],
		        nullflag, 1, record)
	    } else if (tbcigi (cptr, TBL_COL_DATATYPE) == TY_INT) {
	        do record = first_star, nrow + first_star - 1 {
	    	    call tbrgti (tp, cptr, ival, nullflag, 1, record)
		    Memr[dptr+record-first_star] = ival
		}
	    }
	}

	call sfree (sp)

	if (PX_NCOLS(px) <= 0)
	    return (0)
	else
	    return (nrow)
end


# PT_PHOTSEL -- Procedure to select real records from a text file.

int procedure pt_photsel (key, fd, infields, max_nrecords, outfields, data)

pointer key		# pointer to key structure
int	fd		# text file descriptor
char	infields[ARB]	# requested output fields
int	max_nrecords	# maximum number of records to be read
char	outfields[ARB]	# selected output field
real	data[ARB]	# array of real values read from the file

int	nchars, nunique, uunique, funique, ncontinue, recptr
int 	first_rec, record
pointer	line
int	getline(), strncmp(), pt_choose()

data	first_rec /YES/

begin
	# Initialize the file read.
	if (first_rec == YES) {
	    record = 0
	    nunique = 0
	    uunique = 0
	    funique = 0
	    call malloc (line, SZ_LINE, TY_CHAR)
	}

	ncontinue = 0
	recptr = 1

	# Loop over the text file records.
	repeat  {

	    # Check for the maximum number of records and EOF.
	    if (record >= max_nrecords)
		nchars = EOF
	    else
	        nchars = getline (fd, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] ==  KY_CHAR_POUND) {

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

	    } else if (Memc[line] ==  KY_CHAR_NEWLINE) {
		# skip blank lines

	    } else {

		# Construct the table record.
		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct output record when there is no continuation char.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Select the appropriate records.
		    if (first_rec == YES) {
			call pt_fields (key, infields, outfields)
		        if (pt_choose (key, outfields) <= 0) {
			    nchars = EOF
			    break
			}
		    }

		    # Construct the output record by moving selected fields
		    # into the data structures.

		    call pt_grecord (key, data)
		    first_rec = NO
		    record = record + 1

		    # Record is complete so exit the loop.
		    break
	        }
	    }

	}

	if (nchars == EOF) {
	    first_rec = YES
	    record = 0
	    nunique = 0
	    uunique = 0
	    funique = 0
	    call mfree (line, TY_CHAR)
	    return (EOF)
	} else
	    return (record)
end


# PT_FIELDS -- Check the user definitions for multiply defined entries.

procedure pt_fields (key, infields, outfields)

pointer	key		# pointer to keys strucuture
char	infields[ARB]	# the list of input fields
char	outfields[ARB]	# the list of input fields

int	ijunk, num
pointer	sp, name, aranges, ranges, rangeset, list
int	pt_gnfn(), pt_ranges(), decode_ranges(), get_next_number(), strlen()
int	pt_kstati()
pointer	pt_ofnl()

begin
	call smark (sp)
	call salloc (name, PX_SZCOLNAME, TY_CHAR)
	call salloc (aranges, SZ_LINE, TY_CHAR)
	call salloc (ranges, SZ_LINE, TY_CHAR)
	call salloc (rangeset, 3 * KY_MAXNRANGES + 1, TY_INT)

	list = pt_ofnl (key, infields)
	outfields[1] = EOS
	while (pt_gnfn (list, Memc[name], Memc[aranges], KY_SZPAR) != EOF) {
	    if (Memc[name] == EOS)
		next
	    num = 0
	    if (Memc[aranges] == EOS) {
		if (pt_kstati (key, Memc[name], KY_NUMELEMS) > 1)
		    call strcat ("[1]", Memc[name], PX_SZCOLNAME)
	    } else if (pt_ranges (Memc[aranges], Memc[ranges], ijunk,
	        SZ_LINE) == ERR) {
		call strcat ("[1]", Memc[name], PX_SZCOLNAME)
	    } else if (decode_ranges (Memc[ranges], Memi[rangeset],
	        KY_MAXNRANGES, ijunk) == ERR) {
		call strcat ("[1]", Memc[name], PX_SZCOLNAME)
	    } else if (get_next_number (Memi[rangeset], num) > 0) {
		call sprintf (Memc[name+strlen(Memc[name])], PX_SZCOLNAME,
		    "[%d]")
		    call pargi (num)
	    } else {
		call strcat ("[1]", Memc[name], PX_SZCOLNAME)
	    }
	    call strcat (",", outfields, PX_SZCOLNAME * (PX_MAXNCOLS + 1))
	    call strcat (Memc[name], outfields, PX_SZCOLNAME *
	        (PX_MAXNCOLS + 1))
	}

	call pt_cfnl (list)
	call sfree (sp)
end


# PT_GRECORD -- Move selected photometry results into a real arrays.

procedure pt_grecord (key, data)

pointer	key		# pointer to keys strucuture
real	data[ARB]	# output array of real selected data

int	i, index, elem, maxch, kip, ip
int	ctor()

begin
	do i = 1, KY_NSELECT(key) {

	    index = Memi[KY_SELECT(key)+i-1]
	    elem = Memi[KY_ELEM_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1]
	    kip = Memi[KY_PTRS(key)+index-1] + (elem - 1) * maxch

	    ip = 1
	    if (kip == NULL)
		data[i] = INDEFR
	    else if (ctor (Memc[kip], ip, data[i]) <= 0)
	        data[i] = INDEFR
	}

end
