#include <ctype.h>
include <tbset.h>
include "../../lib/ptkeysdef.h"

# PT_DEFTABLE -- Set up and create the table for the conversion of
# the APPHOT text file.

int procedure pt_deftable (td, key, fields, columns)

pointer	td			# output table descriptor
pointer	key			# key structure
char	fields[ARB]		# fields to be output to the table
int	columns[ARB] 		# pointer to array of column pointers

int	ncols
int	pt_defcols()

begin
	# Define the table columns.
	ncols  = pt_defcols (td, key, fields, columns)
	if (ncols <= 0)
	    return (0)

	# Define the table header.
	call pt_defheader (td, key)

	return (ncols)
end


# PT_DEFHEADER -- Define the header structure.

procedure pt_defheader (td, key)

pointer	td				# table descriptor
pointer	key				# pointer to the keys structure

int	i, index, type, len, ival, ip
pointer	sp, kname, value, ptr
real    rval
bool	streq()
int	pt_strwrd(), pt_kstati(), ctoi(), ctor()

begin
	call smark (sp)
	call salloc (kname, SZ_KEYWORD, TY_CHAR)
	call salloc (value, KY_SZPAR, TY_CHAR)

	# Define the columns determining datatype from the format string.
	do i = 1, KY_NPKEYS(key) {

	    index = pt_strwrd (i, Memc[kname], SZ_KEYWORD, Memc[KY_WORDS(key)])
	    if (index <= 0)
		next

	    ptr = Memi[KY_PTRS(key)+index-1] 
	    len = pt_kstati (key, Memc[kname], KY_LENGTH)
	    call strcpy (Memc[ptr], Memc[value], len)
	    type = pt_kstati (key, Memc[kname], KY_DATATYPE)

	    switch (type) {
	    case TY_INT:
		ip = 1
		if (ctoi (Memc[value], ip, ival) <= 0)
		    call tbhadi (td, Memc[kname], INDEFI)
		else
		    call tbhadi (td, Memc[kname], ival)
	    case TY_REAL:
		ip = 1
		if (ctor (Memc[value], ip, rval) <= 0)
		    call tbhadr (td, Memc[kname], INDEFR)
		else
		    call tbhadr (td, Memc[kname], rval)
	    case TY_BOOL:
		if (streq ("yes", Memc[value]))
		    call tbhadb (td, Memc[kname], true)
		else
		    call tbhadb (td, Memc[kname], false)
	    default:
		call tbhadt (td, Memc[kname], Memc[value])
	    }
	}

	call sfree (sp)
end
	

# PT_DEFCOLS -- Define the data columns to be output.

int procedure pt_defcols (td, key, fields, columns)

pointer	td			# table Descriptor
pointer key			# key structure
char 	fields[ARB]		# fields selected
pointer columns[ARB]		# array of pointers for columns

int	max_nkeys, index, ncols, element, nelems, type, len
pointer	sp, kname, aranges, ranges, rangeset, colname, colunits, colformat
pointer	list

int	pt_gnfn(), strdic(), pt_ranges(), get_next_number()
int	decode_ranges(), pt_kstati()
pointer	pt_ofnl()
real	asumi()
errchk	tbcdef()

begin
	# Allocate working space.

	call smark (sp)
	call salloc (kname, KY_SZPAR, TY_CHAR)
	call salloc (aranges, SZ_FNAME, TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)
	call salloc (rangeset, 3 * KY_MAXNRANGES, TY_INT)

	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colformat, SZ_COLFMT, TY_CHAR)

	# Initialize the select buffers. The select keyword, format
	# and units string buffers are not necessary in this application.

	max_nkeys = nint (asumi (Memi[KY_NELEMS(key)], KY_NKEYS(key)))
	if (KY_SELECT(key) != NULL)
	    call mfree (KY_SELECT(key), TY_INT)
	call malloc (KY_SELECT(key), max_nkeys, TY_INT)
	if (KY_ELEM_SELECT(key) != NULL)
	    call mfree (KY_ELEM_SELECT(key), TY_INT)
	call malloc (KY_ELEM_SELECT(key), max_nkeys, TY_INT)
	if (KY_LEN_SELECT(key) != NULL)
	    call mfree (KY_LEN_SELECT(key), TY_INT)
	call malloc (KY_LEN_SELECT(key), max_nkeys, TY_INT)

	# Initialize the number of columns.
	ncols = 0

	# Open the list of fields.
	list = pt_ofnl (key, fields)

	# Loop over the fields.
	while (pt_gnfn (list, Memc[kname], Memc[aranges], KY_SZPAR) != EOF) {

	    # Find location of the field in the keyword dictionary and 
	    # expand the ranges string if it is defined.
	    index = strdic (Memc[kname], Memc[kname], KY_SZPAR, 
		Memc[KY_WORDS(key)])
	    if (pt_ranges (Memc[aranges], Memc[ranges], element, SZ_LINE) ==
	        ERR)
		call error (0, "Cannot decode DAOPHOT range string")

	    # The field was not found.
	    if (index == 0) {
		next

	    # Reject keyword fields.
	    } else if (index <= KY_NPKEYS (key)) {
 		next

	    # The field is single valued.
	    } else if (Memi[KY_NELEMS(key)+index-1] == 1) {

		# Enter the pointers into the select buffer.
	        Memi[KY_SELECT(key)+ncols] = index
	        Memi[KY_ELEM_SELECT(key)+ncols] = 1
		len = pt_kstati (key, Memc[kname], KY_LENGTH)
		Memi[KY_LEN_SELECT(key)+ncols] = len
		ncols = ncols + 1

		# Get the column name, the units and the formats.
		call strcpy (Memc[kname], Memc[colname], SZ_COLNAME)
		call pt_kstats (key, Memc[kname], KY_UNITSTR, Memc[colunits],
		    SZ_COLUNITS)
		call pt_kstats (key, Memc[kname], KY_FMTSTR, Memc[colformat],
		    SZ_COLFMT)

	        # Create the column  determining the datatype from the
		# format string.
		type = pt_kstati (key, Memc[kname], KY_DATATYPE)
		if (type != TY_CHAR)
		    call tbcdef (td, columns[ncols], Memc[colname], 
		        Memc[colunits], Memc[colformat], type, 1, 1)
		else
		    call tbcdef (td, columns[ncols], Memc[colname], 
		        Memc[colunits], Memc[colformat], -len, 1, 1)

	    # The field is multi-valued.
	    } else {

		if (Memc[ranges] == EOS) {
		    call sprintf (Memc[ranges], SZ_FNAME, "1-%d")
			call pargi (Memi[KY_NELEMS(key)+index-1])
		}
		if (decode_ranges (Memc[ranges], Memi[rangeset], KY_MAXNRANGES,
		    nelems) == ERR)
		    call error (0, "Cannot decode ranges string")

		nelems = 0
		while (get_next_number (Memi[rangeset], nelems) != EOF) {
		    if (nelems < 1 || nelems > Memi[KY_NELEMS(key)+index-1])
			break

		    len = pt_kstati (key, Memc[kname], KY_LENGTH)
		    Memi[KY_SELECT(key)+ncols] = index
		    Memi[KY_ELEM_SELECT(key)+ncols] = nelems
		    Memi[KY_LEN_SELECT(key)+ncols] = len
		    ncols = ncols + 1

		    # Get the column name, units and format.
		    call sprintf (Memc[colname], SZ_COLNAME, "%s[%d]")
			call pargstr (Memc[kname])
			call pargi (nelems)
		    call pt_kstats (key, Memc[kname], KY_UNITSTR,
		        Memc[colunits], SZ_COLUNITS)
		    call pt_kstats (key, Memc[kname], KY_FMTSTR,
		        Memc[colformat], SZ_COLFMT)

	    	    # Create the column determining the datatype from the
		    # format string.
		    type = pt_kstati (key, Memc[kname], KY_DATATYPE)
		    if (type != TY_CHAR)
		        call tbcdef (td, columns[ncols], Memc[colname], 
		            Memc[colunits], Memc[colformat], type, 1, 1)
		    else
		        call tbcdef (td, columns[ncols], Memc[colname], 
		             Memc[colunits], Memc[colformat], -len, 1, 1)
		}
	    }
	}

	# Create the table.
	call tbpset (td, TBL_MAXPAR, KY_NPKEYS(key))
	call tbtcre (td)

	# Free the space
	call pt_cfnl (list)
	call sfree (sp)

	# Reallocate select buffer space
	KY_NSELECT(key) = ncols
	call realloc (KY_SELECT(key), KY_NSELECT(key), TY_INT)
	call realloc (KY_ELEM_SELECT(key), KY_NSELECT(key), TY_INT)
	call realloc (KY_LEN_SELECT(key), KY_NSELECT(key), TY_INT)

	return (ncols)
end


# PT_APPTABLE -- Find the array of column pointers for appending to an
# existing ST table.

int procedure pt_apptable (td, key, fields, columns)

pointer	td			# the table descriptor
pointer	key			# key structure
char	fields[ARB]		# string containing fields to include in table
pointer columns[ARB]		# Array of pointers for columns

int	max_nkeys, index, ncols, element, nelems
pointer	list, sp, kname, aranges, ranges, rangeset, colname
int	pt_gnfn(), strdic(), pt_ranges(), get_next_number(), decode_ranges()
int     pt_kstati()
pointer	pt_ofnl()
real	asumi()

begin
	# Allocate buffer space.
	call smark (sp)
	call salloc (kname, KY_SZPAR, TY_CHAR)
	call salloc (aranges, SZ_FNAME, TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)
	call salloc (rangeset, 3 * KY_MAXNRANGES, TY_INT)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	# Initialize the select buffers. The select keyword, format
	# and units string buffers are not necessary in this application.

	max_nkeys = nint (asumi (Memi[KY_NELEMS(key)], KY_NKEYS(key)))
	max_nkeys = nint (asumi (Memi[KY_NELEMS(key)], KY_NKEYS(key)))
	if (KY_SELECT(key) != NULL)
	    call mfree (KY_SELECT(key), TY_INT)
	call malloc (KY_SELECT(key), max_nkeys, TY_INT)
	if (KY_ELEM_SELECT(key) != NULL)
	    call mfree (KY_ELEM_SELECT(key), TY_INT)
	call malloc (KY_ELEM_SELECT(key), max_nkeys, TY_INT)
	if (KY_LEN_SELECT(key) != NULL)
	    call mfree (KY_LEN_SELECT(key), TY_INT)
	call malloc (KY_LEN_SELECT(key), max_nkeys, TY_INT)

	# Initialize the number of columns.
	ncols = 0

	# Initialize the list of fields.
	ncols = 0
	list = pt_ofnl (key, fields)

	# Loop over the fields.
	while (pt_gnfn (list, Memc[kname], Memc[aranges], KY_SZPAR) != EOF) {

	    # Find the column.
	    index = strdic (Memc[kname], Memc[kname], KY_SZPAR, 
		Memc[KY_WORDS(key)])
	    if (pt_ranges (Memc[aranges], Memc[ranges], element,
	        SZ_LINE) == ERR)
		call error (0, "Cannot decode DAOPHOT range string")

	    # Skip if the column does exist in the text database.
	    if (index == 0)
		next

	    # Skip the header keywords.
	    else if (index <= KY_NPKEYS (key))
 		next

	    # Convert single-valued elements.
	    else if (Memi[KY_NELEMS(key)+index-1] == 1) {

		call strcpy (Memc[kname], Memc[colname], SZ_COLNAME)
		call tbcfnd (td, Memc[colname], columns[ncols+1], 1)
		if (columns[ncols+1] == NULL) {
		    call eprintf ("Column %s not found\n")
			call pargstr (Memc[colname])
		} else {
		    Memi[KY_SELECT(key)+ncols] = index
		    Memi[KY_ELEM_SELECT(key)+ncols] = 1
		    Memi[KY_LEN_SELECT(key)+ncols] = pt_kstati (key,
		        Memc[kname], KY_LENGTH)
	            ncols = ncols + 1
		}
	    
	    # Convert multivalued elements.
	    } else {

		if (Memc[ranges] == EOS) {
		    call sprintf (Memc[ranges], SZ_FNAME, "1-%d")
			call pargi (Memi[KY_NELEMS(key)+index-1])
		}
		if (decode_ranges (Memc[ranges], Memi[rangeset], KY_MAXNRANGES,
		    nelems) == ERR)
		    call error (0, "Cannot decode ranges string")

		nelems = 0
		while (get_next_number (Memi[rangeset], nelems) != EOF) {
		    if (nelems < 1 || nelems > Memi[KY_NELEMS(key)+index-1])
			break
		    call sprintf (Memc[colname], SZ_COLNAME, "%s[%d]")
			call pargstr (Memc[kname])
			call pargi (nelems)
		    call tbcfnd (td, Memc[colname], columns[ncols+1], 1)
		    if (columns[ncols+1] == NULL) {
			call eprintf ("Column %s not found\n")
			    call pargstr (Memc[colname])
		    } else {
		        Memi[KY_SELECT(key)+ncols] = index
		        Memi[KY_ELEM_SELECT(key)+ncols] = nelems
		        Memi[KY_LEN_SELECT(key)+ncols] = pt_kstati (key,
		            Memc[kname], KY_LENGTH)
		        ncols = ncols + 1
		    }

		}
	    }
	}

	# Free the space
	call pt_cfnl (list)
	call sfree (sp)

	# Reallocate select buffer space
	KY_NSELECT(key) = ncols
	call realloc (KY_SELECT(key), KY_NSELECT(key), TY_INT)
	call realloc (KY_ELEM_SELECT(key), KY_NSELECT(key), TY_INT)
	call realloc (KY_LEN_SELECT(key), KY_NSELECT(key), TY_INT)

	return (ncols)
end
