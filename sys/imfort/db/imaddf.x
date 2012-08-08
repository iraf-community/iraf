# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	"../imfort.h"
include	"idb.h"

# IMADDF -- Add a user field to the image header.  It is an error if the named
# field already exists.

procedure imaddf (im, key, datatype, comment)

pointer	im			# image descriptor
char	key[ARB]		# name of the new parameter
int	datatype		# datatype of parameter
char	comment[ARB]		# comment describing new parameter

int	max_lenuserarea
pointer	sp, keyname, rp, ua, op
int	idb_kwlookup(), idb_findrecord(), strlen()
errchk	syserrs

begin
	call smark (sp)
	call salloc (keyname, SZ_FNAME, TY_CHAR)

	# FITS format requires that the keyword name be upper case.
	call strcpy (key, Memc[keyname], SZ_FNAME)
	call strupr (Memc[keyname])

	# Check for a redefinition.
	if ((idb_kwlookup (key) > 0) || (idb_findrecord (im, key, rp) > 0))
	    call syserrs (SYS_IDBREDEF, key)
	
	# Open the user area string for appending.  If the user area is not
	# empty the last character must be the newline record delimiter,
	# else the new record we add will be invalid.

	max_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU + 1) * SZ_STRUCT
	ua = IM_USERAREA(im)

	for (rp=ua;  Memc[rp] != EOS;  rp=rp+1)
	    ;
	if (rp - ua + IDB_RECLEN + 1 >= max_lenuserarea)
	    call syserrs (SYS_IDBOVFL, key)

	if (rp > ua && Memc[rp-1] != '\n') {
	    Memc[rp] = '\n'
	    rp = rp + 1
	}

	# Append the new record with an uninitialized value field.  Keyword
	# value pairs are encoded in FITS format.

	do op = rp, rp + IDB_RECLEN		# blank fill card
	    Memc[op] = ' '

	# Add the "= 'value' / comment".
	call amovc (Memc[keyname], Memc[rp], strlen(Memc[keyname]))
	Memc[rp+9-1] = '='
	if (datatype == TY_CHAR) {
	    Memc[rp+11-1] = '\''
	    Memc[rp+20-1] = '\''
	}

	# Add the comment field.
	Memc[rp+32-1] = '/'
	call amovc (comment, Memc[rp+34-1],
	    min (IDB_RECLEN-34+1, strlen(comment)))

	# Terminate the card.
	Memc[rp+IDB_RECLEN] = '\n'
	Memc[rp+IDB_RECLEN+1] = EOS

	call sfree (sp)
end
