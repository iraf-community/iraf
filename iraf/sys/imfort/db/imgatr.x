# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"idb.h"

# IMGATR -- Get the attribute fields (type code and comment) of a header
# keyword.  A separate, normally typed, call is required to get the keyword
# value.

procedure imgatr (im, key, dtype, comm, maxch)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
int	dtype			# receives datatype code
char	comm[ARB]		# output string to comment field
int	maxch

int	op
pointer	rp, ip
int	idb_getstring(), idb_findrecord(), imgftype()
errchk	syserrs, imgftype

begin
	# Get the field datatype.
	dtype = imgftype (im, key)

	# Check for a standard header parameter first.
	if (idb_getstring (im, key, comm, maxch) != ERR) {
	    comm[1] = EOS
	    return
	}

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

	# Extract the comment field.
	for (ip=rp+IDB_ENDVALUE;  Memc[ip] != '/' && Memc[ip] != '\n';  ip=ip+1)
	    ;
	if (Memc[ip] == '/') {
	    for (ip=ip+1;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    for (op=1;  Memc[ip] != '\n';  ip=ip+1) {
		comm[op] = Memc[ip]
		op = op + 1
	    }
	    comm[op] = EOS
	} else
	    comm[1] = EOS
end
