# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"idbc.h"

# IMDRMCOM -- Remove the comment field for a keyword.

procedure imdrmcom (im, key)

pointer	im			#I image descriptor
char	key[ARB]		#I parameter to be set

bool	string_valued
int	ch, i, ti, j, n
pointer	rp, ip, op, sp, val, start, text, cmmt
int	idb_findrecord()
errchk	syserrs

begin
	call smark (sp)
	call salloc (val, SZ_LINE, TY_CHAR)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (cmmt, SZ_LINE, TY_CHAR)

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

	for (i=0; i<SZ_LINE; i=i+1)
	    Memc[text+i] = ' '
	Memc[text+SZ_LINE] = EOS

	# Determine the actual datatype of the parameter.  String valued
	# parameters will have an apostrophe in the first nonblank column
	# of the value field.

	string_valued = false
	ti = text
	for (ip=IDB_STARTVALUE;  ip <= IDB_ENDVALUE;  ip=ip+1) {
	    # Skip leading whitespace.
	    for (; Memc[rp+ip-1] == ' '; ip=ip+1) {
		Memc[ti] = Memc[rp+ip-1]
		ti = ti + 1
	    }
	    if (Memc[rp+ip-1] == '\'') {
		# Get string value.
	        Memc[ti] = Memc[rp+ip-1]
		ti = ti + 1
		do i = ip, IDB_RECLEN {
		    ch = Memc[rp+i]
		    Memc[ti] = ch
		    ti = ti + 1
		    if (ch == '\n')
			break
		    if (ch == '\'')
			break
		}
		break

	    } else {
		# Numeric value.
		do i = ip, IDB_RECLEN {
		    ch = Memc[rp+i-1]
		    Memc[ti] = ch
		    ti = ti + 1
		    if (ch == '\n' || ch == ' ' || ch == '/')
			break
		}
#		if (ch == ' ')
#		    ti = ti - 1
		break
	    }
	}

	n = 0
	do j = i, IDB_RECLEN {
	   ch = Memc[rp+j]
	   Memc[cmmt+n] = ch
	   n = n + 1
	   if (ch == '\n') {
	       n = n - 1
	       break
	   }
	}
	Memc[cmmt+n] = EOS 

	# Update the parameter value.
	op = rp + IDB_STARTVALUE + ti-text - 1 
	start = op
	for (ip=ti;  Memc[ip] != EOS && Memc[op] != '\n';  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}

	call sfree (sp)
end
