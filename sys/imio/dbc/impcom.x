# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"idbc.h"

# IMPCOM -- Change the comment field for a keyword.

procedure impcom (im, key, comment)

pointer	im			#I image descriptor
char	key[ARB]		#I parameter to be set
char	comment[ARB]		#I comment string

bool	string_valued
int	ch, i, ti, j
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
		do j = i, IDB_ENDVALUE-2 {
	           Memc[ti] = ' ' ; ti=ti+1
	        }
		break

	    } else {
		# Skip numeric value.
		do i = ip, IDB_RECLEN {
		    ch = Memc[rp+i-1]
		    Memc[ti] = ch
		    ti = ti + 1
		    if (ch == '\n' || ch == ' ' || ch == '/')
			break
		}
		if (ch == ' ')
		    ti = ti - 1
		do j = i, IDB_ENDVALUE {
	           Memc[ti] = ' ' ; ti=ti+1
	        }
		break
	    }
	}
	Memc[ti]=EOS
        if (comment[1] != EOS) {
	    call strcat (" / ", Memc[ti], SZ_LINE)
            for (i=1; comment[i] == ' '; i=i+1)
                ;
            call strcat (comment[i], Memc[ti], SZ_LINE)
        } else {
	    do j = i, IDB_RECLEN {
	       Memc[ti] = ' ' ; ti=ti+1
	    }
	}
	# Update the parameter value.
	op = rp + IDB_STARTVALUE + ti-text - 1 
	start = op
	for (ip=ti;  Memc[ip] != EOS && Memc[op] != '\n';  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}

	call sfree (sp)
end
