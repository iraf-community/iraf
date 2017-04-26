# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include <ctype.h>
include	"idbc.h"

# IMGCOM -- Get the comment field for a keyword.

procedure imgcom (im, key, comment)

pointer	im			#I image descriptor
char	key[ARB]		#I parameter to be set
char	comment[ARB]		#O comment string

bool	string_valued
int	ch, i, n, j, ic, op
pointer	rp, ip, sp, buf
int	idb_findrecord(), ctowrd(), stridx(), idb_getstring()
errchk	syserrs

define  end_ 91
begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

        # Special fields do not have comment.
        if (key[1] == 'i' && key[2] == '_') {  
	    comment[1] = EOS
	    return
        }

	# Find the record.
	if (idb_findrecord (im, key, rp) == 0)
	    call syserrs (SYS_IDBKEYNF, key)

        ip = IDB_STARTVALUE
        if (ctowrd (Memc[rp], ip, Memc[buf], SZ_LINE) <= 0) {
	    comment[1] = EOS
	    goto end_
        }
 
	# Look for '/'
        while (ip < IDB_RECLEN && (Memc[rp+ip] != '/'))
             ip = ip + 1
        if (ip == IDB_RECLEN) {
	   comment[1] = EOS
	   goto end_
        }
        op = rp+ip+1
        while (op < IDB_RECLEN+rp && (IS_WHITE(Memc[op]) || Memc[op] == '\n'))
             op = op + 1

        # Copy comment section
        for (i = 1; Memc[op] != '\n' && op < IDB_RECLEN+rp; op=op+1) {
            comment[i] = Memc[op]
            i = i + 1
        }
        # Trim
	i = i - 1
        while (i >= 1 && IS_WHITE(comment[i]))
	   i = i - 1

        comment[i+1] = EOS
end_
	call sfree (sp)
end
