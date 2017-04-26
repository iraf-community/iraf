# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"idb.h"

# IDB_FINDRECORD -- Search the image database for a particular record given
# the key.  The record number (a positive nonzero integer) is returned if
# the record is found, else 0.  

int procedure idb_findrecord (im, key, rp)

pointer	im			# image descriptor
char	key[ARB]		# record key
pointer	rp			# char record pointer (output)

pointer	sp, pat, patbuf, ukey, lkey, ip, ua
int	recno, nchars, lch, uch, ch, junk, n, i
int	patmake(), patmatch(), stridxs(), gstrcpy()

begin
	call smark (sp)
	call salloc (pat, SZ_FNAME, TY_CHAR)
	call salloc (ukey, SZ_FNAME, TY_CHAR)
	call salloc (lkey, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	# Prepare U/L FITS keywords, truncated to 8 chars.
	nchars = gstrcpy (key, Memc[lkey], IDB_SZFITSKEY)
	call strlwr (Memc[lkey])
	nchars = gstrcpy (key, Memc[ukey], IDB_SZFITSKEY)
	call strupr (Memc[ukey])

	# Search for the FIRST occurrence of a record with the given key.
	# If the key is abbreviated and multiple keys are matched, the first
	# record matched is used.

	ua = IM_USERAREA(im)
	rp = NULL
	recno = 1

	if (IM_UABLOCKED(im) < 0) {
	    # At image open time this flag is set by IMMAP to -1 to indicate
	    # that the user area record type is not known.  An IKI kernel may
	    # subsequently set the flag to yes/no, else we determine the
	    # record type by inspection the first time we are called.  If the
	    # user area is empty the record type is set to blocked; IDB always
	    # writes blocked records.

	    IM_UABLOCKED(im) = YES
	    for (ip=ua;  Memc[ip] != EOS;  ip=ip+1) {
		for (n=0;  Memc[ip] != EOS;  n=n+1) {
		    if (Memc[ip] == '\n')
			break
		    ip = ip + 1
		}
		if (n != IDB_RECLEN) {
		    IM_UABLOCKED(im) = NO
		    break
		}
	    }
	}

	if (IM_UABLOCKED(im) == NO) {
	    # Variable length, newline terminated records, EOS terminated
	    # record group.

	    call sprintf (Memc[pat], SZ_FNAME, "^{%s}[ =]")
		call pargstr (Memc[ukey])
	    junk = patmake (Memc[pat], Memc[patbuf], SZ_LINE)

	    for (ip=ua;  Memc[ip] != EOS;  ip=ip+1) {
		if (patmatch (Memc[ip], Memc[patbuf]) > 0) {
		    rp = ip
		    break
		}
		#if (Memc[ip] != EOS)
		#    ip = ip + 1
		while (Memc[ip] != '\n' && Memc[ip] != EOS)
		    ip = ip + 1
		recno = recno + 1
	    }

	} else {
	    # Fixed length (80 character), newline terminated records, EOS
	    # terminated record group.

	    if (stridxs ("*?[]", Memc[ukey]) > 0) {
		# Pattern matching search.
		call sprintf (Memc[pat], SZ_FNAME, "^{%s}[ =]")
		    call pargstr (Memc[ukey])
		junk = patmake (Memc[pat], Memc[patbuf], SZ_LINE)

		for (ip=ua;  Memc[ip] != EOS;  ip=ip+IDB_RECLEN+1) {
		    if (patmatch (Memc[ip], Memc[patbuf]) > 0) {
			rp = ip
			break
		    }
		    recno = recno + 1
		}

	    } else {
		# Simple fast search, fixed length records.  Case insensitive
		# keyword match.

		lch = Memc[lkey]
		uch = Memc[ukey]

		for (ip=ua;  Memc[ip] != EOS;  ip=ip+IDB_RECLEN+1) {
		    ch = Memc[ip]
		    if (ch == EOS)
			break
		    else if (ch != lch && ch != uch)
			next
		    else {
			# Abbreviations are not permitted.
			ch = Memc[ip+nchars]
			if (ch != ' ' && ch != '=')
			    next
		    }

		    # First char matches; check rest of string.
		    do i = 1, nchars-1 {
			ch = Memc[ip+i]
			if (ch != Memc[lkey+i] && ch != Memc[ukey+i]) {
			    ch = 0
			    break
			}
		    }
		    if (ch != 0) {
			rp = ip		# match
			break
		    }

		    recno = recno + 1
		}
	    }
	}

	call sfree (sp)
	if (rp == NULL)
	    return (0)
	else
	    return (recno)
end
