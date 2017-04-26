# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"../imfort.h"
include	"idb.h"

# IDB_FINDRECORD -- Search the image database for a particular record given
# the key.  The record number (a positive nonzero integer) is returned if
# the record is found, else 0.  

int procedure idb_findrecord (im, key, rp)

pointer	im			# image descriptor
char	key[ARB]		# record key
pointer	rp			# char record pointer (output)

pointer	sp, pat, patbuf, ukey, lkey, ip, ua
int	recno, nchars, lch, uch, ch, junk, i
int	patmake(), patmatch(), gstrcpy()

begin
	call smark (sp)
	call salloc (pat, SZ_FNAME, TY_CHAR)
	call salloc (ukey, SZ_FNAME, TY_CHAR)
	call salloc (lkey, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

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
		for (nchars=0;  Memc[ip] != EOS;  nchars=nchars+1) {
		    if (Memc[ip] == '\n')
			break
		    ip = ip + 1
		}
		if (nchars != IDB_RECLEN) {
		    IM_UABLOCKED(im) = NO
		    break
		}
	    }
	}

	if (IM_UABLOCKED(im) == NO) {
	    # Variable length, newline terminated records, EOS terminated
	    # record group.

	    call strcpy ("^{", Memc[pat], SZ_FNAME)
	    call strcat (key,  Memc[pat], SZ_FNAME)
	    call strcat ("}[ =]",  Memc[pat], SZ_FNAME)
	    junk = patmake (Memc[pat], Memc[patbuf], SZ_LINE)

	    for (ip=ua;  Memc[ip] != EOS;  ip=ip+1) {
		if (patmatch (Memc[ip], Memc[patbuf]) > 0) {
		    rp = ip
		    break
		}
		while (Memc[ip] != '\n' && Memc[ip] != EOS)
		    ip = ip + 1
		recno = recno + 1
	    }

	} else {
	    # Fixed length (80 character), newline terminated records, EOS
	    # terminated record group.  Simple fast search, fixed length
	    # records.  Case insensitive keyword match.

	    nchars = gstrcpy (key, Memc[lkey], SZ_FNAME)
	    call strlwr (Memc[lkey])
	    lch = Memc[lkey]

	    nchars = gstrcpy (key, Memc[ukey], SZ_FNAME)
	    call strupr (Memc[ukey])
	    uch = Memc[ukey]

	    for (ip=ua;  Memc[ip] != EOS;  ip=ip+IDB_RECLEN+1) {
		ch = Memc[ip]
		if (ch == EOS)
		    break
		else if (ch != lch && ch != uch)
		    next
		else {
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

	call sfree (sp)
	if (rp == NULL)
	    return (0)
	else
	    return (recno)
end
