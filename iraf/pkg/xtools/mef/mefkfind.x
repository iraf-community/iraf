include <syserr.h>
include <pkg/mef.h>

# MEF_FINDKW -- Search the header database for a particular keyword
# and get its value. An error is returned if the keyword is not found.

procedure mef_findkw (hdrp, key, keywval)

pointer	hdrp			#I pointer to header buffer 
char	key[ARB]		#I Keyword name
char	keywval[ARB]		#O string value

pointer	sp, ukey, lkey, ip
int	nchars, lch, uch, ch, i
int	gstrcpy()

errchk  syserrs

begin
	call smark (sp)
	call salloc (ukey, SZ_KEYWORD, TY_CHAR)
	call salloc (lkey, SZ_KEYWORD, TY_CHAR)

	# Prepare U/L FITS keywords, truncated to 8 chars.
	nchars = gstrcpy (key, Memc[lkey], SZ_KEYWORD)
	call strlwr (Memc[lkey])
	nchars = gstrcpy (key, Memc[ukey], SZ_KEYWORD)
	call strupr (Memc[ukey])

	# Search for the FIRST occurrence of a record with the given key.

	# Fixed length (80 character), newline terminated records, EOS
	# terminated record group.

	# Simple fast search, fixed length records.  Case insensitive
	# keyword match.

	lch = Memc[lkey]
	uch = Memc[ukey]

	for (ip=hdrp;  Memc[ip] != EOS;  ip=ip+LEN_CARDNL) {
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
		#Copy card starting at ip
		call mef_gvalt (Memc[ip], keywval, MEF_SZVALSTR)
		call sfree (sp)
		return
	    }
	}

	# Keyword not found
	call syserrs (SYS_IDBKEYNF, key)

	call sfree (sp)
end
