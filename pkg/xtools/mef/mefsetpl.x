include <pkg/mef.h>

define  MEF_PLVERSION  MEF_HFLAG
define  MEF_PLSIZE     MEF_CGROUP

define  DEF_SZBUF       32768
define  INC_SZBUF       16384
define  INC_HDRMEM      8100
define  IDB_RECLEN      80

define  KW_TITLE        "$TITLE = "
define  LEN_KWTITLE     9
define  KW_CTIME        "$CTIME = "
define  LEN_KWCTIME     9
define  KW_MTIME        "$MTIME = "
define  LEN_KWMTIME     9
define  KW_LIMTIME      "$LIMTIME = "
define  LEN_KWLIMTIME   11
define  KW_MINPIXVAL    "$MINPIXVAL = "
define  LEN_KWMINPIXVAL 13
define  KW_MAXPIXVAL    "$MAXPIXVAL = "
define  LEN_KWMAXPIXVAL 13

define SZ_IMTITLE      383    # image title string

procedure mef_setpl (version, plsize, imhdr, title, ctime, mtime, limtime,
		minval, maxval, mef)

int     version		#I PL version number
char    imhdr[ARB]      #I Mask title
char    title[ARB]
int     plsize		#I Mask size of TY_SHORT
int	ctime
int	mtime
int	limtime
real	minval
real	maxval
pointer mef		#I Mef descriptor

int	tlen, i, ch, hdrlen, nchars
pointer	sp, tbuf, ip, op, rp, bp, hd
int	strncmp(), ctol(), ctor(), strlen()
errchk	realloc

begin
	MEF_PLVERSION(mef) = version
	MEF_PLSIZE(mef) = plsize
	tlen= strlen(imhdr)

	call smark (sp)
	call salloc (tbuf, SZ_IMTITLE, TY_CHAR)
	call salloc (bp, tlen, TY_CHAR)

	call strcpy (imhdr, Memc[bp], tlen)


	# Get the image title string.
	for (ip = bp;  Memc[ip] != EOS;) {
	    if (Memc[ip] == '$') {
		if (strncmp (Memc[ip], KW_TITLE, LEN_KWTITLE) == 0) {
		    # Advance to first character of quoted string.
		    ip = ip + LEN_KWTITLE
		    while (Memc[ip] != EOS && Memc[ip] != '"')
			ip = ip + 1
		    if (Memc[ip] == '"')
			ip = ip + 1

		    # Extract the string.
		    op = tbuf
		    while (Memc[ip] != EOS && Memc[ip] != '"') {
			if (Memc[ip] == '\\' && Memc[ip+1] == '"')
			    ip = ip + 1
			Memc[op] = Memc[ip]
			op = min (tbuf + SZ_IMTITLE, op + 1)
			ip = ip + 1
		    }

		    # Store in image descriptor.
		    Memc[op] = EOS
		    call strcpy (Memc[tbuf], title, SZ_IMTITLE)

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp (Memc[ip], KW_CTIME, LEN_KWCTIME) == 0) {
		    # Decode the create time.
		    ip = ip + LEN_KWCTIME
		    rp = 1
		    if (ctol (Memc[ip], rp, ctime) <= 0)
			ctime = 0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp (Memc[ip], KW_MTIME, LEN_KWMTIME) == 0) {
		    # Decode the modify time.
		    ip = ip + LEN_KWMTIME
		    rp = 1
		    if (ctol (Memc[ip], rp, mtime) <= 0)
			mtime = 0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp (Memc[ip], KW_LIMTIME, LEN_KWLIMTIME) == 0) {
		    # Decode the limits time.
		    ip = ip + LEN_KWLIMTIME
		    rp = 1
		    if (ctol (Memc[ip], rp, limtime) <= 0)
			limtime = 0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp(Memc[ip],KW_MINPIXVAL,LEN_KWMINPIXVAL)==0) {
		    # Decode the minimum pixel value.
		    ip = ip + LEN_KWMINPIXVAL
		    rp = 1
		    if (ctor (Memc[ip], rp, minval) <= 0)
			minval = 0.0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp(Memc[ip],KW_MAXPIXVAL,LEN_KWMAXPIXVAL)==0) {
		    # Decode the maximum pixel value.
		    ip = ip + LEN_KWMAXPIXVAL
		    rp = 1
		    if (ctor (Memc[ip], rp, maxval) <= 0)
			maxval = 0.0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1
		}
	    } else
		break
	}
	
	hdrlen = tlen*2
	call malloc (hd, hdrlen, TY_CHAR)
	op = hd

	while (Memc[ip] != EOS) {
	    rp = op

	    nchars = rp - hd
	    if (nchars + IDB_RECLEN + 2 > hdrlen) {
		hdrlen = hdrlen + INC_HDRMEM
		call realloc (hd, hdrlen, TY_CHAR)
		op = hd + nchars
	    }
	    # Copy the saved card, leave IP positioned to past newline.
	    do i = 1, IDB_RECLEN {
		ch = Memc[ip]
		if (ch != EOS)
		    ip = ip + 1
		if (ch == '\n')
		    break
		Memc[op] = ch
		op = op + 1
	    }

	    # Blank fill the card.
	    while (op - rp < IDB_RECLEN) {
		Memc[op] = ' '
		op = op + 1
	    }

	    # Add newline termination.
	    Memc[op] = '\n';  op = op + 1
	}

	Memc[op] = EOS

	MEF_HDRP(mef) = hd
	MEF_HSIZE(mef) = strlen(Memc[hd])

	call sfree (sp)
end

