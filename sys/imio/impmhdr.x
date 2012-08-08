# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<ctype.h>

.help impmhdr
.nf --------------------------------------------------------------------------
IMPMHDR -- Routines to encode/decode an image header in a title string
such as is provided by pl_[save|load]f, so that general image headers can
be saved in .pl files.

	    nchars = im_pmsvhdr (im, bufp, sz_buf)
		     im_pmldhdr (im, bufp)

The information saved in the plio save file title string consist of a
series of keyword = value assignments, one per line.
.endhelp ---------------------------------------------------------------------

define	DEF_SZBUF	32768
define	INC_SZBUF	16384
define	INC_HDRMEM	8100
define	IDB_RECLEN	80

define	KW_TITLE	"$TITLE = "
define	LEN_KWTITLE	9
define	KW_CTIME	"$CTIME = "
define	LEN_KWCTIME	9
define	KW_MTIME	"$MTIME = "
define	LEN_KWMTIME	9
define	KW_LIMTIME	"$LIMTIME = "
define	LEN_KWLIMTIME	11
define	KW_MINPIXVAL	"$MINPIXVAL = "
define	LEN_KWMINPIXVAL	13
define	KW_MAXPIXVAL	"$MAXPIXVAL = "
define	LEN_KWMAXPIXVAL	13


# IM_PMSVHDR -- Save an image header in a text string as a sequence of
# keyword = value assignments, one per line.  A pointer to a text buffer
# containing the encoded header is returned as the output parameter, and
# the string length in chars is returned as the function value.
# The caller should deallocate this buffer when it is no longer needed.

int procedure im_pmsvhdr (im, bp, sz_buf)

pointer	im				#I image descriptor
pointer	bp				#U buffer containing encoded header
int	sz_buf				#U allocated size of buffer, chars

int	nchars, ualen, ch, i
pointer	sp, tbuf, ip, op, idb, rp
errchk	malloc, realloc, idb_open
int	gstrcpy(), idb_nextcard
pointer	idb_open()

begin
	call smark (sp)
	call salloc (tbuf, SZ_IMTITLE, TY_CHAR)

	# Allocate text buffer if the user hasn't already done so.
	if (bp == NULL || sz_buf <= 0) {
	    sz_buf = DEF_SZBUF
	    call malloc (bp, sz_buf, TY_CHAR)
	}

	# Store title string in buffer.
	call strcpy (IM_TITLE(im), Memc[tbuf], SZ_IMTITLE)
	op = bp + gstrcpy (KW_TITLE, Memc[bp], ARB)
	Memc[op] = '"';  op = op + 1
	for (ip=tbuf;  Memc[ip] != EOS;  ip=ip+1) {
	    if (Memc[ip] == '"') {
		Memc[op] = '\\';  op = op + 1
	    }
	    Memc[op] = Memc[ip];  op = op + 1
	}
	Memc[op] = '"';  op = op + 1
	Memc[op] = '\n';  op = op + 1

	# Store the create time in buffer.
	call sprintf (Memc[tbuf], SZ_IMTITLE, "%d")
	    call pargl (IM_CTIME(im))
	op = op + gstrcpy (KW_CTIME, Memc[op], ARB)
	op = op + gstrcpy (Memc[tbuf], Memc[op], ARB)
	Memc[op] = '\n';  op = op + 1

	# Store the modify time in buffer.
	call sprintf (Memc[tbuf], SZ_IMTITLE, "%d")
	    call pargl (IM_MTIME(im))
	op = op + gstrcpy (KW_MTIME, Memc[op], ARB)
	op = op + gstrcpy (Memc[tbuf], Memc[op], ARB)
	Memc[op] = '\n';  op = op + 1

	# Store the limits time in buffer.
	call sprintf (Memc[tbuf], SZ_IMTITLE, "%d")
	    call pargl (IM_LIMTIME(im))
	op = op + gstrcpy (KW_LIMTIME, Memc[op], ARB)
	op = op + gstrcpy (Memc[tbuf], Memc[op], ARB)
	Memc[op] = '\n';  op = op + 1

	# Store the minimum good pixel value in buffer.
	call sprintf (Memc[tbuf], SZ_IMTITLE, "%g")
	    call pargr (IM_MIN(im))
	op = op + gstrcpy (KW_MINPIXVAL, Memc[op], ARB)
	op = op + gstrcpy (Memc[tbuf], Memc[op], ARB)
	Memc[op] = '\n';  op = op + 1

	# Store the maximum good pixel value in buffer.
	call sprintf (Memc[tbuf], SZ_IMTITLE, "%g")
	    call pargr (IM_MAX(im))
	op = op + gstrcpy (KW_MAXPIXVAL, Memc[op], ARB)
	op = op + gstrcpy (Memc[tbuf], Memc[op], ARB)
	Memc[op] = '\n';  op = op + 1

	# Copy the header cards.  
	idb = idb_open (im, ualen)
	while (idb_nextcard (idb, rp) != EOF) {

	    # Increase the size of the output buffer if it fills.
	    nchars = op - bp
	    if (sz_buf - nchars < IDB_RECLEN) {
		sz_buf = sz_buf + INC_SZBUF
		call realloc (bp, sz_buf, TY_CHAR)
		op = bp + nchars
	    }

	    # Copy the card, stripping any trailing whitespace.
	    nchars = 0
	    do i = 1, IDB_RECLEN {
		ch = Memc[rp+i-1]
		Memc[op+i-1] = ch
		if (!IS_WHITE(ch))
		    nchars = i
	    }

	    op = op + nchars 
	    Memc[op] = '\n';  op = op + 1
	}

	# All done, terminate the string and return any extra space.
	Memc[op] = EOS;  op = op + 1
	nchars = op - bp
	call realloc (bp, nchars, TY_CHAR)

	# Clean up.
	call idb_close (idb)
	call sfree (sp)

	return (nchars)
end


# IM_PMLDHDR -- Load the image header from a save buffer, prepared in a
# previous call to im_pmsvhdr.  The saved header will overwrite any
# existing cards in the output image header.

procedure im_pmldhdr (im, bp)

pointer	im			#I image descriptor
pointer	bp			#I pointer to text buffer (header save buf)

int	hdrlen, sz_ua, nchars, ch, i
pointer	sp, tbuf, ip, op, rp, ua
int	strncmp(), ctol(), ctor()
errchk	realloc

begin
	call smark (sp)
	call salloc (tbuf, SZ_IMTITLE, TY_CHAR)

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
		    call strcpy (Memc[tbuf], IM_TITLE(im), SZ_IMTITLE)

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else if (strncmp (Memc[ip], KW_CTIME, LEN_KWCTIME) == 0) {
		    # Decode the create time.
		    ip = ip + LEN_KWCTIME
		    rp = 1
		    if (ctol (Memc[ip], rp, IM_CTIME(im)) <= 0)
			IM_CTIME(im) = 0
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
		    if (ctol (Memc[ip], rp, IM_MTIME(im)) <= 0)
			IM_MTIME(im) = 0
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
		    if (ctol (Memc[ip], rp, IM_LIMTIME(im)) <= 0)
			IM_LIMTIME(im) = 0
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
		    if (ctor (Memc[ip], rp, IM_MIN(im)) <= 0)
			IM_MIN(im) = 0.0
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
		    if (ctor (Memc[ip], rp, IM_MAX(im)) <= 0)
			IM_MAX(im) = 0.0
		    ip = ip + rp - 1

	    	    # Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1

		} else {
	    	    # No keyword matched. Advance to next line.
	    	    while (Memc[ip] != EOS && Memc[ip] != '\n')
			ip = ip + 1
	    	    if (Memc[ip] == '\n')
			ip = ip + 1
		}
	    } else
		break
	}

	# Get the header keywords.
	hdrlen = LEN_IMDES + IM_LENHDRMEM(im)
	sz_ua = (hdrlen - IMU) * SZ_STRUCT - 1
	ua = IM_USERAREA(im)
	op = ua

	while (Memc[ip] != EOS) {
	    rp = op

	    # Reallocate descriptor if we need more space.  Since we are
	    # called at image map time and the descriptor pointer has not
	    # yet been passed out, the image descriptor can be reallocated.

	    nchars = rp - ua
	    if (nchars + IDB_RECLEN + 2 > sz_ua) {
		hdrlen = hdrlen + INC_HDRMEM
		IM_LENHDRMEM(im) = IM_LENHDRMEM(im) + INC_HDRMEM
		call realloc (im, hdrlen, TY_STRUCT)
		sz_ua = (hdrlen - IMU) * SZ_STRUCT - 1
		ua = IM_USERAREA(im)
		op = ua + nchars
	    }

	    # Copy the saved card, leave IP positioned to past newline.
	    do i = 1, IDB_RECLEN + 1 {
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
	IM_UABLOCKED(im) = YES

	call sfree (sp)
end
