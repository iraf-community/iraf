# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"stf.h"

# STF_REBLOCK -- If the user area is not blocked to fixed length records, e.g.,
# as is possible in a new copy image, reblock it fixed length.

procedure stf_reblock (im)

pointer	im			# image descriptor

pointer	sp, lbuf, op, ua
int	fd, spool, nlines, nchars, sz_userarea, len_hdrmem
errchk	stropen, open, getline, putline, realloc, seek, fcopyo
int	open(), stropen(), getline()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	ua = IM_USERAREA(im)
	fd = stropen (Memc[ua], ARB, READ_ONLY)
	spool = open ("rb_spool", READ_WRITE, SPOOL_FILE)
	
	# Reblock into a spool file, counting the lines.
	for (nlines=0;  ;  nlines=nlines+1) {
	    nchars = getline (fd, Memc[lbuf])
	    if (nchars <= 0)
		break

	    for (op=nchars;  op <= FITS_RECLEN;  op=op+1)
		Memc[lbuf+op-1] = ' '
	    Memc[lbuf+FITS_RECLEN] = '\n'
	    Memc[lbuf+FITS_RECLEN+1] = EOS

	    call putline (spool, Memc[lbuf])
	}

	call close (fd)

	# Reallocate header the right size.
	sz_userarea = nlines * (FITS_RECLEN+1) + SZ_EXTRASPACE

	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_MII_INT-1) / SZ_MII_INT
	len_hdrmem = LEN_IMHDR +
	    (sz_userarea+1 + SZ_MII_INT-1) / SZ_MII_INT

	if (IM_LENHDRMEM(im) < len_hdrmem) {
	    IM_LENHDRMEM(im) = len_hdrmem
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}

	# Move spooled data back to user area.
	ua = IM_USERAREA(im)
	fd = stropen (Memc[ua], sz_userarea, NEW_FILE)
	call seek (spool, BOFL)
	call fcopyo (spool, fd)

	call close (fd)
	call close (spool)
	call sfree (sp)
end
