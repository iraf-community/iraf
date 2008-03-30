# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<gio.h>
include	"gtr.h"

# GRC_READ -- Fill the frame buffer from a metacode spool file and redraw
# the screen.  The contents of the frame buffer are overwritten.

procedure grc_read (tr, stream, fname)

pointer	tr			# graphics descriptor
int	stream			# graphics stream
char	fname[ARB]		# metacode file

size_t	sz_val
pointer	sp, lbuf, op
int 	fd, nchars, filelen
long	fstatl()
pointer	gtr_writep()
int	open()
long	read()
errchk	read
define	err_ 91

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (lbuf, sz_val, TY_CHAR)

	iferr (fd = open (fname, READ_ONLY, BINARY_FILE)) {
	    call grc_message (stream, " - cannot open file")
	    call sfree (sp)
	    return
	}

	filelen = fstatl (fd, F_FILESIZE)
	call sprintf (Memc[lbuf], SZ_LINE, " - file size %d chars")
	    call pargi (filelen)
	call grc_message (stream, Memc[lbuf])

	# Discard the current frame.
	call gtr_frame (tr, TR_FRAMEBUF(tr), stream)

	# Read new frame buffer.
	nchars = filelen
	if (nchars <= 0)
	    goto err_
	op = gtr_writep (stream, nchars)
	sz_val = nchars
	if (read (fd, Mems[op], sz_val) < nchars)
	    goto err_

	# Redraw the new frame buffer.
	call gtr_redraw (stream)

	call close (fd)
	call sfree (sp)
	return
err_
	call close (fd)
	call grc_message (stream, " [READ ERROR]")
	call sfree (sp)
end
