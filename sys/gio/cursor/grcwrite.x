# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<gio.h>
include	"gtr.h"
include	"grc.h"

# GRC_WRITE -- Write the contents of the frame buffer to a file, with or
# without applying the workstation transformation, optionally clobbering
# any existing file of the same name.

procedure grc_write (tr, stream, fname, clobber, fullframe)

pointer	tr			# graphics stream descriptor
int	stream			# graphics stream
char	fname[ARB]		# file name
bool	clobber			# clobber existing file
bool	fullframe		# write full frame (no workstation transform)

pointer	sp, lbuf
long	size1, size2
int	save1, save2, fd, nchars
long	fstatl()
int	open()
errchk	write, gtr_redraw

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Delete existing file if clobber requested.
	if (clobber)
	    iferr (call delete (fname))
		;

	# Open metacode spool file for appending.
	iferr (fd = open (fname, APPEND, BINARY_FILE)) {
	    call grc_message (stream, " - cannot open file for appending")
	    call sfree (sp)
	    return
	}

	# Write either the full frame or the displayed frame into spool file.

	size1 = fstatl (fd, F_FILESIZE)
	if (fullframe) {
	    nchars = (TR_OP(tr) - TR_FRAMEBUF(tr)) * SZ_SHORT
	    call write (fd, Mems[TR_FRAMEBUF(tr)], nchars)
	} else {
	    call gki_redir  (stream, fd, save1, save2)
	    call gtr_redraw (stream)
	    call gki_redir  (stream,  0, save1, save2)
	}

	size2 = fstatl (fd, F_FILESIZE)
	call sprintf (Memc[lbuf], SZ_LINE, " - %d chars %s")
	    call pargi (size2 - size1)
	if (size1 > 0)
	    call pargstr ("appended")
	else
	    call pargstr ("")
	call grc_message (stream, Memc[lbuf])

	call close (fd)
	call sfree (sp)
end
