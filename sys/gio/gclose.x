# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GCLOSE -- Close a graphics stream previously opened with GOPEN.  Flush any
# buffered polyline output, output the close worstation metacode instruction,
# close the output stream, close the graphcap descriptor, and return all
# buffer space.

procedure gclose (gp)

pointer	gp			# graphics descriptor

int	fd
int	and()

begin
	fd = GP_FD(gp)

	if (and (GP_GFLAGS(gp), GF_WSOPEN) != 0) {
	    call gflush (gp)
	    call gki_closews (fd, GP_DEVNAME(gp))

	    # If the output stream is a file rather than a standard graphics
	    # stream, write a WCS savefile to permit restoration of the WCS if
	    # the device is subsequently opened in APPEND mode.

	    if (fd > STDPLOT)
		call gwrwcs (GP_DEVNAME(gp),
		    Memi[GP_WCSPTR(gp,1)], LEN_WCSARRAY)

	    # If the output file was opened by GOPEN (as indicated by the
	    # CLOSEFD flag), close the file.

	    if (and (GP_GFLAGS(gp), GF_CLOSEFD) != 0)
		call close (fd)
	    else
		call flush (fd)
	}

	call ttycdes (GP_TTY(gp))
	call mfree (gp, TY_STRUCT)
	call gki_redir (fd, NULL, NULL, NULL)
	call gexfls_clear (fd)
end
