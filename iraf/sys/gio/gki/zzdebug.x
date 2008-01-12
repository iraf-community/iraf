# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <fio.h>
include <gki.h>

task	ggcur	= t_ggcur


# GGCUR -- Debug cursor read in inline graphics kernel.

procedure t_ggcur()

pointer	gp
char	device[SZ_FNAME]

real	cx, cy
int	key, xres, yres, hardchar
int	dd[LEN_GKIDD]
pointer	gopen()

begin
	call clgstr ("device", device, SZ_FNAME)
	hardchar = YES
	xres = 0
	yres = 0

	call fseti (STDGRAPH, F_TYPE, SPOOL_FILE)
	call fseti (STDGRAPH, F_CANCEL, OK)

	call stg_open (device, dd, STDIN, STDOUT, xres, yres, hardchar)
	call gki_inline_kernel (STDGRAPH, dd)

	gp = gopen (device, NEW_FILE, STDGRAPH)
	call ggcur (gp, cx, cy, key)

	call gclose (gp)
	call stg_close()

	call printf ("cx=%f, cy=%f, key=%d\n")
	    call pargr (cx)
	    call pargr (cy)
	    call pargi (key)
end
