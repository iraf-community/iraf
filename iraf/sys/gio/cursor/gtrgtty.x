# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_GTTY -- Get the graphcap descriptor for a stream.

pointer procedure gtr_gtty (stream)

int	stream			# graphics stream of interest

pointer	tr
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (stream)
	return (TR_TTY(tr))
end
