# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include	"gtr.h"

# GTR_PAGE -- Prepare the workstation for output of one or more pages of text.
# Whether or not the terminal is paged is optional.  On terminals where the
# text and graphics are overlaid, it is possible to run the text by beneath
# the plot without affecting the plot.

procedure gtr_page (fd, stream)

int	fd			# output file
int	stream			# graphics stream

pointer	tr
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (stream)

	if (TR_PAGE(tr) == YES)
	    call gki_deactivatews (stream, AW_CLEAR)
	else
	    call gki_deactivatews (stream, 0)

	TR_WAITPAGE(tr) = YES
end
