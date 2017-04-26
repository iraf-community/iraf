# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fset.h>
include "../lib/ids.h"
include "iis.h"

# ZSETUP -- Setup up common block information for read/write

procedure zsetup (frame, bitpl, flag)

short	frame[ARB]			# frame information
short	bitpl[ARB]			# bitplane information
bool	flag				# true if image, false if graphics

short	iispack()
int	mapcolor()

include "iis.com"
include "../lib/ids.com"

begin
	# If don't flush, then last line of "previous" frame
	# may get steered to wrong image plane
	call flush (i_out)
	call fseti (i_out, F_CANCEL, OK)
	if ( flag ) {
	    iframe = iispack ( frame )
	    iplane = iispack ( bitpl )
	} else {
	    iframe = GRCHAN
	    iplane = mapcolor( bitpl )
	}
end
