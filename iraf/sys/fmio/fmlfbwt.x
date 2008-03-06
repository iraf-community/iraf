# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	"fmio.h"

# FM_LFBINWAIT -- Wait for i/o on a binary lfile.

procedure fm_lfbinwait (lf_chan, status)

int	lf_chan			#I lfile descriptor
long	status			#O i/o status (nbytes transferred or ERR)

pointer	lf, fm
int	chan

include "fmio.com"

begin
	lf = Memp[lf_ptrs+lf_chan]
	fm = LF_FM(lf)
	chan = FM_CHAN(fm)

	# Wait for i/o and increment byte count.
	if (and (LF_FLAGS(lf), LFF_IOINPROGRESS) != 0) {
	    call zawtbf (chan, status)
	    if (status >= 0)
		LF_STATUS(lf) = LF_STATUS(lf) + min(LF_LTSIZE(lf),status)
	    else
		LF_STATUS(lf) = ERR
	    LF_FLAGS(lf) = and (LF_FLAGS(lf), not(LFF_IOINPROGRESS))
	}

	call fmio_tick (fm)
	status = LF_STATUS(lf)
end
