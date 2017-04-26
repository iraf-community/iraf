# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmio.h"

# FM_LFCLOSE -- Close an lfile descriptor.  There isn't much for us to do,
# since the physical datafile remains open, and opening an lfile does not
# allocate a descriptor.

procedure fm_lfclose (lf, status)

pointer	lf			#I lfile descriptor
int	status			#O i/o status (nbytes transferred or ERR)

pointer	fm

begin
	fm = LF_FM(lf)

	if (fm == NULL)
	    status = ERR
	else if (FM_MAGIC(fm) != FMIO_MAGIC)
	    status = ERR
	else
	    status = OK

	call fmio_tick (fm)
end
