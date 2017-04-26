# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gescape.h>
include	<fset.h>
include	<gio.h>

# GIM_IOMAPREAD -- Read a segment of the gterm widget iomap.

procedure gim_iomapread (gp, iomap, first, nelem)

pointer	gp			#I graphics descriptor
int	iomap[ARB]		#o iomap data
int	first			#I first iomap cell to be read
int	nelem			#I number of elements to read

int	nchars
pointer	sp, data
short	gim[GIM_READIOMAP_LEN]
short	retval[GIM_RET_RIOMAP_LEN]
int	read()

string	s_readiomap "gim_iomapread"
errchk	flush, read, syserrs

begin
	call smark (sp)
	call gpl_flush()

	gim[GIM_READIOMAP_FC] = first
	gim[GIM_READIOMAP_NC] = nelem
	call gescape (gp, GIM_READIOMAP, gim, GIM_READIOMAP_LEN)
	call flush (GP_FD(gp))

        # Get return value instruction header.
        nchars = GIM_RET_RIOMAP_LEN * SZ_SHORT
        if (read (GP_FD(gp), retval, nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readiomap)
	}

        if (retval[GIM_RET_RIOMAP_NC] != nelem)
            call syserrs (SYS_FREAD, s_readiomap)

	# Get the iomap data.
	call salloc (data, nelem, TY_SHORT)
	nchars = nelem * SZ_SHORT
        if (read (GP_FD(gp), Mems[data], nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readiomap)
	} else
	    call achtsi (Mems[data], iomap, nelem)

	call fseti (GP_FD(gp), F_CANCEL, OK)
	call sfree (sp)
end
