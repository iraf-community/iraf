# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gescape.h>
include	<fset.h>
include	<gio.h>

# GIM_READCOLORMAP -- Read a segment of a colormap.  The number of cells
# read is returned.  The number of cells read may be less than the request
# size if the cells have not yet been allocated.

int procedure gim_readcolormap (gp, colormap, first, maxelem, r, g, b)

pointer	gp			#I graphics descriptor
int	colormap		#I colormap number (0=screen)
int	first			#I first colormap entry to be read
int	maxelem			#I number of elements to read
int	r[ARB],g[ARB],b[ARB]	#O RGB color values (0-255)

pointer	sp, cm, ip
int	ncells, nret, nchars, i
short	gim[GIM_READCMAP_LEN]
short	retval[GIM_RET_RCMAP_LEN]
int	read()

string	s_readcmap "gim_readcolormap"
errchk	flush, read, syserrs

begin
	call smark (sp)
	call gpl_flush()

	gim[GIM_READCMAP_MP] = colormap
	gim[GIM_READCMAP_FC] = first
	gim[GIM_READCMAP_NC] = maxelem
	call gescape (gp, GIM_READCMAP, gim, GIM_READCMAP_LEN)
	call flush (GP_FD(gp))

        # Get return value instruction header.
        nchars = GIM_RET_RCMAP_LEN * SZ_SHORT
        if (read (GP_FD(gp), retval, nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readcmap)
	}

        ncells = retval[GIM_RET_RCMAP_NC]
	call salloc (cm, ncells * 3, TY_SHORT)
	nret = min (ncells, maxelem)

	# Get the colormap data.
	nchars = (ncells * 3) * SZ_SHORT
        if (read (GP_FD(gp), Mems[cm], nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readcmap)
	}
	
	do i = 1, nret {
	    ip = cm + (i - 1) * 3
	    r[i] = Mems[ip+0]
	    g[i] = Mems[ip+1]
	    b[i] = Mems[ip+2]
	}

	call fseti (GP_FD(gp), F_CANCEL, OK)
	call sfree (sp)

	return (nret)
end
