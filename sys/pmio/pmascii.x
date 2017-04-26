# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_ASCIIDUMP -- Dump a two dimensional region of a mask as a printable ASCII
# character array on the output stream.  Intended as a simple debugging tool;
# see also PM_SAVEIM.

procedure pm_asciidump (pl, vs, ve, outfd)

pointer	pl			#I mask descriptor
long	vs[ARB]			#I ll vector (only first two elements used)
long	ve[ARB]			#I ur vector (only first two elements used)
int	outfd			#I output file

include	"pmio.com"

begin
	if (PM_MAPXY(pl) == NO)
	    call pl_asciidump (pl, vs, ve, outfd)
	else {
	    call imaplv (PM_REFIM(pl), vs, v1, PM_MAXDIM)
	    call imaplv (PM_REFIM(pl), ve, v2, PM_MAXDIM)
	    call pl_asciidump (pl, v1, v2, outfd)
	}
end
