# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "mtio.h"

# MTFNAME -- Edit the input mtname (magtape file specification) to reference
# the numbered file.  The edited mtname is returned in the output string.

procedure mtfname (mtname, fileno, outstr, maxch)

char	mtname[ARB]			#I magtape device specification
int	fileno				#I desired file number
char	outstr[ARB]			#O output mtname string
int	maxch				#I maxch chars out

int	ofileno, orecno
pointer	sp, device, devcap

begin
	call smark (sp)
	call salloc (device, SZ_DEVICE, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	call mtparse (mtname, Memc[device], SZ_DEVICE, ofileno, orecno,
	    Memc[devcap], SZ_DEVCAP)
	call mtencode (outstr, maxch,
	    Memc[device], fileno, orecno, Memc[devcap])

	call sfree (sp)
end
