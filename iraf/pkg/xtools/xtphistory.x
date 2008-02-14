# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# XT_PHISTORY -- Put history string.

procedure xt_phistory (im, str)

pointer	im			# IMIO pointer
char	str			# String to be put in history

size_t	sz_val
pointer	sp, timestr

long	clktime()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (timestr, sz_val, TY_CHAR)
	call cnvdate (clktime (0), Memc[timestr], SZ_LINE)
	call strcat (Memc[timestr], IM_HISTORY(im), SZ_IMHIST)
	call strcat (": ", IM_HISTORY(im), SZ_IMHIST)
	call strcat (str, IM_HISTORY(im), SZ_IMHIST)
	call sfree (sp)
end
