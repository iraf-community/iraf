# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <knet.h>

# IMDOPEN -- Open the image display device as a binary file.

int procedure imdopen (fname, access_mode) 

char	fname[ARB]
int	access_mode, fopnbf()
extern	zopnim(), zclsim(), zardim(), zawrim(), zawtim(), zsttim()

begin
	return (fopnbf (fname, access_mode,
		zopnim, zardim, zawrim, zawtim, zsttim, zclsim))
end
