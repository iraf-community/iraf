# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	imt	= t_imt

# IMT -- Test the image template package.

procedure t_imt()

char	template[SZ_LINE]
char	image[SZ_FNAME]
pointer	imt, imtopen()
int	imtgetim()

begin
	call clgstr ("template", template, SZ_LINE)
	imt = imtopen (template)

	while (imtgetim (imt, image, SZ_FNAME) != EOF) {
	    call printf ("%s\n")
		call pargstr (image)
	}

	call imtclose (imt)
end
