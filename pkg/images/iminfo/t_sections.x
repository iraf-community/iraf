# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SECTIONS -- Expand a image template into a list of images on the
# standard output and record the number of sections in a parameter.

procedure t_sections()

char	images[SZ_LINE]				# Image template
char	image[SZ_FNAME]
char	str[SZ_LINE]
int	option, list
int	clgwrd(), imtopen(), imtgetim(), imtlen()

begin
	call clgstr ("images", images, SZ_LINE)
	option = clgwrd ("option", str, SZ_LINE,
	    ",nolist,fullname,root,section,")
	list = imtopen (images)

	call clputi ("nimages", imtlen (list))

	while (imtgetim (list, image, SZ_FNAME) != EOF) {
	    switch (option) {
	    case 2:
	        call printf ("%s\n")
		    call pargstr (image)
	    case 3:
		call get_root (image, str, SZ_LINE)
	        call printf ("%s\n")
		    call pargstr (str)
	    case 4:
		call get_section (image, str, SZ_LINE)
	        call printf ("%s\n")
		    call pargstr (str)
	    }
	}

	call imtclose (list)
end
