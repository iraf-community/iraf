# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGIMAGE -- Get the name of an individual image within a cluster of images,
# i.e., the image name minus any image section.

procedure imgimage (imspec, image, maxch)

char	imspec[ARB]		# full image specification
char	image[ARB]		# receives image name
int	maxch

size_t	sz_val
int	cl_index, cl_size
pointer	sp, cluster, ksection, section

begin
	call smark (sp)
	sz_val = SZ_PATHNAME
	call salloc (cluster, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (ksection, sz_val, TY_CHAR)
	call salloc (section, sz_val, TY_CHAR)

	call imparse (imspec,
	    Memc[cluster], SZ_PATHNAME,
	    Memc[ksection], SZ_FNAME,
	    Memc[section], SZ_FNAME, cl_index, cl_size)

	if (cl_index >= 0 && cl_size == -1) {
	    call sprintf (image, maxch, "%s[%d]")
		call pargstr (Memc[cluster])
		call pargi (cl_index)
	} else if (cl_index >= 0 && cl_size > 0) {
	    call sprintf (image, maxch, "%s[%d/%d]")
		call pargstr (Memc[cluster])
		call pargi (cl_index)
		call pargi (cl_size)
	} else
	    call strcpy (Memc[cluster], image, maxch)

	call strcat (Memc[ksection], image, maxch)
	call sfree (sp)
end
