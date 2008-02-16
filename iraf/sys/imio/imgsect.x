# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGSECTION -- Get the image section field from an image specifcation.

procedure imgsection (imspec, section, maxch)

char	imspec[ARB]		# full image specifcation
char	section[ARB]		# receives image section
int	maxch

int	cl_index, cl_size
pointer	sp, cluster, ksection

begin
	call smark (sp)
	call salloc (cluster, SZ_PATHNAME, TY_CHAR)
	call salloc (ksection, SZ_FNAME, TY_CHAR)

	call imparse (imspec, Memc[cluster], SZ_PATHNAME,
	    Memc[ksection], SZ_FNAME, section, maxch, cl_index, cl_size)

	call sfree (sp)
end
