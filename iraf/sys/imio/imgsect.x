# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGSECTION -- Get the image section field from an image specifcation.

procedure imgsection (imspec, section, maxch)

char	imspec[ARB]		# full image specifcation
char	section[ARB]		# receives image section
int	maxch

size_t	sz_val
int	cl_index, cl_size
pointer	sp, cluster, ksection

begin
	call smark (sp)
	sz_val = SZ_PATHNAME
	call salloc (cluster, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (ksection, sz_val, TY_CHAR)

	call imparse (imspec, Memc[cluster], SZ_PATHNAME,
	    Memc[ksection], SZ_FNAME, section, maxch, cl_index, cl_size)

	call sfree (sp)
end
