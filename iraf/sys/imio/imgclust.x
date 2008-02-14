# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGCLUSTER -- Get the cluster name of an image, i.e., the name of the cluster
# to which the image belongs, minus the cluster index and image section, if any.

procedure imgcluster (imspec, cluster, maxch)

char	imspec[ARB]		# full image specification
char	cluster[ARB]		# receives root image name
int	maxch

int	cl_index, cl_size
pointer	sp, ksection, section

begin
	call smark (sp)
	call salloc (ksection, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	call imparse (imspec, cluster, maxch, Memc[ksection], SZ_FNAME,
	    Memc[section], SZ_FNAME, cl_index, cl_size)

	call sfree (sp)
end
