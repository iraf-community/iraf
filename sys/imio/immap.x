# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMMAP -- Map an imagefile to an image structure.  This is the "open"
# procedure for an imagefile.

pointer procedure immap (imspec, acmode, hdr_arg)

char	imspec[ARB]		# image specification
int	acmode			# image access mode
int	hdr_arg			# length of user fields, or header pointer

pointer	sp, cluster, extn
int	fnextn(), strsearch()
pointer	immapz(), im_pmmap()
bool	streq()

begin
	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	call iki_init()

	# Open a pixel list?
	if (strsearch (imspec, ".pl") > 0) {
	    call imgcluster (imspec, Memc[cluster], SZ_FNAME)
	    if (fnextn (Memc[cluster], Memc[extn], SZ_FNAME) > 0)
		if (streq (Memc[extn], "pl")) {
		    call sfree (sp)
		    return (im_pmmap (imspec, acmode, NULL))
		}
	}

	call sfree (sp)
	return (immapz (imspec, acmode, hdr_arg))
end
