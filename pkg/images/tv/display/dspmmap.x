# DS_PMMAP -- Open a pixel mask READ_ONLY.

pointer procedure ds_pmmap (pmname, refim)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer

pointer	sp, mname
pointer	im, yt_mappm()
errchk	yt_mappm

begin
	call smark (sp)
	call salloc (mname, SZ_FNAME, TY_CHAR)

	im = yt_mappm (pmname, refim, "pmmatch", Memc[mname], SZ_FNAME)

	call sfree (sp)
	return (im)
end
