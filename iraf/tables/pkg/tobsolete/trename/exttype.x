include	<tbset.h>

# EXT_TYPE -- Determine table type from its extension

int procedure ext_type (table)

char	table[ARB]	# i: table name
#--
int	nc, fd, filetype, hdu
pointer	sp, fname, extname

int	tbparse(), fstdfile()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	if (fstdfile (table, fd) == YES) {
	    filetype = TBL_TYPE_TEXT

	} else {
	    nc = tbparse (table, Memc[fname], Memc[extname], 
			  SZ_FNAME, filetype, hdu)
	}

	call sfree (sp)
	return (filetype)
end
