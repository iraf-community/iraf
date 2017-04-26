# IS_WHOLETAB -- Return true if table name has no extension

bool procedure is_wholetab (table)

char	table[ARB]	# i: table name
#--
bool	wholetab
int	nc, hdu
pointer	sp, fname, extname

int	tbparse()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	nc = tbparse (table, Memc[fname], Memc[extname], SZ_FNAME, hdu)

	wholetab = Memc[extname] == EOS

	call sfree (sp)
	return (wholetab)
end
