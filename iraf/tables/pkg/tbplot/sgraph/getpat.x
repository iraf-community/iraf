int procedure getpat (param)

#  GETPAT -- Find the index of the pattern style

char	param[ARB]

pointer	sp
pointer	dict, word
int	fpind

int	clgwrd()

begin
	call smark (sp)
	call salloc (dict, SZ_LINE, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)

	call clgstr ("barpat.p_min", Memc[dict], SZ_LINE)

	fpind = clgwrd (param, Memc[word], SZ_LINE, Memc[dict])

	call sfree (sp)

	return (fpind)
end
