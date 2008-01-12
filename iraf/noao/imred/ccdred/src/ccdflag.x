# CCDFLAG -- Determine if a CCD processing flag is set.  This is less than
# obvious because of the need to use the default value to indicate a
# false flag.

bool procedure ccdflag (im, name)

pointer	im		# IMIO pointer
char	name[ARB]	# CCD flag name

bool	flag, strne()
pointer	sp, str1, str2

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Get the flag string value and the default value.
	# The flag is true if the value and the default do not match.

	call hdmgstr (im, name, Memc[str1], SZ_LINE)
	call hdmgdef (name, Memc[str2], SZ_LINE)
	flag = strne (Memc[str1], Memc[str2])

	call sfree (sp)
	return (flag)
end
