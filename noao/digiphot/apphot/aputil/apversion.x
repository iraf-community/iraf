
# APOVERSION -- Procedure to compute the next available version number of a
# given file name template and output the new files name.

procedure apoversion (template, filename, maxch)

char	template[ARB]			# name template
char	filename[ARB]			# output name
int	maxch				# maximum number of characters

char	period
int	newversion, version, len
pointer	sp, list, name
int	fntgfnb() strldx(), ctoi()
pointer	fntopnb()

begin
	# Allocate temporary space
	call smark (sp)
	call salloc (name, maxch, TY_CHAR)
	period = '.'
	list = fntopnb (template, NO)

	# Loop over the names in the list searchng for the highest version.
	newversion = 0
	while (fntgfnb (list, Memc[name], maxch) != EOF) {
	    len = strldx (period, Memc[name])
	    len = len + 1
	    if (ctoi (Memc[name], len, version) <= 0)
		next
	    newversion = max (newversion, version)
	}

	# Make new output file name.
	len = strldx (period, template)
	call strcpy (template, filename, len)
	call sprintf (filename[len+1], maxch, "%d")
	    call pargi (newversion + 1)

	call fntclsb (list)
	call sfree (sp)
end


# APIVERSION -- Procedure to compute the next available version number of a
# given file name template and output the new files name.

procedure apiversion (template, filename, maxch)

char	template[ARB]			# name template
char	filename[ARB]			# output name
int	maxch				# maximum number of characters

char	period
int	newversion, version, len
pointer	sp, list, name
int	fntgfnb() strldx(), ctoi()
pointer	fntopnb()

begin
	# Allocate temporary space
	call smark (sp)
	call salloc (name, maxch, TY_CHAR)
	period = '.'
	list = fntopnb (template, NO)

	# Loop over the names in the list searchng for the highest version.
	newversion = 0
	while (fntgfnb (list, Memc[name], maxch) != EOF) {
	    len = strldx (period, Memc[name])
	    len = len + 1
	    if (ctoi (Memc[name], len, version) <= 0)
		next
	    newversion = max (newversion, version)
	}

	# Make new output file name.
	len = strldx (period, template)
	call strcpy (template, filename, len)
	call sprintf (filename[len+1], maxch, "%d")
	    call pargi (newversion)

	call fntclsb (list)
	call sfree (sp)
end
