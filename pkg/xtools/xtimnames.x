# Routines to deal with image kernel extensions
# XT_IMROOT   -- Get root name of an image minus it's image kernel extention
# XT_IMEXT    -- Get image kernel extension with the period.
# XT_IMNAMEEQ -- Check if two image names are equal.


# XT_IMROOT -- Get root name of an image minus it's image kernel extention

procedure xt_imroot (image, root, maxchar)

char	image[ARB]		# Full image name
char	root[maxchar]		# Root name
int	maxchar			# Size of root name string

int	i, strlen()

begin
	call imgimage (image, root, maxchar)
	i = strlen (root)
	switch (root[i]) {
	case 'h':							# .??h
	    if (i > 3 && root[i-3] == '.')
		root[i-3] = EOS
	case 'l':							# .pl
	    if (i > 2 && root[i-2] == '.' && root[i-1] == 'p')
		root[i-2] = EOS
	case 's':							# .fits
	    if (i > 4 && root[i-4] == '.' && root[i-3] == 'f' &&
		root[i-2] == 'i' && root[i-1] == 't')
		root[i-4] = EOS
	case 't':							# .fit
	    if (i > 3 && root[i-3] == '.' && root[i-2] == 'f' &&
		root[i-1] == 'i')
		root[i-3] = EOS
	}
end


# XT_IMEXT -- Get image kernel extension with the period.

procedure xt_imext (image, ext, maxchar)

char	image[ARB]		# Full image name
char	ext[maxchar]		# Extension
int	maxchar			# Size of extension

int	i, strlen()
pointer	sp, root

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	# Get root and extension
	call imgimage (image, Memc[root], SZ_LINE)
	ext[1] = EOS
	i = strlen (Memc[root])
	switch (Memc[root+i-1]) {
	case 'h':							# .??h
	    if (i > 3 && Memc[root+i-4] == '.')
		call strcpy (Memc[root+i-4], ext, maxchar)
	case 'l':							# .pl
	    if (i > 2 && Memc[root+i-3] == '.' && Memc[root+i-2] == 'p')
		call strcpy (Memc[root+i-3], ext, maxchar)
	case 's':							# .fits
	    if (i > 4 && Memc[root+i-5] == '.' && Memc[root+i-4] == 'f' &&
		Memc[root+i-3] == 'i' && Memc[root+i-2] == 't')
		call strcpy (Memc[root+i-5], ext, maxchar)
	case 't':							# .fit
	    if (i > 3 && Memc[root+i-4] == '.' && Memc[root+i-3] == 'f' &&
		Memc[root+i-2] == 'i')
		call strcpy (Memc[root+i-5], ext, maxchar)
	}
	call sfree (sp)
end


# XT_IMNAMEEQ -- Check if two image names are equal.
# Image sections and clusters are removed.  If an image extension is missing
# it is assumed the same as the other image; i.e. only if both names
# have extensions are the extensions checked for equality.

bool procedure xt_imnameeq (imname1, imname2)

char	imname1[ARB]		# First image name
char	imname2[ARB]		# Second image name

bool	stat, streq()
pointer	sp, str1, str2

begin
	if (streq (imname1, imname2))
	    return (true)

	call smark (sp)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	# Check roots
	call xt_imroot (imname1, Memc[str1], SZ_FNAME)
	call xt_imroot (imname2, Memc[str2], SZ_FNAME)
	stat = streq (Memc[str1], Memc[str2])

	# If the roots are equal check the extensions.
	if (stat) {
	    call xt_imext (imname1, Memc[str1], SZ_FNAME)
	    call xt_imext (imname2, Memc[str2], SZ_FNAME)
	    if (Memc[str1] != EOS && Memc[str2] != EOS)
		stat = streq (Memc[str1], Memc[str2])
	}

	call sfree (sp)
	return (stat)
end
