# Routines to deal with image kernel extensions
# XT_IMROOT   -- Get root name of an image minus it's image kernel extention
# XT_IMEXT    -- Get image kernel extension with the period.
# XT_IMNAMEEQ -- Check if two image names are equal.


# XT_IMROOT -- Get root name of an image minus it's image kernel extention
# This calls the IKI routines which is an interface violation.

procedure xt_imroot (image, root, maxchar)

char	image[ARB]		# Full image name
char	root[maxchar]		# Root name
int	maxchar			# Size of root name string

int	i, fnextn(), iki_validextn(), strlen()
pointer	sp, extn

begin
	call smark (sp)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	call imgimage (image, root, maxchar)
	i = fnextn (root, Memc[extn], SZ_FNAME)
	if (i > 0) {
	    call iki_init()
	    if (iki_validextn (0, Memc[extn]) != 0)
		root[strlen(root)-i] = EOS
	}

	call sfree (sp)
end


# XT_IMEXT -- Get image kernel extension with the period.
# This calls the IKI routines which is an interface violation.

procedure xt_imext (image, ext, maxchar)

char	image[ARB]		# Full image name
char	ext[maxchar]		# Extension
int	maxchar			# Size of extension

int	i, fnextn(), iki_validextn()
pointer	sp, root

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	ext[1] = EOS

	# Get root and extension
	call imgimage (image, Memc[root], SZ_LINE)
	i = fnextn (Memc[root], ext[2], maxchar-1)
	if (i > 0) {
	    call iki_init()
	    if (iki_validextn (0, ext[2]) != 0)
		ext[1] = '.'
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
