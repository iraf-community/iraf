# MK_OUTNAME -- Procedure to construct an daophot output file name.
# If output is null or a directory a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.
#
#procedure mk_outname (image, output, ext, name, maxch)
#
#char	image[ARB]		# image name
#char	output[ARB]		# output directory or name
#char	ext[ARB]		# extension
#char	name[ARB]		# output name
#int	maxch			# maximum size of name
#
#int	ndir
#pointer	sp, root
#int	fnldir(), strlen(), mk_imroot()
#
#begin
#	call smark (sp)
#	call salloc (root, SZ_FNAME, TY_CHAR)
#	call imgimage (image, Memc[root], maxch)
#
#	ndir = fnldir (output, name, maxch)
#	if (strlen (output) == ndir) {
#	    ndir = ndir + mk_imroot (Memc[root], name[ndir+1], maxch)
#	    call sprintf (name[ndir+1], maxch, ".%s.*")
#		call pargstr (ext)
#	    call mk_version (name, name, maxch)
#	} else
#	    call strcpy (output, name, maxch)
#
#	call sfree (sp)
#end


# MK_IMROOT -- Procedure to fetch the root image name minus the directory
# specification and the section notation. The length of the root name is
# returned.
#
#int procedure mk_imroot (image, root, maxch)
#
#char	image[ARB]		# image specification
#char	root[ARB]		# rootname
#int	maxch			# maximum number of characters
#
#int	nchars
#pointer	sp, str
#int	fnldir(), strlen()
#
#begin
#	call smark (sp)
#	call salloc (str, SZ_FNAME, TY_CHAR)
#
#	call imgimage (image, root, maxch)
#	nchars = fnldir (root, Memc[str], maxch)
#	call strcpy (root[nchars+1], root, maxch)
#
#	call sfree (sp)
#	return (strlen (root))
#end


# MK_VERSION -- Routine to compute the next available version number of a given
# file name template and output the new files name.
#
#procedure mk_version (template, filename, maxch)
#
#char	template[ARB]			# name template
#char	filename[ARB]			# output name
#int	maxch				# maximum number of characters
#
#char	period
#int	newversion, version, len, ip
#pointer	sp, list, name
#int	fntgfnb() strldx(), ctoi()
#pointer	fntopnb()
#
#begin
#	# Allocate temporary space
#	call smark (sp)
#	call salloc (name, maxch, TY_CHAR)
#	period = '.'
#	list = fntopnb (template, NO)
#	len = strldx (period, template)
#
#	# Loop over the names in the list searchng for the highest version.
#	newversion = 0
#	while (fntgfnb (list, Memc[name], maxch) != EOF) {
#	    len = strldx (period, Memc[name])
#	    ip = len + 1
#	    if (ctoi (Memc[name], ip, version) <= 0)
#		next
#	    newversion = max (newversion, version)
#	}
#
#	# Make new output file name.
#	call strcpy (template, filename, len)
#	call sprintf (filename[len+1], maxch, "%d")
#	    call pargi (newversion + 1)
#
#	call fntclsb (list)
#	call sfree (sp)
#end


# MK_IMNAME -- Procedure to construct an output image name.
# If output is null or a directory a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.

procedure mk_imname (image, output, ext, name, maxch)

char    image[ARB]              # image name
char    output[ARB]             # output directory or name
char    ext[ARB]                # extension
char    name[ARB]               # output name
int     maxch                   # maximum size of name

int     ndir, nimdir, clindex, clsize
pointer sp, root, str
int     fnldir(), strlen()

begin
        call smark (sp)
        call salloc (root, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_FNAME, TY_CHAR)

        ndir = fnldir (output, name, maxch)
        if (strlen (output) == ndir) {
            call imparse (image, Memc[root], SZ_FNAME, Memc[str], SZ_FNAME,
                Memc[str], SZ_FNAME, clindex, clsize)
            nimdir = fnldir (Memc[root], Memc[str], SZ_FNAME)
            if (clindex >= 0) {
                call sprintf (name[ndir+1], maxch, "%s%d.%s.*")
                    call pargstr (Memc[root+nimdir])
                    call pargi (clindex)
                    call pargstr (ext)
            } else {
                call sprintf (name[ndir+1], maxch, "%s.%s.*")
                    call pargstr (Memc[root+nimdir])
                    call pargstr (ext)
            }
            call mk_oimversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


# MK_OIMVERSION -- Routine to compute the next available version number of
# a given file name template and output the new files name.

procedure mk_oimversion (template, filename, maxch)

char    template[ARB]                   # name template
char    filename[ARB]                   # output name
int     maxch                           # maximum number of characters

char    period
int     newversion, version, len
pointer sp, list, name
int     imtopen(), imtgetim(), strldx(), ctoi()

begin
        # Allocate temporary space
        call smark (sp)
        call salloc (name, maxch, TY_CHAR)
        period = '.'
        list = imtopen (template)

        # Loop over the names in the list searchng for the highest version.
        newversion = 0
        while (imtgetim (list, Memc[name], maxch) != EOF) {
            len = strldx (period, Memc[name])
            Memc[name+len-1] = EOS
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

        call imtclose (list)
        call sfree (sp)
end



# MK_IMNAME -- Procedure to construct an daophot output image name.
# If output is null or a directory a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.
#
#procedure mk_imname (image, output, ext, name, maxch)
#
#char	image[ARB]		# image name
#char	output[ARB]		# output directory or name
#char	ext[ARB]		# extension
#char	name[ARB]		# output name
#int	maxch			# maximum size of name
#
#int	ndir
#pointer	sp, root
#int	fnldir(), strlen(), mk_imroot()
#
#begin
#	call smark (sp)
#	call salloc (root, SZ_FNAME, TY_CHAR)
#	call imgimage (image, Memc[root], maxch)
#
#	ndir = fnldir (output, name, maxch)
#	if (strlen (output) == ndir) {
#	    ndir = ndir + mk_imroot (Memc[root], name[ndir+1], maxch)
#	    call sprintf (name[ndir+1], maxch, ".%s.*")
#		call pargstr (ext)
#	    call mk_imversion (name, name, maxch)
#	} else
#	    call strcpy (output, name, maxch)
#
#	call sfree (sp)
#end


# MK_VERSION -- Routine to compute the next available version number of a given
# file name template and output the new files name.
#
#procedure mk_imversion (template, filename, maxch)
#
#char	template[ARB]			# name template
#char	filename[ARB]			# output name
#int	maxch				# maximum number of characters
#
#char	period
#int	newversion, version, len, ip
#pointer	sp, list, name
#int	fntgfnb() strldx(), ctoi()
#pointer	fntopnb()
#
#begin
#	# Allocate temporary space
#	call smark (sp)
#	call salloc (name, maxch, TY_CHAR)
#	period = '.'
#	list = fntopnb (template, NO)
#	len = strldx (period, template)
#
#	# Loop over the names in the list searchng for the highest version.
#	newversion = 0
#	while (fntgfnb (list, Memc[name], maxch) != EOF) {
#	    len = strldx (period, Memc[name])
#	    Memc[name+len-1] = EOS
#	    len = strldx (period, Memc[name])
#	    ip = len + 1
#	    if (ctoi (Memc[name], ip, version) <= 0)
#		next
#	    newversion = max (newversion, version)
#	}
#
#	# Make new output file name.
#	call strcpy (template, filename, len)
#	call sprintf (filename[len+1], maxch, "%d")
#	    call pargi (newversion + 1)
#
#	call fntclsb (list)
#	call sfree (sp)
#end
