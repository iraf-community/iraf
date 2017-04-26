
define	RS_EXTNLIST "|imh|fits|pl|qpoe|hhh|"


# RS_IMLIST -- Create a list of input masks using the input image list and an
# output template string.

int procedure rs_imlist (inlist, output, defaultstr, extstr)

int	inlist			#I the input image list descriptor
char    output[ARB] 	        #I the input output file list
char    defaultstr[ARB]         #I the defaults id string
char    extstr[ARB]             #I the extension string

pointer	sp, fname, image, dirname, otemplate
int	i, outlist, len_dir, len_otemplate, strfd
int	imtopen(), imtlen(), imtrgetim(), fnldir(), strncmp(), strlen()
int	stropen(), strmatch()
errchk	imtopen()

begin
        # Return if the ouyput file list is empty.
	iferr (outlist = imtopen (output))
            outlist = imtopen ("")
        if (output[1] == EOS || imtlen (outlist) <= 0)
            return (outlist)

        # Return if the output image list is the wrong length.
        if ((imtlen (outlist) > 1) && (imtlen (outlist) != imtlen(inlist))) {
            call imtclose (outlist)
            outlist = imtopen ("")
            return (outlist)
        }

	# Get working space.
        call smark (sp)
        call salloc (fname, SZ_FNAME, TY_CHAR)
        call salloc (image, SZ_FNAME, TY_CHAR)
        call salloc (dirname, SZ_FNAME, TY_CHAR)

        # Get the directory name.
        if (imtrgetim (outlist, 1, Memc[fname], SZ_FNAME) == EOF)
            Memc[fname] = EOS
        len_dir = fnldir (Memc[fname], Memc[dirname], SZ_FNAME)

        # Get the default output file names. There will be one output image per
        # input image.
        if (strncmp (defaultstr, Memc[fname+len_dir],
            strlen (defaultstr)) == 0 || len_dir == strlen (Memc[fname])) {

	    # Create a temporary list string.
            call imtclose (outlist)
            len_otemplate = imtlen (inlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

	    # Loop over the input image list.
            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do i = 1, imtlen (inlist) {

		# Get the root image name.
		if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF)
		    ;

		# Construct the default name.
                call rs_oimname (Memc[image], Memc[dirname], extstr,
		    Memc[fname], SZ_FNAME)
                if (strmatch (Memc[fname], ".pl$") == 0)
                    call strcat (".pl", Memc[fname], SZ_FNAME)


		# Record the file name.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
	    }
            call close (strfd)

	    # Create the final list.
            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            outlist = imtopen (Memc[otemplate])

        # Get the user output names.
        } else {

	    # Create a temporary list string.
            len_otemplate = imtlen (outlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    # Loop over the fields.
            do i = 1, imtlen (inlist) {

                if (imtrgetim (outlist, i, Memc[fname], SZ_FNAME) == EOF)
		    break
                if (strmatch (Memc[fname], ".pl$") == 0)
                    call strcat (".pl", Memc[fname], SZ_FNAME)

	        # Add the output name to the list.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
            }
            call close (strfd)

            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            call imtclose (outlist)
            outlist = imtopen (Memc[otemplate])
        }

        call sfree (sp)

        return (outlist)
end


# RS_OLIST -- Create a list of output images using the input image list and an
# output template string.

int procedure rs_olist (inlist, output, defaultstr, extstr)

int	inlist			#I the input image list descriptor
char    output[ARB] 	        #I the input output file list
char    defaultstr[ARB]         #I the defaults id string
char    extstr[ARB]             #I the extension string

pointer	sp, fname, image, dirname, otemplate
int	i, outlist, len_dir, len_otemplate, strfd
int	imtopen(), imtlen(), imtrgetim(), fnldir(), strncmp(), strlen()
int	stropen()
errchk	imtopen()

begin
        # Return if the input file list is empty.
	iferr (outlist = imtopen (output))
            outlist = imtopen ("")
        if (output[1] == EOS || imtlen (outlist) <= 0)
            return (outlist)

        # Return if the output image list is the wrong length.
        if ((imtlen (outlist) > 1) && (imtlen (outlist) != imtlen(inlist))) {
            call imtclose (outlist)
            outlist = imtopen ("")
            return (outlist)
        }

	# Get working space.
        call smark (sp)
        call salloc (fname, SZ_FNAME, TY_CHAR)
        call salloc (image, SZ_FNAME, TY_CHAR)
        call salloc (dirname, SZ_FNAME, TY_CHAR)

        # Get the directory name.
        if (imtrgetim (outlist, 1, Memc[fname], SZ_FNAME) == EOF)
            Memc[fname] = EOS
        len_dir = fnldir (Memc[fname], Memc[dirname], SZ_FNAME)

        # Get the default output file names. There will be one output image per
        # input image.
        if (strncmp (defaultstr, Memc[fname+len_dir],
            strlen (defaultstr)) == 0 || len_dir == strlen (Memc[fname])) {

	    # Create a temporary list string.
            call imtclose (outlist)
            len_otemplate = imtlen (inlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

	    # Loop over the input image list.
            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do i = 1, imtlen (inlist) {

		# Get the root image name.
		if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF)
		    ;

		# Construct the default name.
                call rs_oimname (Memc[image], Memc[dirname], extstr,
		    Memc[fname], SZ_FNAME)

		# Record the file name.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
	    }
            call close (strfd)

	    # Create the final list.
            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            outlist = imtopen (Memc[otemplate])

        # Get the user output names.
        } else {

	    # Create a temporary list string.
            len_otemplate = imtlen (outlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    # Loop over the fields.
            do i = 1, imtlen (inlist) {

		# Get the output file name.
                if (imtrgetim (outlist, i, Memc[fname], SZ_FNAME) == EOF)
		    break

	        # Add the output name to the list.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
            }
            call close (strfd)

            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            call imtclose (outlist)
            outlist = imtopen (Memc[otemplate])
        }

        call sfree (sp)

        return (outlist)
end


# RS_OMLIST -- Create a list of output masks using the input image list and an
# output template string.

int procedure rs_omlist (inlist, output, defaultstr, extstr)

int	inlist			#I the input image list descriptor
char    output[ARB] 	        #I the input output file list
char    defaultstr[ARB]         #I the defaults id string
char    extstr[ARB]             #I the extension string

pointer	sp, fname, image, dirname, otemplate
int	i, outlist, len_dir, len_otemplate, strfd
int	imtopen(), imtlen(), imtrgetim(), fnldir(), strncmp(), strlen()
int	stropen(), strmatch()
errchk	imtopen()

begin
        # Return if the input file list is empty.
	iferr (outlist = imtopen (output))
            outlist = imtopen ("")
        if (output[1] == EOS || imtlen (outlist) <= 0)
            return (outlist)

        # Return if the output image list is the wrong length.
        if ((imtlen (outlist) > 1) && (imtlen (outlist) != imtlen(inlist))) {
            call imtclose (outlist)
            outlist = imtopen ("")
            return (outlist)
        }

	# Get working space.
        call smark (sp)
        call salloc (fname, SZ_FNAME, TY_CHAR)
        call salloc (image, SZ_FNAME, TY_CHAR)
        call salloc (dirname, SZ_FNAME, TY_CHAR)

        # Get the directory name.
        if (imtrgetim (outlist, 1, Memc[fname], SZ_FNAME) == EOF)
            Memc[fname] = EOS
        len_dir = fnldir (Memc[fname], Memc[dirname], SZ_FNAME)

        # Get the default output file names. There will be one output image per
        # input image.
        if (strncmp (defaultstr, Memc[fname+len_dir],
            strlen (defaultstr)) == 0 || len_dir == strlen (Memc[fname])) {

	    # Create a temporary list string.
            call imtclose (outlist)
            len_otemplate = imtlen (inlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

	    # Loop over the input image list.
            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do i = 1, imtlen (inlist) {

		# Get the root image name.
		if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF)
		    ;

		# Construct the default name.
                call rs_oimname (Memc[image], Memc[dirname], extstr,
		    Memc[fname], SZ_FNAME)
                if (strmatch (Memc[fname], ".pl$") == 0)
                    call strcat (".pl", Memc[fname], SZ_FNAME)


		# Record the file name.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
	    }
            call close (strfd)

	    # Create the final list.
            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            outlist = imtopen (Memc[otemplate])

        # Get the user output names.
        } else {

	    # Create a temporary list string.
            len_otemplate = imtlen (outlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    # Loop over the fields.
            do i = 1, imtlen (inlist) {

		# Get the output file name.
                if (imtrgetim (outlist, i, Memc[fname], SZ_FNAME) == EOF)
		    break
                if (strmatch (Memc[fname], ".pl$") == 0)
                    call strcat (".pl", Memc[fname], SZ_FNAME)

	        # Add the output name to the list.
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[fname])
            }
            call close (strfd)

            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            call imtclose (outlist)
            outlist = imtopen (Memc[otemplate])
        }

        call sfree (sp)

        return (outlist)
end


# RS_OUTNAME -- Construct an output file name. If output is null or a
# directory, a name is constructed from the root of the image name and the
# extension. The disk is searched to avoid name collisions.

procedure rs_outname (image, output, ext, name, maxch)

char    image[ARB]              #I input image name
char    output[ARB]             #I input output directory or name
char    ext[ARB]                #I input extension
char    name[ARB]               #O output file name
int     maxch                   #I maximum size of name

pointer sp, root, str
int     ndir, nimdir, clindex, clsize, nextn
int     fnldir(), strlen(), strldx(), strdic()
char	period 

begin
        call smark (sp)
        call salloc (root, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_FNAME, TY_CHAR)

        ndir = fnldir (output, name, maxch)
        if (strlen (output) == ndir) {
            call imparse (image, Memc[root], SZ_FNAME, Memc[str], SZ_FNAME,
                Memc[str], SZ_FNAME, clindex, clsize)
            nimdir = fnldir (Memc[root], Memc[str], SZ_FNAME)
	    period = '.'
	    nextn = strldx (period, Memc[root])
	    if (nextn > 0) {
		if (strdic (Memc[root+nextn], Memc[str], SZ_FNAME,
		    RS_EXTNLIST) > 0)
		    Memc[root+nextn-1] = EOS
	    }
            if (clindex >= 0) {
		if (ext[1] == EOS) {
                    call sprintf (name[ndir+1], maxch, "%s%d.*")
                        call pargstr (Memc[root+nimdir])
                        call pargi (clindex)
		} else {
                    call sprintf (name[ndir+1], maxch, "%s%d.%s.*")
                        call pargstr (Memc[root+nimdir])
                        call pargi (clindex)
                        call pargstr (ext)
		}
            } else {
		if (ext[1] == EOS) {
                    call sprintf (name[ndir+1], maxch, "%s.*")
                        call pargstr (Memc[root+nimdir])
		} else {
                    call sprintf (name[ndir+1], maxch, "%s.%s.*")
                        call pargstr (Memc[root+nimdir])
                        call pargstr (ext)
		}
            }
            call rs_oversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


# RS_OVERSION -- Compute the next available version number of a given file
# name template and output the new file name.

procedure rs_oversion (template, filename, maxch)

char    template[ARB]                   #I the input name template
char    filename[ARB]                   #O the output name
int     maxch                           #I the  maximum number of characters

char    period
int     newversion, version, len
pointer sp, list, name
int     fntgfnb() strldx(), ctoi(), fntopnb()
errchk  fntopnb()

begin
        # Allocate temporary space
        call smark (sp)
        call salloc (name, maxch, TY_CHAR)
        period = '.'
        iferr (list = fntopnb (template, NO))
            list = fntopnb ("", NO)

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


# RS_OIMNAME -- Construct an output image name. If output is null or a
# directory a name is constructed from the root of the image name and the
# extension. The disk is searched to avoid name collisions.

procedure rs_oimname (image, output, ext, name, maxch)

char    image[ARB]              #I the input image name
char    output[ARB]             #I the output directory or ouput image name
char    ext[ARB]                #I the output image extension
char    name[ARB]               #O the final output image name
int     maxch                   #I maximum size of name

int     ndir, nimdir, clindex, clsize
pointer sp, root, str
int     fnldir(), strlen()

begin
	# Allocate some temporary space.
        call smark (sp)
        call salloc (root, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_FNAME, TY_CHAR)

	# Determine the length of the directory spec.
        ndir = fnldir (output, name, maxch)

	# If the file spec is a directory create a name from the directory and
	# the route image name, otherwise use the output name directly.
        if (strlen (output) == ndir) {
            call imparse (image, Memc[root], SZ_FNAME, Memc[str], SZ_FNAME,
                Memc[str], SZ_FNAME, clindex, clsize)
            nimdir = fnldir (Memc[root], Memc[str], SZ_FNAME)
            if (clindex >= 0) {
		if (ext[1] == EOS) {
                    call sprintf (name[ndir+1], maxch, "%s%d.*")
                        call pargstr (Memc[root+nimdir])
                        call pargi (clindex)
		} else {
                    call sprintf (name[ndir+1], maxch, "%s%d.%s.*")
                        call pargstr (Memc[root+nimdir])
                        call pargi (clindex)
                        call pargstr (ext)
		}
            } else {
		if (ext[1] == EOS) {
                    call sprintf (name[ndir+1], maxch, "%s.*")
                        call pargstr (Memc[root+nimdir])
		} else {
                    call sprintf (name[ndir+1], maxch, "%s.%s.*")
                        call pargstr (Memc[root+nimdir])
                        call pargstr (ext)
		}
            }
            call rs_oimversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


# RS_OIMVERSION -- Determine the next available version number for a given
# image  name template and output the new image name.

procedure rs_oimversion (template, filename, maxch)

char    template[ARB]                   #I the image name template
char    filename[ARB]                   #O the output image name
int     maxch                           #I the maximum number of characters

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
