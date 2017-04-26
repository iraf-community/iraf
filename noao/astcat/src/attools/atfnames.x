include <pkg/cq.h>
include "../../lib/astrom.h"

# AT_SVLIST -- Create the input image survey list

int procedure at_svlist (surveys, imdb)

char	surveys[ARB]		#I the input image survey list
char	imdb[ARB]		#I the input image survey database file

pointer	sp, stemplate, svname, cq
int	i, svlist, len_stemplate, strfd, svno
pointer	cq_map()
int	fntopnb(), fntlenb(), fntrfnb(), stropen(), cq_setcat(), strlen()
int	cq_stati(), cq_locaten()
errchk	fntopnb()

begin
	iferr (svlist = fntopnb (surveys, NO))
            svlist = fntopnb ("", NO)
        if (surveys[1] == EOS)
            return (svlist)
	if (surveys[1] != '*' && fntlenb (svlist) <= 0)
            return (svlist)

	# Open the catalog database.
	cq = cq_map (imdb, READ_ONLY)
	if (cq == NULL) {
	    call fntclsb (svlist)
            svlist = fntopnb ("", NO)
	    return (svlist)
	}

	# Determine the length of the image survey list string.
	call smark (sp)
	call salloc (svname, SZ_FNAME, TY_CHAR)
	if (surveys[1] == '*')
            len_stemplate = cq_stati (cq, CQNRECS) * SZ_FNAME + 1
	else
            len_stemplate = fntlenb (svlist) * SZ_FNAME + 1
	call salloc (stemplate, len_stemplate, TY_CHAR)
	Memc[stemplate] = EOS

	# Loop through the surveys checking for appropriate entry in the
	# survey database
        strfd = stropen (Memc[stemplate], len_stemplate, NEW_FILE)
	if (surveys[1] == '*') {
	    do i = 1, cq_stati (cq, CQNRECS) {
		if (cq_locaten (cq, i, Memc[svname], SZ_FNAME) != i)
		    next
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[svname])
	    }
	} else {
	    do i = 1, fntlenb (svlist) {
	        if (fntrfnb (svlist, i, Memc[svname], SZ_FNAME) == EOF)
		    break
	        svno = cq_setcat (cq, Memc[svname])
	        if (svno <= 0)
		    next
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[svname])
	    }
	}
	call close (strfd)

	# Create the final catalog list.
        if (Memc[stemplate] != EOS)
            Memc[stemplate+strlen(Memc[stemplate])-1] = EOS
        call fntclsb (svlist)
        svlist = fntopnb (Memc[stemplate], NO)

	# Unmap the catalog database.
	call cq_unmap (cq)

	call sfree (sp)

	return (svlist)
end

define	SZ_HDRTEXT	(5 * SZ_LINE)

# AT_CATLIST -- Create the input catalog list

int procedure at_catlist (catalogs, catdb)

char	catalogs[ARB]		#I the input catalog list
char	catdb[ARB]		#I the input catalog database file

pointer	sp, ctemplate, catname, hdrtext, cq
int	i, catlist, len_ctemplate, strfd, catno, tmpfd
pointer	cq_map()
int	fntopnb(), fntlenb(), fntrfnb(), stropen(), cq_setcat(), strlen()
int	cq_stati(), cq_locaten(), access(), open(), at_gcathdr()
bool	streq()
errchk	fntopnb()

begin
	iferr (catlist = fntopnb (catalogs, NO))
            catlist = fntopnb ("", NO)
        if (catalogs[1] == EOS)
            return (catlist)
	if (catalogs[1] != '*' && fntlenb (catlist) <= 0)
            return (catlist)

	# Open the catalog database.
	cq = cq_map (catdb, READ_ONLY)
	if (cq == NULL) {
	    call fntclsb (catlist)
            catlist = fntopnb ("", NO)
	    return (catlist)
	}

	# Determine the length of the catalog list string.
	call smark (sp)
	call salloc (catname, SZ_FNAME, TY_CHAR)
	if (catalogs[1] == '*')
	    len_ctemplate = cq_stati (cq, CQNRECS) * SZ_FNAME + 1
	else
            len_ctemplate = fntlenb (catlist) * SZ_FNAME + 1
	call salloc (ctemplate, len_ctemplate, TY_CHAR)
	Memc[ctemplate] = EOS
	call salloc (hdrtext, SZ_HDRTEXT, TY_CHAR)

	# Loop through the catalogs checking for appropriate entry in the
	# catalog database
        strfd = stropen (Memc[ctemplate], len_ctemplate, NEW_FILE)
	if (catalogs[1] == '*') {
	    do i = 1, cq_stati (cq, CQNRECS) {
		if (cq_locaten (cq, i, Memc[catname], SZ_FNAME) != i)
		    next
		if (streq (Memc[catname], "filename@noao"))
		    next
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[catname])
	    }
	} else {
	    do i = 1, fntlenb (catlist) {
	        if (fntrfnb (catlist, i, Memc[catname], SZ_FNAME) == EOF)
		    break
	        catno = cq_setcat (cq, Memc[catname])
	        if (catno <= 0) {
		    if (access (Memc[catname], READ_ONLY, TEXT_FILE) == NO)
		        next
		    tmpfd = open (Memc[catname], READ_ONLY, TEXT_FILE)
		    if (at_gcathdr (tmpfd, Memc[hdrtext], SZ_HDRTEXT) <= 0)
			next
		    call close (tmpfd)
		}
                call fprintf (strfd, "%s,")
                    call pargstr (Memc[catname])
	    }
	}
	call close (strfd)

	# Create the final catalog list.
        if (Memc[ctemplate] != EOS)
            Memc[ctemplate+strlen(Memc[ctemplate])-1] = EOS
        call fntclsb (catlist)
        catlist = fntopnb (Memc[ctemplate], NO)

	# Unmap the catalog database.
	call cq_unmap (cq)

	call sfree (sp)

	return (catlist)
end


# AT_OSVLIST -- Create a list output images using the input surveys list,
# the field center list stored in the field center symbol table,  and an input
# template string.

int procedure at_osvlist (at, svlist, output, defaultstr, extstr, append)

pointer at                      #I the astrometry package descriptor
int	svlist			#I the input surveys list descriptor
char    output[ARB] 	        #I the input output file list
char    defaultstr[ARB]         #I the defaults id string
char    extstr[ARB]             #I the extension string
int     append                  #I test for existence of file ?

pointer	sp, dirname, fname, fcname, symlist, symbol, otemplate, st
int	i, j, imlist, len_dir, len_otemplate, strfd
pointer	sthead(), stnext(), at_statp()
int	imtopen(), fntlenb(), stnsymbols(), fnldir(), strncmp()
int	strlen(), stropen(), access(), imtlen(), imtrgetim()
errchk	imtopen()

begin
        # Return if the input file list is empty.
	call at_sets (at, OUTPUT, "")
	if (at_statp(at, PRCENTER) == NULL) {
            imlist = imtopen ("")
	} else if (at_statp(at, RCST)  == NULL) {
            imlist = imtopen ("")
        } else {
	    iferr (imlist = imtopen (output))
                imlist = imtopen ("")
	}
        if (output[1] == EOS || imtlen (imlist) <= 0)
            return (imlist)

	# Get the symbol table descriptor.
	st = at_statp(at,RCST)

        # Return if the output file list is the wrong length.
        if ((imtlen (imlist) > 1) && (imtlen (imlist) != fntlenb(svlist) *
	    stnsymbols(st, 0))) {
            call imtclose (imlist)
            imlist = imtopen ("")
            return (imlist)
        }

	# Get working space
        call smark (sp)
        call salloc (dirname, SZ_FNAME, TY_CHAR)
        call salloc (fname, SZ_FNAME, TY_CHAR)
        call salloc (fcname, SZ_FNAME, TY_CHAR)
        call salloc (symlist, stnsymbols(st,0), TY_INT)

        # Get the symbol list. Note that it is in reverse order.
        symbol = sthead (st)
        do i = 1, stnsymbols (st, 0) {
            Memi[symlist+i-1] = symbol
            symbol = stnext (st, symbol)
        }

        # Get the directory name.
        if (imtrgetim (imlist, 1, Memc[fname], SZ_FNAME) == EOF)
            Memc[fname] = EOS
        len_dir = fnldir (Memc[fname], Memc[dirname], SZ_FNAME)

        # Get the default output file names. There will be one output file per
        # input image.
        if (strncmp (defaultstr, Memc[fname+len_dir],
            strlen (defaultstr)) == 0 || len_dir == strlen (Memc[fname])) {

	    # Creata temporary list string.
            call imtclose (imlist)
            len_otemplate = fntlenb (svlist) * stnsymbols (st, 0) *
	        SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

	    # Loop over the catalog list.
            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do j = 1, fntlenb (svlist) {

		# Loop over the symbol table.
                do i = stnsymbols (st, 0), 1, -1 {

		    symbol = Memi[symlist+i-1]
		    if (strncmp (AT_RCSTSOURCE(symbol), "image", 5) == 0) {
			if (fntlenb (svlist) > 1) {
		            call sprintf (Memc[fcname], SZ_FNAME, "%s.%03d")
			        call pargstr (AT_RCSTNAME(symbol))
			        call pargi (j)
                            call at_oimname (Memc[fcname], Memc[dirname],
		                extstr, Memc[dirname], SZ_FNAME)
			} else
                            call at_oimname (AT_RCSTNAME(symbol), Memc[dirname],
		                extstr, Memc[dirname], SZ_FNAME)
		    } else {
			if (fntlenb (svlist) > 1) {
			    call sprintf (Memc[fcname], SZ_FNAME,
			        "reg%03d.%03d")
			        call pargi (stnsymbols(st, 0) - i + 1)
				call pargi (j)
			} else {
			    call sprintf (Memc[fcname], SZ_FNAME, "reg%03d")
			        call pargi (stnsymbols(st, 0) - i + 1)
			}
                        call at_oimname (Memc[fcname], Memc[dirname], extstr,
		            Memc[dirname], SZ_FNAME)
		    }

		    # Record the file name.
                    call fprintf (strfd, "%s,")
                        call pargstr (Memc[dirname])

                }
	    }
            call close (strfd)

	    # Create the final list.
            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            imlist = imtopen (Memc[otemplate])

        # Get the user output names.
        } else {

	    # Create a temporary list string.
            len_otemplate = imtlen (imlist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do j = 1, fntlenb (svlist) {

		# Loop over the fields.
                do i = 1, imtlen (imlist) {

		    # Get the output file name.
                    if (imtrgetim (imlist, i, Memc[fname], SZ_FNAME) == EOF)
                        break

		    # Provide a default name if necessary.
                    if (append == NO && access (Memc[fname], 0, 0) == YES) {
		        symbol = Memi[symlist+stnsymbols(st,0)-i]
		        if (strncmp (AT_RCSTSOURCE(symbol), "image", 5) == 0) {
			    if (stnsymbols (st,0) > 1) {
		                call sprintf (Memc[fcname], SZ_FNAME, "%s.%03d")
			            call pargstr (AT_RCSTNAME(symbol))
			            call pargi (j)
                                call at_oimname (Memc[fcname], Memc[dirname],
				    extstr, Memc[fname], SZ_FNAME)
			    } else
                                call at_oimname (AT_RCSTNAME(symbol),
				    Memc[dirname], extstr, Memc[fname],
				    SZ_FNAME)
		        } else {
			    if (fntlenb (svlist) > 1) {
			        call sprintf (Memc[fcname], SZ_FNAME,
				    "reg%03d.%03d")
			            call pargi (stnsymbols(st, 0) - i + 1)
				    call pargstr (j)
			    } else {
			        call sprintf (Memc[fcname], SZ_FNAME,
				    "reg%03d")
			            call pargi (stnsymbols(st, 0) - i + 1)
			    }
                            call at_oimname (Memc[fcname], Memc[dirname],
			        extstr, Memc[fname], SZ_FNAME)
		        }
                    }

		    # Add the file name to the list.
                    call fprintf (strfd, "%s,")
                        call pargstr (Memc[fname])
                }
	    }
            call close (strfd)

            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            call imtclose (imlist)
            imlist = imtopen (Memc[otemplate])
        }
	call at_sets (at, OUTPUT, output)

        call sfree (sp)

        return (imlist)
end


# AT_OCATLIST -- Create a list output files list using the input catalog list,
# the field center list stored in the field center symbol table,  and an input
# file template string.

int procedure at_ocatlist (at, catlist, output, defaultstr, extstr, append)

pointer at                      #I the astrometry package descriptor
int	catlist			#I the input catalog list descriptor
char    output[ARB] 	        #I the input output file list
char    defaultstr[ARB]         #I the defaults id string
char    extstr[ARB]             #I the extension string
int     append                  #I test for existence of file ?

pointer	sp, dirname, fname, fcname, symlist, symbol, otemplate, st
int	i, j, olist, len_dir, len_otemplate, strfd
pointer	sthead(), stnext(), at_statp()
int	fntopnb(), fntlenb(), stnsymbols(), fntrfnb(), fnldir(), strncmp()
int	strlen(), stropen(), access()
errchk	fntopnb()

begin
        # Return if the input file list is empty.
	if (at_statp(at, PRCENTER) == NULL) {
            olist = fntopnb ("", NO)
	} else if (at_statp(at, RCST)  == NULL) {
            olist = fntopnb ("", NO)
        } else {
	    iferr (olist = fntopnb (output, NO))
                olist = fntopnb ("", NO)
	}
        if (output[1] == EOS || fntlenb (olist) <= 0)
            return (olist)

	# Get the symbol table descriptor.
	st = at_statp(at,RCST)

        # Return if the output file list is the wrong length.
        if ((fntlenb (olist) > 1) && (fntlenb (olist) != fntlenb(catlist) *
	    stnsymbols(st, 0))) {
            call fntclsb (olist)
            olist = fntopnb ("", NO)
            return (olist)
        }

	# Get working space
        call smark (sp)
        call salloc (dirname, SZ_FNAME, TY_CHAR)
        call salloc (fname, SZ_FNAME, TY_CHAR)
        call salloc (fcname, SZ_FNAME, TY_CHAR)
        call salloc (symlist, stnsymbols(st,0), TY_INT)

        # Get the symbol list. Note that it is in reverse order.
        symbol = sthead (st)
        do i = 1, stnsymbols (st, 0) {
            Memi[symlist+i-1] = symbol
            symbol = stnext (st, symbol)
        }

        # Get the directory name.
        if (fntrfnb (olist, 1, Memc[fname], SZ_FNAME) == EOF)
            Memc[fname] = EOS
        len_dir = fnldir (Memc[fname], Memc[dirname], SZ_FNAME)

        # Get the default output file names. There will be one output file per
        # input image.
        if (strncmp (defaultstr, Memc[fname+len_dir],
            strlen (defaultstr)) == 0 || len_dir == strlen (Memc[fname])) {

	    # Creata temporary list string.
            call fntclsb (olist)
            len_otemplate = fntlenb (catlist) * stnsymbols (st, 0) *
	        SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

	    # Loop over the catalog list.
            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do j = 1, fntlenb (catlist) {

		# Loop over the symbol table.
                do i = stnsymbols (st, 0), 1, -1 {

		    symbol = Memi[symlist+i-1]
		    if (strncmp (AT_RCSTSOURCE(symbol), "image", 5) == 0) {
			if (fntlenb (catlist) > 1) {
		            call sprintf (Memc[fcname], SZ_FNAME, "%s.%03d")
			        call pargstr (AT_RCSTNAME(symbol))
				call pargi (j)
                            call at_outname (Memc[fcname], Memc[dirname],
		                extstr, Memc[dirname], SZ_FNAME)
			} else
                            call at_outname (AT_RCSTNAME(symbol), Memc[dirname],
		                extstr, Memc[dirname], SZ_FNAME)
		    } else {
			if (fntlenb (catlist) > 1) {
		            call sprintf (Memc[fcname], SZ_FNAME,
			        "reg%03d.%03d")
			        call pargi (stnsymbols(st, 0) - i + 1)
				call pargi (j)
			} else {
		            call sprintf (Memc[fcname], SZ_FNAME, "reg%03d")
			        call pargi (stnsymbols(st, 0) - i + 1)
			}
                        call at_outname (Memc[fcname], Memc[dirname], extstr,
		            Memc[dirname], SZ_FNAME)
		    }

		    # Record the file name.
                    call fprintf (strfd, "%s,")
                        call pargstr (Memc[dirname])

                }
	    }
            call close (strfd)

	    # Create the final list.
            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            olist = fntopnb (Memc[otemplate], NO)

        # Get the user output names.
        } else {

	    # Create a temporary list string.
            len_otemplate = fntlenb (olist) * SZ_FNAME + 1
            call salloc (otemplate, len_otemplate, TY_CHAR)
            Memc[otemplate] = EOS

            strfd = stropen (Memc[otemplate], len_otemplate, NEW_FILE)
	    do j = 1, fntlenb (catlist) {

		# Loop over the fields.
                do i = 1, fntlenb (olist) {

		    # Get the output file name.
                    if (fntrfnb (olist, i, Memc[fname], SZ_FNAME) == EOF)
                        break

		    # Provide a default name if necessary.
                    if (append == NO && access (Memc[fname], 0, 0) == YES) {
		        symbol = Memi[symlist+stnsymbols(st,0)-i]
		        if (strncmp (AT_RCSTSOURCE(symbol), "image", 5) == 0) {
			    if (fntlenb(catlist) > 1) {
		                call sprintf (Memc[fcname], SZ_FNAME, "%s.%03d")
			            call pargstr (AT_RCSTNAME(symbol))
			            call pargi (j)
                                call at_outname (Memc[fcname], Memc[dirname],
				    extstr, Memc[fname], SZ_FNAME)
			    } else
                                call at_outname (AT_RCSTNAME(symbol),
				    Memc[dirname], extstr, Memc[fname],
				    SZ_FNAME)
		        } else {
			    if (fntlenb (catlist) > 1) {
		                call sprintf (Memc[fcname], SZ_FNAME,
				    "reg%03d.%03d")
			            call pargi (stnsymbols(st, 0) - i + 1)
				    call pargi (j)
			    } else {
		                call sprintf (Memc[fcname], SZ_FNAME,
				    "reg%03d")
			            call pargi (stnsymbols(st, 0) - i + 1)
			    }
                            call at_outname (Memc[fcname], Memc[dirname],
			        extstr, Memc[dirname], SZ_FNAME)
		        }
                    }

		    # Add the file name to the list.
                    call fprintf (strfd, "%s,")
                        call pargstr (Memc[fname])
                }
	    }
            call close (strfd)

            if (Memc[otemplate] != EOS)
                Memc[otemplate+strlen(Memc[otemplate])-1] = EOS
            call fntclsb (olist)
            olist = fntopnb (Memc[otemplate], NO)
        }

        call sfree (sp)

        return (olist)
end


# AT_OUTNAME -- Construct an astrom output file name.
# If output is null or a directory, a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.

procedure at_outname (image, output, ext, name, maxch)

char    image[ARB]              #I input image name
char    output[ARB]             #I input output directory or name
char    ext[ARB]                #I input extension
char    name[ARB]               #O output file name
int     maxch                   #I maximum size of name

int     ndir, nimdir, clindex, clsize
pointer sp, root, str
int     fnldir(), strlen(),

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
            call at_oversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


# AT_OVERSION -- Compute the next available version number of a given file
# name template and output the new file name.

procedure at_oversion (template, filename, maxch)

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


# AT_OIMNAME -- Construct an output image name. If output is null or a
# directory a name is constructed from the root of the image name and the
# extension. The disk is searched to avoid name collisions.

procedure at_oimname (image, output, ext, name, maxch)

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
            call at_oimversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


# AT_OIMVERSION -- Determine the next available version number of for a given
# image  name template and output the new image name.

procedure at_oimversion (template, filename, maxch)

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
