include <pkg/mef.h>

# MEFOPEN --- Open a FITS extension, it can be the Primary or extension
#	      unit, file.fits[0] for the PU or file.fits[extn] for the
#	      Extension Unit.
#
#	filename.ext[abs#][extname,extver]
#
#	The absolute extension number (abs#) convention is zero for
#	the Primary Unit.
#


# MEF_OPEN -- Open a FITS Unit from a file and returns its characteristics.

pointer procedure mef_open (fitsfile, acmode, oldp)

char	fitsfile[ARB]	#I Input FITS filename
int	acmode		#I access mode
pointer	oldp		#I Old Fits pointer or header size

pointer	sp, ksec, section, mef
int	group, clsize, open()

begin
        call smark (sp)
	call salloc (ksec, LEN_CARD, TY_CHAR)
	call salloc (section, LEN_CARD, TY_CHAR)

	call calloc (mef, LEN_MEF, TY_STRUCT)

	MEF_ACMODE(mef) = acmode

	# Get filename components
	call imparse (fitsfile, MEF_FNAME(mef), SZ_FNAME, Memc[ksec],
		     LEN_CARD, Memc[section], LEN_CARD, group, clsize)

	# Check if file has an extension and exists.
	call mef_file_access (MEF_FNAME(mef), acmode)

	if (Memc[section] != EOS) 
	   call error(13, "mefopen: Image sections not allowed")

	MEF_FD(mef) = open (MEF_FNAME(mef), acmode, BINARY_FILE)
	MEF_ENUMBER(mef) = group
	MEF_CGROUP(mef) = -1
	MEF_KEEPXT(mef) = NO

	call sfree (sp)
	return(mef)
end


# MEF_FILE_ACCESS -- Check that file exists if READ* mode is given. Mainly we
# want to check if there is an extension 'fits'. If file was given with no
# extension, append .fits and see if exists.

procedure mef_file_access (fname, acmode)

char	fname[ARB]
int	acmode

pointer sp, fext, fn
int	len, fnextn(), access(), strncmp()
begin
	if (acmode == NEW_FILE || acmode == NEW_COPY)
	    return

	call smark (sp)
	call salloc (fext, SZ_FNAME, TY_CHAR)
	call salloc (fn, SZ_FNAME, TY_CHAR)

	call strcpy (fname, Memc[fn], SZ_FNAME)

	len = fnextn (Memc[fn], Memc[fext], SZ_FNAME)

	if (strncmp("fits", Memc[fext], 4) == 0)
	    return

	# See if file exists with no extension
	if (access(fname, 0, 0) == YES)
	    return
	else {
	    call strcat( ".fits", Memc[fn], SZ_FNAME)
	    if (access(Memc[fn], 0, 0) == YES) {
		call strcpy (Memc[fn], fname, SZ_FNAME)
		return
	    }
	}

	call sfree(sp)

end
