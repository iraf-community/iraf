# MASKNAME -- Make a mask name.  This creates a FITS mask extension if
# possible, otherwise it creates a pixel list file.  To create a FITS
# extension the filename must explicitly select the FITS kernel or the
# default image type must be a FITS file.  The input and output strings
# may be the same.

procedure xt_maskname (fname, extname, mode, mname, maxchar)

char	fname[ARB]			#I File name
char	extname[ARB]			#I Default pixel mask extension name
int	mode				#I Mode
char	mname[maxchar]			#O Output mask name
int	maxchar				#I Maximum characters in mask name

int	i, fits
pointer	sp, temp

bool	streq()
int	strmatch(), stridxs(), strldxs(), strncmp()
int	envfind(), access(), imaccess()

begin
	call smark (sp)
	call salloc (temp, maxchar, TY_CHAR)

	# Determine whether to use FITS pixel mask extensions.  One may set
	# fits=NO to force use of pl even when FITS mask extensions are
	# supported.
	fits = YES
	if (fits == YES && envfind ("masktype", Memc[temp], maxchar) > 0) {
	    if (streq (Memc[temp], "pl"))
		fits = NO
	}
	i = strldxs ("]", fname)

	# Check for explicit .pl extension.
	if (strmatch (fname, ".pl$") > 0)
	    call strcpy (fname, mname, maxchar)

	# Check for explicit mask extension.
	else if (strmatch (fname, "type=mask") > 0)
	    call strcpy (fname, mname, maxchar)
	else if (strmatch (fname, "type\\\=mask") > 0)
	    call strcpy (fname, mname, maxchar)

	# Add mask type.
	else if (i > 0) {
	    if (mode != READ_ONLY) {
		call strcpy (fname[i], Memc[temp], maxchar)
		call sprintf (mname[i], maxchar-i, ",type=mask%s")
		    call pargstr (Memc[temp])
	    }
	    i = stridxs ("[", mname)
	    if (i > 0) {
		call strcpy (mname[i+1], Memc[temp], SZ_FNAME)
		if (strncmp (mname[i+1], "append", 6)==0 ||
		    strncmp (mname[i+1], "inherit", 7)==0) {
		    call sprintf (mname[i+1], SZ_FNAME-i+1, "%s,%s")
			call pargstr (extname)
			call pargstr (Memc[temp])
		}
	    }

	# Create output from rootname name.
	} else if (fits == YES) {
	    call strcpy (fname, Memc[temp], SZ_FNAME)
	    if (mode == READ_ONLY) {
		call sprintf (mname, maxchar, "%s[%s]")
		    call pargstr (Memc[temp])
		    call pargstr (extname)
	    } else {
		call sprintf (mname, maxchar, "%s[%s,type=mask]")
		    call pargstr (Memc[temp])
		    call pargstr (extname)
	    }
	} else
	    call strcat (".pl", mname, maxchar)

	# Check if extension name is actually given.
	i = stridxs ("[", mname)
	if (i > 0) {
	    call strcpy (mname[i+1], Memc[temp], SZ_FNAME)
	    if (strncmp (mname[i+1], "append", 6)==0 ||
		strncmp (mname[i+1], "inherit", 7)==0) {
		call sprintf (mname[i+1], SZ_FNAME-i+1, "%s,%s")
		    call pargstr (extname)
		    call pargstr (Memc[temp])
	    }
	}
		
	# Convert to pl form if required.
	i = stridxs ("[", mname)
	if (i > 0 && mode == READ_ONLY)
	    fits = imaccess (mname, mode)
	if (fits == NO && i > 0) {
	    mname[i] = EOS
	    if (mode == NEW_IMAGE) {
		if (access (mname, 0, 0) == NO) {
		    ifnoerr (call fmkdir (mname))
			mname[i] =  '/'
		    else
			mname[i] = '.'
		} else
		    mname[i] =  '/'
	    } else {
		if (access (mname, 0, 0) == NO)
		    mname[i] = '.'
		else
		    mname[i] =  '/'
	    }
	        
	    if (strncmp (mname[i+1], "type", 4) == 0 ||
	        strncmp (mname[i+1], "append", 6) == 0 ||
	        strncmp (mname[i+1], "inherit", 7) == 0) {
		mname[i+1] = EOS
		call strcat (extname, mname, maxchar)
	    } else {
		i = stridxs (",]", mname)
		mname[i] = EOS
	    }
	    call strcat (".pl", mname, maxchar)
	}

	call sfree (sp)
end
