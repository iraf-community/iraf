# XT_MASKNAME -- Make a mask name.
# 
# This creates a FITS mask extension if possible, otherwise it creates a
# pixel list file.  To override this default the environment variable
# "masktype" needs to be set to "pl".  To create a FITS extension the
# filename must explicitly select the FITS kernel or the default image type
# must be a FITS file.  The input and output strings may be the same.
# This supports multiextension masks for pl format by using a subdirectory.

procedure xt_maskname (fname, extname, mode, mname, maxchar)

char	fname[ARB]			#I File name
char	extname[ARB]			#I Default pixel mask extension name
int	mode				#I Mode
char	mname[maxchar]			#O Output mask name
int	maxchar				#I Maximum characters in mask name

int	i, fits
pointer	sp, extnm, temp

bool	streq()
int	strmatch(), stridxs(), strldxs(), strncmp()
int	envfind(), access(), imaccess()

begin
	call smark (sp)
	call salloc (extnm, SZ_FNAME, TY_CHAR)
	call salloc (temp, maxchar, TY_CHAR)

	# Set extension name.
	if (extname[1] == EOS)
	    call strcpy ("pl", Memc[extnm], SZ_FNAME)
	else
	    call strcpy (extname, Memc[extnm], SZ_FNAME)

	# Determine whether to use FITS pixel mask extensions.
	if (envfind ("masktype", Memc[temp], maxchar) > 0) {
	    if (streq (Memc[temp], "pl"))
		fits = NO
	    else
		fits = YES
	} else
	    fits = YES
	i = strldxs ("]", fname)

	# Check for explicit .pl extension.
	if (strmatch (fname, ".pl$") > 0)
	    call strcpy (fname, mname, maxchar)

	# Check for explicit mask extension.
	else if (strmatch (fname, "type=mask") > 0)
	    call strcpy (fname, mname, maxchar)
	else if (strmatch (fname, "type\\\=mask") > 0)
	    call strcpy (fname, mname, maxchar)

	# Check for kernel section and add mask type.
	else if (i > 0) {
	    call strcpy (fname, mname, maxchar)
	    if (mode != READ_ONLY) {
		call strcpy (fname[i], Memc[temp], maxchar)
		call sprintf (mname[i], maxchar-i, ",type=mask%s")
		    call pargstr (Memc[temp])
	    }

	# Create output from rootname name.
	} else if (fits == YES) {
	    if (mode == READ_ONLY) {
		call sprintf (mname, maxchar, "%s[%s]")
		    call pargstr (fname)
		    call pargstr (Memc[extnm])
	    } else {
		call sprintf (mname, maxchar, "%s[%s,type=mask]")
		    call pargstr (fname)
		    call pargstr (Memc[extnm])
	    }
	} else if (extname[1] != EOS) {
	    call sprintf (mname, maxchar, "%s[%s]")
	        call pargstr (fname)
		call pargstr (Memc[extnm])
	} else {
	    call sprintf (mname, maxchar, "%s.pl")
	        call pargstr (fname)
	}

	# Convert extension references to pl form if required.
	# Extensions are implemented as directories.

	i = stridxs ("[", mname)
	if (i > 0 && mode == READ_ONLY)
	    fits = imaccess (mname, mode)
	if (fits == NO && i > 0) {
	    call strcpy (mname, Memc[temp], maxchar)
	    mname[i] = EOS
	    if (mode == NEW_IMAGE) {
		if (access (mname, 0, 0) == NO) {
		    ifnoerr (call fmkdir (mname))
			mname[i] =  '/'
		    else
			mname[i] = '_'
		} else
		    mname[i] =  '/'
	    } else {
		if (access (mname, 0, 0) == NO)
		    mname[i] = '_'
		else
		    mname[i] =  '/'
	    }
	        
	    if (strncmp (mname[i+1], "type", 4) == 0 ||
	        strncmp (mname[i+1], "append", 6) == 0 ||
	        strncmp (mname[i+1], "inherit", 7) == 0) {
		mname[i+1] = EOS
		call strcat (Memc[extnm], mname, maxchar)
	    } else {
		i = stridxs (",]", mname)
		mname[i] = EOS
	    }
	    call strcat (".pl", mname, maxchar)

	    if (mode == READ_ONLY && imaccess(mname,0)==NO)
	        call strcpy (Memc[temp], mname, maxchar)
	}

	call sfree (sp)
end
