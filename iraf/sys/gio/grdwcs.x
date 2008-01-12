# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

.help savewcs
.nf __________________________________________________________________________
SAVEWCS -- A package for saving the WCS in a file for later restoration when
a device is opened in append mode.

	gwrwcs (devname, wcs, len_wcs)		save wcs in file
  len = grdwcs (devname, wcs, len_wcs)		read wcs from file

Only the 16+1 WCS structures are currently saved.  There is no provision for
saving the WCSSTATE and the index of the current WCS.
.endhelp _____________________________________________________________________


# GWRWCS -- Save the WCS in a binary file in the user directory UPARM.
# Any existing file is overwritten.

procedure gwrwcs (devname, wcs, len_wcs)

char	devname[ARB]			# device name
int	wcs[ARB]			# array to be saved
int	len_wcs

pointer	sp, fname
int	fd
int	open()
errchk	open, write

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call gwcs_mkfilename (devname, Memc[fname], SZ_FNAME)
	iferr (call delete (Memc[fname]))
	    ;
	fd = open (Memc[fname], NEW_FILE, BINARY_FILE)
	call write (fd, wcs, len_wcs * SZ_INT)
	call close (fd)

	call sfree (sp)
end


# GRDWCS -- Read the WCS from a binary file in the user directory UPARM.
# The actual number of size int elements read is returned as the function
# value.  It is not an error if there is no file or the file cannot be read.

int procedure grdwcs (devname, wcs, len_wcs)

char	devname[ARB]			# device name
int	wcs[ARB]			# array to be returned
int	len_wcs				# max ints to read

pointer	sp, fname
int	fd, nchars
int	open(), read()
errchk	read

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call gwcs_mkfilename (devname, Memc[fname], SZ_FNAME)
	iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE))
	    nchars = 0
	else {
	    nchars = read (fd, wcs, len_wcs * SZ_INT)
	    call close (fd)
	}

	call sfree (sp)
	return (nchars / SZ_INT)
end


# GWCS_MKFILENAME -- Make the filename of the WCS savefile for the named
# device.  The filename is "uparm$fname.gd", where the "fname" is the
# device name with any illegal filename characters deleted.  The mapping
# is not necessarily unique.

procedure gwcs_mkfilename (devname, fname, maxch)

char	devname[ARB]		# device name
char	fname[ARB]		# generated filename (output)
int	maxch

int	ip, op, ch
int	gstrcpy()

begin
	# Leave OP pointing to last char output.
	op = gstrcpy ("uparm$", fname, maxch)

	for (ip=1;  devname[ip] != EOS;  ip=ip+1) {
	    ch = devname[ip]
	    if (IS_ALNUM(ch) || ch == '.' || ch == '_') {
		op = min (maxch, op + 1)
		fname[op] = ch
	    }
	}

	fname[op+1] = EOS
end
