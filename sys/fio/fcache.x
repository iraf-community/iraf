# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<mach.h>
include	<finfo.h>
include	<diropen.h>
include	<fset.h>
include	<knet.h>


#  FCACHE -- Simple file caching interface.  Our purpose is to take as
#  input a URL string and return a unique name for a local disk file.
#  The format of the name is of the form
#
#	cache$urlXXXXXX[.extn]
#
#  where 'cache' is a logical directory for the stored file, and the XXXXXXX
#  is the computed 32-bit checksum of the input URL string.  To provide a 
#  backward mapping of the filename in the cache to the original URL, a
#  file of the same name prefixed with a "." (e.g. "cache$.urlXXXX") will be
#  created containing the URL string.  This file may also be checked to 
#  avoid collisions of names in rare cases where multiple checksums may be
#  the same for different URLs.  
#
#  The 'cache' environment variables is used to define the location of the
#  cache directory.  When creating a cache filename, the caller may choose 
#  to append an extension to the file if it determines this is appropriate 
#  for the type of file accessed.  In this case, the dot-file will remain as
#  just the root part of the name.
#
#  This interface is intentionally simple so that it may be shared with host
#  applications that also require a cache.
#
#
#	fcinit  (cache, pattern)
#      fcpurge  (cache, verbose, age)
#    fcdestroy  (cache, verbose)
#
#	fclist  (cache, verbose, fd)
#     fclookup  (cache, src, cfname, extn, maxch)
#
#     fcaccess  (cache, inname, extn)
#        fcadd  (cache, inname, cfname, maxch)
#     fcdelete  (cache, fname)
#       fcwait  (cache, fname, timeout)
#
#	fcname  (cache, in, root, out, maxch)
#        fcsrc  (cache, in, out, maxch)



# FCINIT -- Initialize the file cache, i.e. delete all contents that contain
# the pattern substring (or all files if no pattern is specified).

procedure fcinit (cache, pattern)

char	cache[ARB]				#i cache dir to initialize
char	pattern[ARB]				#i filename substring pattern

int	dir, len
char	cfname[SZ_PATHNAME], fname[SZ_LINE], dirname[SZ_PATHNAME]
char	patbuf[SZ_LINE]

int	access(), strlen(), diropen(), isdirectory(), getline()
int	patmatch(), patmake()
errchk	delete()

begin
	# Simply create the directory if it doesn't exist.
	if (access (cache, 0, 0) == NO) {
	    call fmkdir (cache)
	    return

	} else if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	    call syserr (SYS_FOPENDIR)


	if (patmake (pattern, patbuf, SZ_LINE) == ERR)
	    call error (1, "Pattern is too complex")


	# Otherwise, read through the directory and remove the contents.
	dir = diropen (dirname, PASS_HIDDEN_FILES)

	while (getline (dir, fname) != EOF) {
	    len = strlen (fname)
	    fname[len] = '\0'

	    call sprintf (cfname, SZ_PATHNAME, "%s/%s")
		call pargstr (cache)
		call pargstr (fname)

	    # We only delete plain files, skip directories.
	    if (isdirectory (cfname, dirname, SZ_PATHNAME) > 0)
		next

	    if (patmatch (fname, patbuf) > 0) {
	        iferr (call delete (cfname))	# delete the file, ignore errors
		    ;
	    }
	}

	call close (dir)			# clean up
end


# FCPURGE -- Clean out a cache of file older than the given age.

procedure fcpurge (cache, verbose, age)

char	cache[ARB]				#i cache dir to initialize
bool	verbose					#i print verbose output?
int	age					#i age (in days)

int	dir, len
long	info[LEN_FINFO], old
char	cfname[SZ_FNAME], fname[SZ_LINE], dirname[SZ_PATHNAME]

int	access(), strlen(), diropen(), isdirectory(), getline(), finfo()
long	clktime()

begin
	# Simply return if it doesn't exist.
	if (access (cache, 0, 0) == NO)
	    return

	if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	    call syserr (SYS_FOPENDIR)

	# Otherwise, read through the directory and delete old files.
	dir = diropen (dirname, SKIP_HIDDEN_FILES)

	old = (clktime (0) - age * 86400)	# expiration time
	while (getline (dir, fname) != EOF) {
	    len = strlen (fname)
	    fname[len] = '\0'

	    call sprintf (cfname, SZ_PATHNAME, "%s/%s")
		call pargstr (cache)
		call pargstr (fname)

	    # Skip directories.
	    if (isdirectory (cfname, dirname, SZ_FNAME) > 0)
		next

	    if (finfo (cfname, info) == ERR)
		next

	    if (FI_CTIME(info) < old) {
		if (verbose) {
		    call eprintf ("Purging '%s'\n")
			call pargstr (fname)
		}
	        call fcdelete (cache, fname)	# delete the file
	    }
	}

	call close (dir)			# clean up
end


# FCDESTROY -- Destroy the named cache directory.

procedure fcdestroy (cache, verbose)

char	cache[ARB]				#i cache dir to initialize
bool	verbose					#i print verbose output

int	dir, len
char	cfname[SZ_FNAME], fname[SZ_LINE], dirname[SZ_PATHNAME]

int	access(), strlen(), diropen(), isdirectory(), getline()

begin
	# Simply return if it doesn't exist.
	if (access (cache, 0, 0) == NO)
	    return 

	if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	    call syserr (SYS_FOPENDIR)

	# Otherwise, read through the directory and delete old files.
	dir = diropen (dirname, PASS_HIDDEN_FILES)

	while (getline (dir, fname) != EOF) {
	    len = strlen (fname)
	    fname[len] = '\0'

	    call sprintf (cfname, SZ_PATHNAME, "%s/%s")
		call pargstr (cache)
		call pargstr (fname)

	    # Skip directories.
	    if (isdirectory (cfname, dirname, SZ_FNAME) > 0 || fname[1] != '.')
		next

	    if (verbose) {
		call eprintf ("Purging '%s'\n")
		    call pargstr (fname[2])
	    }
	    call fcdelete (cache, fname[2])	# delete the file
	}
	call close (dir)			# clean up

	call frmdir (cache)			# delete the cache directory
end


# FCLIST -- List info about the cache to the given descriptor.

procedure fclist (cache, verbose, fd)

char	cache[ARB]				#i cache dir to initialize
bool	verbose					#i verbose output
int	fd					#i output file descriptor

int	dir, len, age
char	cfname[SZ_FNAME], fname[SZ_LINE], dirname[SZ_PATHNAME]
char	src[SZ_LINE], date[SZ_LINE]
long	file_info[LEN_FINFO]

int	access(), strlen(), diropen(), isdirectory(), getline(), finfo()
bool 	streq()
long	clktime()

begin
	# Simply return if it doesn't exist.
	if (access (cache, 0, 0) == NO) {
	    call fmkdir (cache)
	    return
	} else if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	    call syserr (SYS_FOPENDIR)

	# Otherwise, read through the directory and remove the contents.
	dir = diropen (dirname, PASS_HIDDEN_FILES)

	while (getline (dir, fname) != EOF) {
	    len = strlen (fname)
	    fname[len] = '\0'
	    
	    # We only delete plain files, skip directories.
	    if (streq (fname, ".") || streq (fname, "..") || fname[1] != '.')
		next

	    ifnoerr (call fcsrc (cache, fname, src, SZ_LINE)) {
		call sprintf (cfname, SZ_FNAME, "%s/%s")
		    call pargstr (cache)
		    call pargstr (fname)
		if (finfo (cfname, file_info) != ERR) 
		    call cnvdate (FI_CTIME(file_info), date, SZ_LINE)
		else
		    call strcpy (" ", date, SZ_LINE)
		age = (clktime(0) - FI_CTIME(file_info) + 86400) / 86400

		if (verbose) {
	            call fprintf (fd, "%16s  %s  %s\n")
		        call pargstr (fname[2])
		        call pargstr (date)
		        call pargstr (src)
		} else {
	            call fprintf (fd, "%16s  %d  %s\n")
		        call pargstr (fname[2])
		        call pargi (age)
		        call pargstr (src)
		}
	    }
	}
end


# FCLOOKUP -- Lookup the src string and return the cached filename. If a
# filename in the cache is specified, return the src string.  Both strings
# must be at least 'maxch' chars long.

procedure fclookup (cache, src, cfname, extn, maxch)

char	cache[ARB]				#i cache dir to initialize
char	src[ARB]				#i lookup string
char	cfname[ARB]				#i cached filename
char	extn[ARB]				#i filename extension
int	maxch					#i output file descriptor

int	dir, len
char	dirname[SZ_PATHNAME], fname[SZ_LINE], csrc[SZ_LINE]

int	diropen(), getline(), strlen(), isdirectory(), access ()
bool	streq()

begin
	if (access (cache, 0, 0) == YES) {
	    if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	        call syserr (SYS_FOPENDIR)
	} else {
	    call aclrc (cfname, SZ_FNAME)
	    return
	}

	if (src[1] != EOS) {
	    call fcname (cache, src, "f", cfname, maxch)

	    dir = diropen (dirname, PASS_HIDDEN_FILES)
	    while (getline (dir, fname) != EOF) {
	        len = strlen (fname)
	        fname[len] = '\0'

	        # We only delete plain files, skip directories.
	        if (streq(fname, ".") || streq(fname, "..") || fname[1] != '.')
		    next

	        ifnoerr (call fcsrc (cache, fname, csrc, SZ_LINE)) {
		    if (streq (src, csrc)) {
		        call strcpy (fname[2], cfname, maxch)
	    		call close (dir)

			# Look for the extension.
			call fc_find_extn (cache, cfname, extn, maxch)
			return
		    }
	        }
	    }
	    call close (dir)

	} else if (cfname[1] != EOS)
	    call fcsrc (cache, cfname, src, SZ_LINE)
end


# FCACCESS -- See if a file is already in the cache.  For best results, the 
# input file name should include the full directory path name.

bool procedure fcaccess (cache, inname, extn)

char	cache[ARB]				#i cache dir to initialize
char	inname[ARB]				#i input file name
char	extn[ARB]				#i file extension

char	cname[SZ_PATHNAME], root[SZ_PATHNAME]
char	extfile[SZ_PATHNAME], lext[SZ_PATHNAME]
bool	stat

int	access(), fnroot()

begin
	# No cache, no file....
	if (access (cache, 0, 0) == NO)
	    return (FALSE)

	# Get the cache filename.
	call fcname (cache, inname, "f", cname, SZ_PATHNAME)
	call aclrc (lext, SZ_PATHNAME)
	if (extn[1] != EOS) {
	    if (fnroot (cname, root, SZ_PATHNAME) > 0)
	        call fc_find_extn (cache, root, lext, SZ_PATHNAME)
	}

	call sprintf (extfile, SZ_PATHNAME, "%s.%s")
	    call pargstr (cname)
	    call pargstr (lext)

        stat = (access (cname, 0, 0) == YES || access (extfile, 0, 0) == YES)
        return (stat)
end


# FCADD -- Add a new file to the cache.  This is a wrapper around copying
# the file and returning the cached name.  For best results, the input file
# name should include the full directory path name.

procedure fcadd (cache, inname, extn, cname, maxch)

char	cache[ARB]				#i cache dir to initialize
char	inname[ARB]				#i input file name
char	extn[ARB]				#i file extension
char	cname[ARB]				#o cached filename
int	maxch					#i size of output file name

char	fname[SZ_PATHNAME], imname[SZ_PATHNAME]
char	dotfile[SZ_PATHNAME]
int	retcode, status

int	access(), strncmp(), url_get(), url_errcode(), fnroot()
bool	fcaccess()

errchk	url_get(), delete()

begin
	if (access (cache, 0, 0) == NO)
	    call fmkdir (cache)

	# Get the cache filename.
	call fcname (cache, inname, "f", cname, maxch)

	if (access (cname, 0, 0) == YES) {
#	    iferr (call delete (cname))		# delete the file, ignore errors
#	        ;
	    return
	}


	if (extn[1] != EOS) {
	    call sprintf (fname, SZ_PATHNAME, "%s.%s")
	        call pargstr (cname)
	        call pargstr (extn)
	    if (access (fname, 0, 0) == YES) {
	        iferr (call delete (fname))	# delete the file, ignore errors
	            ;
	    }
	} else
	    call strcpy (cname, fname, SZ_PATHNAME)


	if (strncmp ("http://", inname, 7) == 0) {
	    if (! fcaccess (cache, inname, "")) {
	        retcode = url_get (inname, fname, NULL)
		if (retcode < 0) {
		    status = fnroot (cname, fname, maxch)
		    call sprintf (dotfile, SZ_PATHNAME, "%s/.%s")
			call pargstr (cache)
			call pargstr (fname)
	            call delete (dotfile)	# delete the dot file
		    call filerr (inname, url_errcode (-retcode))
		    return
		}

	        # Create a symlink to the file that can be used as an image ref.
	        call sprintf (imname, SZ_PATHNAME, "%s.fits")
	            call pargstr (fname)
	        call fsymlink (imname, fname)
	    }

	    if (extn[1] != EOS)
		call strcpy (fname, cname, maxch)

	} else if (strncmp ("file://localhost", inname, 16) == 0) {
	    iferr (call fcopy (inname[16], fname))
	        call syserr (SYS_FMKCOPY)
	    call strcpy (fname, cname, maxch)

	} else if (strncmp ("file://localhost", inname, 17) == 0) {
	    iferr (call fcopy (inname[18], fname))
	        call syserr (SYS_FMKCOPY)
	    call strcpy (fname, cname, maxch)

	} else if (strncmp ("file://", inname, 7) == 0) {
	    iferr (call fcopy (inname[7], fname))
	        call syserr (SYS_FMKCOPY)
	    call strcpy (fname, cname, maxch)

	} else {
	    iferr (call fcopy (inname, fname))
	        call syserr (SYS_FMKCOPY)
	    call strcpy (fname, cname, maxch)
	}
end


# FCDELETE -- Delete a named file from the cache.

procedure fcdelete (cache, fname)

char	cache[ARB]				#i cache dir to initialize
char	fname[ARB]				#i cache filename to delete

int	dir, len
char	cfname[SZ_FNAME], dfname[SZ_LINE], dotfile[SZ_FNAME]
char	dirname[SZ_FNAME]

int	access(), diropen(), strlen(), strsearch(), getline(), isdirectory()

errchk	delete()

begin
	if (access (cache, 0, 0) == NO) {
	    return
	} else if (isdirectory (cache, dirname, SZ_PATHNAME) == 0)
	    call syserr (SYS_FOPENDIR)


	call sprintf (cfname, SZ_FNAME, "%s%s")
	    call pargstr (cache)
	    call pargstr (fname)
	call sprintf (dotfile, SZ_FNAME, "%s.%s")
	    call pargstr (cache)
	    call pargstr (fname)


	if (access (cfname, 0, 0) == YES) {
	    iferr (call delete (cfname))
		;
	}
	if (access (dotfile, 0, 0) == YES) {
	    iferr (call delete (dotfile))
		;
	}

	# Loop through any other files in the directory that begin with
	# the requested file.  This removes the links created that may
	# contain file-type specific extensions.

	dir = diropen (dirname, SKIP_HIDDEN_FILES)
	while (getline (dir, dfname) != EOF) {
	    len = strlen (dfname)
	    dfname[len] = '\0'

	    call sprintf (cfname, SZ_FNAME, "%s/%s")
	        call pargstr (cache)
	        call pargstr (dfname)

	    if (strsearch (dfname, fname) > 0) {
	    	iferr (call deletefg (cfname, YES, YES))
		    call funlink (cfname)
	    }
	}
	call close (dir)
end


# FCWAIT -- Wait for the named file to appear in the cache.

int procedure fcwait (cache, fname)

char	cache[ARB]				#i cache dir to initialize
char	fname[ARB]				#i cache filename to wait for

char	cfname[SZ_FNAME], errfile[SZ_FNAME], lockfile[SZ_FNAME]
char	root[SZ_FNAME], extn[SZ_FNAME]

int	access(), fnroot(), fnextn()

begin
	if (access (cache, 0, 0) == NO)
	    call fmkdir (cache)

	if (fnroot (fname, root, SZ_FNAME) == 0)
	    return
	if (fnextn (fname, extn, SZ_FNAME) == 0)
	    ;

	call sprintf (cfname, SZ_FNAME, "%s%s")
	    call pargstr (cache)
	    call pargstr (root)
	if (extn[1] != EOS) {
	    call strcat (".", cfname, SZ_PATHNAME)
	    call strcat (extn, cfname, SZ_PATHNAME)
	}
	call sprintf (errfile, SZ_FNAME, "%s.%s.ERR")
	    call pargstr (cache)
	    call pargstr (root)
	call sprintf (lockfile, SZ_FNAME, "%s.%s.LOCK")
	    call pargstr (cache)
	    call pargstr (root)


	# Even if we've asked to pre-fetch the data, we want to avoid 
	# having to do any process synchronization with the threads
	# downloading the data.  So, block until the requested file is
	# available, or we get a file with a ".ERR" extension that 
	# indicates an error.

	if (access (cfname, 0, 0) == NO || access (lockfile,0,0) == YES) {
	    while (access (cfname,0,0) == NO || access (lockfile,0,0) == YES) {
		if (access (errfile, 0, 0) == YES)
		    return (0)
		call tsleep (1)
	    }
	}
	return (access (cfname, 0, 0))
end


# FCNAME -- Convert an input filename/string/url/whatever to a unique
# filename in the cache.  

procedure fcname (cache, in, root, out, maxch)

char	cache[ARB]				#i cache directory
char	in[ARB]					#i input string/name
char	root[ARB]				#i cache name root
char	out[ARB]				#o output cache filename
int	maxch					#i max size of filename

char	dotfile[SZ_PATHNAME], line[SZ_LINE]
int	fd, len, sum

int	strsum(), strlen(), getline(), access(), filopn()
bool	streq()

extern  zopntx(), zgettx()
errchk	filopn

begin
	if (access (cache, 0, 0) == NO)
	    call fmkdir (cache)

	# Initialize the output string, trash any newlines in the string.
	call aclrc (out, maxch)
	len = strlen (in)
	if (in[len] == '\n')
	    in[len] = EOS

	# Compute the string checksum.
	sum = strsum (in, len, SZ_LINE)

	# Format the dotfile name string.
	call sprintf (dotfile, SZ_PATHNAME, "%s.%s%d")
	    call pargstr (cache)
	    call pargstr (root)
	    call pargi (sum)

	# Format the name string.
	call sprintf (out, maxch, "%s%s%d")
	    call pargstr (cache)
	    call pargstr (root)
	    call pargi (sum)

	# Check to see if the file already exists.
	if (access (dotfile, 0, 0) == YES) {
	    fd = filopn (dotfile, READ_ONLY, TEXT_FILE, zopntx, zgettx)
	    if (getline (fd, line) != EOF) {
		len = strlen (line)
		line[len] = '\0'		# kill newline
	        call close (fd)
		if (streq (in, line))  	# file exists and is current
		    return

	 	else {
		    # FIXME -- what to do ????
		    ;
		}
	    }
	    call close (fd)

	} else {
	    # File doesn't exist so the name is unique.  Write the src string.
	    fd = filopn (dotfile, NEW_FILE, TEXT_FILE, zopntx, zgettx)
	    call fprintf (fd, "%s\n")
		call pargstr (in)
	    call close (fd)
	}
end


# FCSRC -- Return the source string for the named cache file.

procedure fcsrc (cache, in, out, maxch)

char	cache[ARB]				#i cache directory
char	in[ARB]					#i cache file name
char	out[ARB]				#o source string
int	maxch					#i size of output string

int	fd, len
char	dotfile[SZ_PATHNAME], cfname[SZ_PATHNAME]
char	dirname[SZ_PATHNAME], root[SZ_FNAME], line[SZ_LINE]
long	file_info[LEN_FINFO]

int	access(), fnldir(), fnroot(), open(), strlen(), getline(), finfo()

begin
	# Be sure the input file exists, if so the get the root part of
	# the filename.
	call sprintf (cfname, SZ_PATHNAME, "%s/%s")
	    call pargstr (cache)
	    call pargstr (in)
	if (access (in, 0, 0) == NO) {
	    if (access (cfname, 0, 0) == NO) {
		call strcpy ("", out, SZ_FNAME)
		return
	    }
	} else
	    call strcpy (in, cfname, SZ_PATHNAME)
	
	# Break up the filename.
	if (fnldir (cfname, dirname, SZ_PATHNAME) == NULL)
	    call strcpy ("./", dirname, SZ_PATHNAME)	# use current dir
	if (fnroot (cfname, root, SZ_FNAME) == NULL)
	    call strcpy (in, root, SZ_PATHNAME)		# use current dir

	# Read the dotfile to get the source string.
	if (root[1] == '.')
	    call sprintf (dotfile, maxch, "%s/%s")
	else
	    call sprintf (dotfile, maxch, "%s/.%s")
	    call pargstr (cache)
	    call pargstr (root)

	if (access (dotfile, 0, 0) == YES) {
	    if (finfo (dotfile, file_info) != ERR) {
		if (FI_TYPE(file_info) == FI_REGULAR) {
	            fd = open (dotfile, READ_ONLY, TEXT_FILE)
	            if (getline (fd, line) == EOF)
	            	call aclrc (out, maxch)
	            call close (fd)
		}
	    }
	} else
	    call aclrc (line, SZ_LINE)

	# Copy to the output string.
	len = strlen (line)
	line[len] = '\0'
	call strcpy (line, out, SZ_LINE)
end

			
# FC_FIND_EXTN -- Given a cache filename, see if a file/link exists with
# an extension and return the extn string.

procedure fc_find_extn (cache, cfname, extn, maxch)

char	cache[ARB]				#i cache directory
char	cfname[ARB]				#i cache file name
char	extn[ARB]				#o file extension
int	maxch					#i size of output string

char	fname[SZ_LINE], cmp[SZ_PATHNAME]
int	fd, clen, flen

int	diropen(), strncmp(), strlen(), getline()

begin
	call strcpy (cfname, cmp, maxch)
	call strcat (".", cmp, maxch)
	clen = strlen (cmp)

	fd = diropen (cache, SKIP_HIDDEN_FILES)
	while (getline (fd, fname) != EOF) {
	    flen = strlen (fname)
	    fname[flen] = '\0'

	    if (strncmp (fname, cmp, clen) == 0) {
		call strcpy (fname[clen+1], extn, maxch)
		break
	    }
	}
	call close (fd)
end
