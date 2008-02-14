# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <diropen.h>
include	<ctype.h>
include	<finfo.h>

include "xhelp.h"


# Pattern matching definitions.
define  PATCHARS        "*?[" 		

# Browsing command dictionary.
define	DIR_CMDS "|dirlist|loadfile|open|template|home|up|root|rescan|save|"
define	DIRLIST		1		# get the directory listing
define	LOADFILE	2		# load the requested file
define	OPEN		3		# load the requested file
define	TEMPLATE	4		# filename matching template
define	HOME		5		# goto the user's home$
define	UP		6		# go up one directory
define	ROOT		7		# go to the root directory
define	RESCAN		8		# rescan current directory
define	SAVE		9		# save to the requested file


# XH_DIRECTORY -- Process the directory browsing command.

procedure xh_directory (xh, command)

pointer	xh					#i task descriptor
char	command[ARB]				#i command option

pointer	sp, dir, file, pattern, path, fmt
pointer task, pkg, opt, type
int	ncmd, overwrite
int	strdic(), strcmp(), envgets()

begin
	# Allocate working space and clear it.
	call smark (sp)
	call salloc (dir, SZ_PATHNAME, TY_CHAR) 	
	call salloc (path, SZ_PATHNAME, TY_CHAR) 	
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (task, SZ_FNAME, TY_CHAR)
	call salloc (pkg, SZ_FNAME, TY_CHAR)
	call salloc (opt, SZ_FNAME, TY_CHAR)
	call salloc (type, SZ_FNAME, TY_CHAR)

	call aclrc (Memc[dir], SZ_FNAME)
	call aclrc (Memc[path], SZ_FNAME)
	call aclrc (Memc[file], SZ_FNAME)
	call aclrc (Memc[fmt], SZ_FNAME)
	call aclrc (Memc[pattern], SZ_FNAME)
	call aclrc (Memc[task], SZ_FNAME)
	call aclrc (Memc[pkg], SZ_FNAME)
	call aclrc (Memc[opt], SZ_FNAME)
	call aclrc (Memc[type], SZ_FNAME)

	ncmd = strdic (command, command, SZ_LINE, DIR_CMDS)
	switch (ncmd) {
	    case DIRLIST:
		call gargwrd (Memc[dir], SZ_PATHNAME) 	# get the dirname
		if (strcmp ("../", Memc[dir]) == 0) {
		    call xh_updir (xh)
		} else {
		    call sprintf (Memc[path], SZ_PATHNAME, "%s%s")
		        call pargstr (CURDIR(xh))
		        call pargstr (Memc[dir])
		    call xh_set_curdir (xh, Memc[path])
		    call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))
	            call xh_selection (xh, CURDIR(xh))
		}

	    case LOADFILE:
		call gargwrd (Memc[file], SZ_FNAME) 	# get the filename
		call sprintf (Memc[path], SZ_PATHNAME, "%s%s")
		    call pargstr (CURDIR(xh))
		    call pargstr (Memc[file])
		call xh_open_file (xh, "helpres", Memc[path], YES, YES)
	        call xh_selection (xh, Memc[path])

	    case OPEN:
		call gargwrd (Memc[file], SZ_FNAME) 	# get the filename
		call xh_ldfile (xh, Memc[file])

	    case TEMPLATE:
		call gargwrd (Memc[pattern], SZ_FNAME) 	# set the template
		call xh_set_pattern (xh, Memc[pattern])
		call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))

	    case HOME:
		if (envgets ("home", Memc[dir], SZ_PATHNAME) != EOF) {
		    call xh_set_curdir (xh, Memc[dir])
		    call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))
	   	}

	    case UP:
		call xh_updir (xh)

	    case ROOT:
		call xh_set_curdir (xh, "/")
		call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))

	    case RESCAN:
		call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))

	    case SAVE:
		call gargwrd (Memc[path], SZ_FNAME) 	# get the filename
		call gargwrd (Memc[file], SZ_FNAME) 	# get the output fname
		call gargi (overwrite) 			# get the overwrite flag
		call gargwrd (Memc[fmt], SZ_FNAME) 	# get the save format
		call xh_save_file (xh, Memc[path], Memc[file], Memc[fmt],
		    overwrite)
	}

	call sfree (sp)
end


# XH_DIRLIST -- Given the directory name and a file template return the
# directory contents.

procedure xh_dirlist (xh, directory, pattern)

pointer	xh					#i task descriptor
char	directory[ARB]				#i directory to read
char	pattern[ARB]				#i matching template

pointer	sp, path, fname, patbuf
pointer	dp, fp, ip, op, ep, sym
bool	match_extension
int	dd, n, patlen
int	nfiles, ndirs, lastch

pointer	stopen(), stenter()
int	diropen(), xh_isdir(), strncmp(), stridxs()
int	patmake(), patmatch(), strlen(), getline()

begin
	call smark (sp)
	call salloc (path, SZ_PATHNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	call aclrc (Memc[patbuf], SZ_LINE)

	# If this isn't a directory just return silently.
	if (xh_isdir (directory, Memc[path], SZ_PATHNAME) == 0) {
	    call sfree (sp)
	    return
	}
	
	# Open the requested directory
	dd = diropen (directory, PASS_HIDDEN_FILES)

	# Set up the pattern matching code.  We recognize selecting all files
	# with a particular extension as a special case, since this case is
	# very common and can be done much more efficiently if we don't use
	# the general pattern matching code.  If we have no pattern set the
	# length to zero to indicate that everything will match.

	if (pattern[1] == EOS) {
	    patlen = 0
	} else {
	    match_extension = (strncmp (pattern, "*.", 2) == 0 &&
		stridxs (PATCHARS, pattern[3]) <= 0)
	    if (match_extension)
		patlen = strlen (pattern)
	    else {
		# Convert file matching pattern into general pattern string.
		Memc[fname] = '^'
		op = fname + 1
		lastch = 0
		for (ip=1;  pattern[ip] != EOS;  ip=ip+1) {
		    if (pattern[ip] == '*' && lastch != '?' && lastch != ']') {
			Memc[op] = '?'
			op = op + 1
		    }
		    lastch = pattern[ip]
		    Memc[op] = lastch
		    op = op + 1
		}
		Memc[op] = '$'
		op = op + 1
		Memc[op] = EOS
			
		# Compile the pattern.
		patlen = patmake (pattern, Memc[patbuf], SZ_LINE)
	    }
	}

	# Initialize counters.
	ndirs  = 0
	nfiles = 0
	dp = NULL
	fp = NULL

	# Accumulate the contents into the directory and files lists.  We
	# match files against the given template, all directories are
	# matched regardless.
	for (n=0; n != EOF; ) {
	    n = getline (dd, Memc[fname])
	    if (n < 1)
		break
	    n = n - 1
	    Memc[fname+n] = EOS			# stomp the newline

	    # See if this is a directory.
	    call sprintf (Memc[path], SZ_PATHNAME, "%s%s")
		call pargstr (CURDIR(xh))
		call pargstr (Memc[fname])
	    if (xh_isdir (Memc[path], Memc[path], SZ_PATHNAME) > 0) {
		ndirs = ndirs + 1

		# If this is the first directory initialize the symbol table.
		if (ndirs == 1)
		    dp = stopen ("dirlist", LEN_INDEX, LEN_STAB, SZ_SBUF)

		# Enter the directory name into the symbol table.
		call strcat ("/", Memc[fname], SZ_FNAME)
		sym = stenter (dp, Memc[fname], strlen(Memc[fname])+1)

	    } else {
		# Check if the file matches the given pattern.
		if (patlen > 0) {
		    if (match_extension) {
			if (n < patlen)
			    next
			ep = fname + n - 1
			for (ip=patlen; ip > 2; ip=ip-1) {
			    if (Memc[ep] != pattern[ip])
				break
			    ep = ep - 1
			}
			if (pattern[ip] != '.' || Memc[ep] != '.')
			    next
		    } else if (patmatch (Memc[fname], Memc[patbuf]) <= 0)
			next
		}

		# We have a match.
		nfiles = nfiles + 1

		# If this is the first file initialize the symbol table.
		if (nfiles == 1)
		    fp = stopen ("filelist", LEN_INDEX, LEN_STAB, SZ_SBUF)

		# Enter the directory name into the symbol table.
		sym = stenter (fp, Memc[fname], strlen(Memc[fname])+1)
	    }
	}

	# Send the results to the GUI.
	call xh_putlist (xh, dp, "directory", "dirlist")
	call xh_putlist (xh, fp, "directory", "filelist")

	# Clean up.
	if (dp != NULL)
	    call stclose (dp)
	if (fp != NULL)
	    call stclose (fp)
	call close (dd)
	call sfree (sp)
end


# XH_PUTLIST -- Given the symtab for the directory contents construct a
# list suitable for a message to the GUI.  The 'arg' parameter is passed
# to indicate which type of list this is.

procedure xh_putlist (xh, stp, param, arg)

pointer	xh					#i task descriptor
pointer	stp					#i symtab ptr for list
char	param[ARB]				#i GUI param to notify
char	arg[ARB]				#i GUI param arg

pointer	sp, list, msg, sym, name, ip
int	nchars

pointer	sthead(), stnext(), stname()
int	stsize(), gstrcpy(), strcmp(), strlen()

begin
	# Return if there is no symtab information.
	if (stp == NULL) {
	    call smark (sp)
	    call salloc (msg, SZ_FNAME , TY_CHAR)
	    call sprintf (Memc[msg], SZ_FNAME, "%s { }")
	        call pargstr (arg)

	    call gmsg (XH_GP(xh), param, Memc[msg]) 	# send it to the GUI
	    call gflush (XH_GP(xh))
	    call sfree (sp)
	    return
	}

	# Allocate space for the list.
	nchars =  stsize (stp) + 1

	call smark (sp)
	call salloc (list, nchars , TY_CHAR)
	call aclrc (Memc[list], nchars)
	ip = list

	# Build the list from the symtab.
        for (sym = sthead (stp); sym != NULL; sym = stnext (stp,sym)) {
	    name = stname(stp,sym)
	    if (strcmp (Memc[name], "./") != 0) {
	        ip = ip + gstrcpy (Memc[name], Memc[ip], SZ_FNAME)
	        ip = ip + gstrcpy (" ", Memc[ip], SZ_FNAME)
	    }
	}

	# Sort the list.
	call xh_sort_list (Memc[list])

	# Allocate space for the message buffer.  The "+ 6" is space for
	# the brackets around the list in the message created below.
	nchars = nchars + strlen (arg) + 6
	call salloc (msg, nchars, TY_CHAR)
	call aclrc (Memc[msg], nchars)
	ip = msg

	# Begin the message by adding the arg and make a Tcl list of the
	# contents.
	call sprintf (Memc[msg], nchars, "%s { ")
	    call pargstr (arg)
	call strcat (Memc[list], Memc[msg], nchars)
	call strcat (" }", Memc[msg], nchars)

	# Finally, send it to the GUI.
	call gmsg (XH_GP(xh), param, Memc[msg])
	call gflush (XH_GP(xh))

	call sfree (sp)
end


# XH_LDFILE -- Load the requested file.  If this is a file display it's
# contents and update the browser with it's directory, otherwise if it's
# a directory jump the browser to that directory.

procedure xh_ldfile (xh, file)

pointer	xh					#i task descriptor
char	file[ARB]				#i requested file/dir

pointer	sp, ip, dir, parent, path
int	nchars
int	access(), strlen(), xh_isdir()

begin
	call smark (sp)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)
	call salloc (path, SZ_PATHNAME, TY_CHAR)
	call salloc (parent, SZ_PATHNAME, TY_CHAR)

	# Expand the current directory to a host path.
	call fdirname (file, Memc[dir], SZ_PATHNAME)

	if (xh_isdir (Memc[dir], Memc[path], SZ_PATHNAME) > 0) {
	    # Set the curdir and load it's contents.
	    call xh_set_curdir (xh, Memc[path])
	    call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))
	    call xh_selection (xh, Memc[path])
	} 

	if (access(file,0,0) == YES && 
	    xh_isdir (file, Memc[path], SZ_PATHNAME) == 0) {
	        # Work backwards to the parent '/', be sure to skip the trailing
	        # backslash already in the dirname.
	        ip = dir + strlen (Memc[dir]) - 2
	        while (Memc[ip] != '/' && ip > dir) 
	            ip = ip - 1

	        nchars = ip - dir
	        if (nchars > 0)
	            call strcpy (Memc[dir], Memc[parent], nchars)
	        else
	            call strcpy ("/", Memc[parent], nchars)

	        # Set the parent dir and load it's contents.
	        call xh_set_curdir (xh, Memc[parent])
	        call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))

	        # Now load the file itself.
	        call xh_open_file (xh, "helpres", file, YES, YES)
	        call xh_selection (xh, file)
	}

	call sfree (sp)
end


# XH_UPDIR -- Go up to the parent directory and return contents.

procedure xh_updir (xh)

pointer	xh					#i task descriptor

pointer	sp, ip, dir, parent
int	nchars, strlen()

begin
	call smark (sp)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)
	call salloc (parent, SZ_PATHNAME, TY_CHAR)

	# Expand the current directory to a host path.
	call fdirname (CURDIR(xh), Memc[dir], SZ_PATHNAME)

	# Work backwards to the parent '/', be sure to skip the trailing
	# backslash already in the dirname.
	ip = dir + strlen (Memc[dir]) - 2
	while (Memc[ip] != '/' && ip > dir) 
	    ip = ip - 1

	nchars = ip - dir
	if (nchars > 0)
	    call strcpy (Memc[dir], Memc[parent], nchars)
	else
	    call strcpy ("/", Memc[parent], nchars)

	# Set the parent dir and load it's contents.
	call xh_set_curdir (xh, Memc[parent])
	call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))
	call xh_selection (xh, CURDIR(xh))

	call sfree (sp)
end


# XH_SET_CURDIR -- Set the filename matching template pattern.

procedure xh_set_curdir (xh, dir)

pointer	xh					#i task descriptor
char	dir[ARB]				#i current directory

pointer	sp, dirbuf
int	strlen()

begin
	call smark (sp)
	call salloc (dirbuf, SZ_PATHNAME, TY_CHAR)

	call strcpy (dir, CURDIR(xh), SZ_PATHNAME)
	if (dir[strlen(dir)] != '/')
	    call strcat ("/", CURDIR(xh), SZ_PATHNAME)

	call sprintf (Memc[dirbuf], SZ_PATHNAME, "curdir %s")
	    call pargstr (CURDIR(xh))

	call gmsg (XH_GP(xh), "directory", Memc[dirbuf])
	call gflush (XH_GP(xh))

	call sfree (sp)
end


# XH_SET_PATTERN -- Set the filename matching template pattern.

procedure xh_set_pattern (xh, pattern)

pointer	xh					#i task descriptor
char	pattern[ARB]				#i template pattern

pointer	sp, patbuf

begin
	call smark (sp)
	call salloc (patbuf, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[patbuf], SZ_FNAME, "template %s")
	    call pargstr (pattern)

	call strcpy (pattern, PATTERN(xh), SZ_FNAME)
	call gmsg (XH_GP(xh), "directory", Memc[patbuf])
	call gflush (XH_GP(xh))

	call sfree (sp)
end


# XH_SELECTION -- Set the selected filename.

procedure xh_selection (xh, selection)

pointer	xh					#i task descriptor
char	selection[ARB]				#i selection

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[buf], SZ_FNAME, "selection %s")
	    call pargstr (selection)

	call gmsg (XH_GP(xh), "directory", Memc[buf])
	call gflush (XH_GP(xh))
	call sfree (sp)
end


# XH_ISDIR -- Test whether the named file is a directory.  Check first to
# see if it is a subdirectory of the current directory. If VFN is a directory,
# return the OS pathname of the directory in pathname, and the number of
# chars in the pathname as the function value.  Otherwise return 0.

int procedure xh_isdir (vfn, pathname, maxch)

char	vfn[ARB]		# name to be tested
char	pathname[ARB]		# receives path of directory
int	maxch			# max chars out

bool	isdir
pointer	sp, fname, op
int	ip, fd, nchars, ch
long	file_info[LEN_FINFO]
int	finfo(), diropen(), gstrcpy(), strlen()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Copy the VFN string, minus any whitespace on either end.
	op = fname
	for (ip=1;  vfn[ip] != EOS;  ip=ip+1) {
	    ch = vfn[ip]
	    if (!IS_WHITE (ch)) {
		Memc[op] = ch
		op = op + 1
	    }
	}
	Memc[op] = EOS

	isdir = false
	if (finfo (Memc[fname], file_info) != ERR) {
	    isdir = (FI_TYPE(file_info) == FI_DIRECTORY)

	    if (isdir) {
		call fdirname (Memc[fname], pathname, maxch)
		nchars = strlen (pathname)
	    }

	} else {
	    # If we get here, the VFN is the name of a new file.
	    ifnoerr (fd = diropen (Memc[fname], 0)) {
		call close (fd)
		isdir = true
	    }
	    nchars = gstrcpy (Memc[fname], pathname, maxch)
	}

	call sfree (sp)
	if (isdir)
	    return (nchars)
	else {
	    pathname[1] = EOS
	    return (0)
	}
end
