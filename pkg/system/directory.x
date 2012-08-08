# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<diropen.h>
include	<protect.h>
include	<finfo.h>
include	<chars.h>
include	<ctype.h>
include	<fset.h>
include	<time.h>

# DIRECTORY -- List the files matching the given template.  If no template is
# given list the contents of the current directory.  Only existing files are
# listed, i.e., "dir file" may be used to test if "file" exists.  There are
# two types of file listings:
#
#	tabular		multicolumn listing, "ncolumns" columns (default)
#	long		long form listing; prints more info for each file
#
# The template may be either the name of a file or directory, or a true
# filename template containing pattern matching metacharacters.

define	DEF_MAXFILES	256		# initial max file limit
define	DEF_SZSBUF	4096		# initial string buffer size
define	DEF_SCREENWIDTH	80		# default screen width if envgeti fails
define	DEF_NCOLS	4		# default number of columns in table
define	NLINES_FLUSH	4		# nlines flushed out at a time (quick)
define	PATCHARS	"*?["

define	LEN_FLDES	6		# file list descriptor
define	FL_NFILES	Memi[$1]	# number of files in list
define	FL_NEXTOFF	Memi[$1+1]	# next offset in sbuf
define	FL_OFFBP	Memi[$1+2]	# pointer to list of offsets
define	FL_SBUFP	Memi[$1+3]	# pointer to string buffer
define	FL_MAXFILES	Memi[$1+4]	# current length of offset array
define	FL_SZSBUF	Memi[$1+5]	# current string buffer size

define	FL_OFFSET	Memi[FL_OFFBP($1)+($2)-1]
define	FL_FNAME	Memc[FL_SBUFP($1)+(FL_OFFSET($1,$2))-1]


# T_DIRECTORY -- CL callable main routine for the directory program.

procedure t_directory()

pointer	sp, files, fname, dirname, patp, fp, ip, op, ep
int	ncols, maxch, dirmode, fd, n, i, patlen, len_dir
bool	long_format, is_template, is_pattern, is_dir, sort_list, match_extension

bool	clgetb(), strne()
int	clgeti(), fntopnb(), fntgfnb()
int	diropen(), getline(), stridx(), strlen(), strncmp(), stridxs()
int	isdirectory(), access(), envgeti(), btoi()
string	patchars PATCHARS
define	template_ 91
define	done_ 92

begin
	call smark (sp)
	call salloc (files, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (dirname, SZ_PATHNAME, TY_CHAR)

	# If directory is called without any arguments, list the contents
	# of the current directory.  Otherwise read in the file template.

	if (clgeti ("$nargs") == 0)
	    call strcpy (".", Memc[files], SZ_LINE)
	else
	    call clgstr ("files", Memc[files], SZ_LINE)

	# Determine whether a long format listing is desired.
	long_format = clgetb ("long")

	# If not long format listing, determine the number of columns for
	# the multicolumn format listing.

	if (!long_format)
	    ncols = clgeti ("ncols")

	# Max chars of each filename to be shown.
	maxch = clgeti ("maxch")

	# Sort files?
	sort_list = clgetb ("sort")

	# Show hidden files?
	if (clgetb ("all"))
	    dirmode = PASS_HIDDEN_FILES
	else
	    dirmode = SKIP_HIDDEN_FILES

	# Allocate and initialize storage for the file list descriptor and
	# assocated array storage.
	
	call calloc (fp, LEN_FLDES, TY_STRUCT)
	call malloc (FL_OFFBP(fp), DEF_MAXFILES, TY_INT)
	call malloc (FL_SBUFP(fp), DEF_SZSBUF, TY_CHAR)

	FL_MAXFILES(fp) = DEF_MAXFILES
	FL_SZSBUF(fp)   = DEF_SZSBUF
	FL_NEXTOFF(fp)  = 1

	# The template is considered a filename template if it contains any
	# of the metacharacters "*?[],", i.e., any string containing a comma
	# is considered a filename template, since we don't want to mess with
	# parsing comma delimited lists here.

	is_template = false
	is_pattern  = false

	for (ip=files;  Memc[ip] != EOS;  ip=ip+1)
	    if (IS_ALPHA (Memc[ip])) {
		next
	    } else if (stridx (Memc[ip], patchars) > 0) {
		if (ip == files || Memc[ip-1] != '\\')
		    is_pattern  = true
	    } else if (Memc[ip] == ',' || Memc[ip] == '%')
		if (ip == files || Memc[ip-1] != '\\')
		    is_template = true

	# Set IS_DIR flag, and get directory name if not fancy template.

	if (is_template) {
	    is_dir = false

	} else if (is_pattern) {
	    is_dir = true
	    patp = NULL
	    ep = NULL

	    # Search for a valid directory prefix.
	    op = dirname
	    for (ip=files;  Memc[ip] != EOS;  ip=ip+1) {
		Memc[op] = Memc[ip]
		if (Memc[op] == FNLDIR_CHAR || Memc[op] == '//')
		    if (op == dirname || Memc[op-1] != '\\') {
			patp = ip + 1
			ep = op + 1
		    }
		op = op + 1
	    }

	    # Must set patp and dirname before exiting.
	    if (patp == NULL) {
		patp = files
		call strcpy ("./", Memc[dirname], SZ_PATHNAME)
		len_dir = 0
	    } else {
		Memc[ep] = EOS
		len_dir = strlen (Memc[dirname])
	    }

	} else
	    is_dir = (isdirectory (Memc[files],Memc[dirname],SZ_PATHNAME) > 0)

	# If sorting is not desired and we either don't have a template, or
	# a simple unary match-pattern template, we can list the directory
	# directly to the standard output as it is read.  This is the fastest
	# possible type of of directory listing, and is particularly useful
	# for listing very large directories.

	if (is_dir && !sort_list && !long_format && !is_template) {
	    fd = diropen (Memc[dirname], dirmode)
	    call dir_quicklist (STDOUT, fd, Memc[patp], is_pattern, ncols)
	    call close (fd)
	    goto done_
	}

	# If `files' is a template, expand the template and read the file
	# names into memory.  If `files' is not a filename template, determine
	# if it is the name of (or pathname to) a directory.  If so, open the
	# directory directly and read the file list into memory.  If `files' is
	# neither a template nor a directory, it must be a filename, so merely
	# put its name in the file list. 

	if (!is_template && is_dir) {
	    # Read file list from the named directory.

	    # Set up the pattern matching code.  We recognize selecting all
	    # files with a particular extension as a special case, since this
	    # case is very common and can be done more efficiently if we don't
	    # use the general pattern matching code.

	    if (is_pattern) {
		match_extension = (strncmp (Memc[patp], "*.", 2) == 0 &&
		    stridxs (patchars, Memc[patp+2]) <= 0)
		if (match_extension)
		    patlen = strlen (Memc[patp])
		else
		    goto template_
	    }

	    fd = diropen (Memc[dirname], dirmode)

	    # Accumulate the file list in memory.
	    n = 0
	    while (n != EOF) {
		n = getline (fd, Memc[fname])
		if (n <= 1)
		    break
		n = n - 1
		Memc[fname+n] = EOS		# clobber the \n

		# Check if file matches the given pattern.
		if (is_pattern) {
		    if (n < patlen)
			next
		    ep = fname + n - 1
		    for (ip=patp+patlen-1;  ip > patp+1;  ip=ip-1) {
			if (Memc[ep] != Memc[ip])
			    break
			ep = ep - 1
		    }
		    if (Memc[ip] != '.' || Memc[ep] != '.')
			next
		}

		call dir_putstr (fp, Memc[fname], n)
	    }

	    call close (fd)
	    if (sort_list && FL_NFILES(fp) > 1)
		call strsrt (FL_OFFSET(fp,1), Memc[FL_SBUFP(fp)], FL_NFILES(fp))

	} else {
	    # Expand the given filename template and read the file list into
	    # memory.  This also handles the cases of a single filename or
	    # a simple list of filenames.
template_
	    fd = fntopnb (Memc[files], btoi(sort_list))

	    n  = fntgfnb (fd, Memc[fname], SZ_PATHNAME)
	    while (n != EOF) {
		ip = fname
		if (!is_template && is_pattern)
		    ip = fname + len_dir;
		if (is_pattern)
		    call dir_putstr (fp, Memc[ip], n)
		else if (access (Memc[fname],0,0) == YES)
		    call dir_putstr (fp, Memc[ip], n)
		n = fntgfnb (fd, Memc[fname], SZ_PATHNAME)
	    }

	    call fntclsb (fd)
	}

	# All done if no files were found.
	if (FL_NFILES(fp) == 0) {
	    call printf ("no files found\n")
	    goto done_
	}

	# Format and output the file list in either long format (verbose)
	# or in multicolumn format.  A one column file list is treated as
	# a special case of the multifile format.

	if (long_format || ncols == 1) {
	    for (i=1;  i <= FL_NFILES(fp);  i=i+1) {
		# Get filename.
		if (is_dir && strne (Memc[dirname], "./")) {
		    call sprintf (Memc[fname], SZ_PATHNAME, "%s%s")
			call pargstr (Memc[dirname])
			call pargstr (FL_FNAME(fp,i))
		} else
		    call strcpy (FL_FNAME(fp,i), Memc[fname], SZ_PATHNAME)

		if (long_format) {
		    # Long format output, one line per file.
		    call dir_pfiledata (Memc[fname], STDOUT)
		} else {
		    call printf ("%s\n")
			call pargstr (Memc[fname])
		}
	    }

	} else {
	    # Print nice multicolumn table.
	    call strtbl (STDOUT, Memc[FL_SBUFP(fp)], FL_OFFSET(fp,1),
		FL_NFILES(fp), 1, envgeti ("ttyncols"), maxch, ncols)
	}
done_
	call mfree (FL_OFFBP(fp), TY_INT)
	call mfree (FL_SBUFP(fp), TY_CHAR)
	call mfree (fp, TY_STRUCT)
	call sfree (sp)
end


# DIR_QUICKLIST -- List the directory directly to the standard output, without
# first reading and sorting the entire directory.  This is the best type of
# listing algorithm for very large directories.

procedure dir_quicklist (out, fd, pat, have_pattern, a_ncols)

int	out			# output file
int	fd			# fd of directory being listed
char	pat[ARB]		# selection pattern, if any
bool	have_pattern		# do we have a pattern?
int	a_ncols			# number of columns out

bool	flushlines, match_extension
pointer	sp, fname, obuf, patbuf, op, ep
int	colwidth, patlen, ip, col, maxch, junk, lastch
int	scrwidth, ncols, nfiles, nchars, nblanks, nlines, nflush
int	fstati(), getline(), envgeti(), patmake(), patmatch()
int	strncmp(), stridxs(), strlen()
errchk	getline, ungetline, putline

begin
	call smark (sp)
	call salloc (obuf, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	# Initialization.
	flushlines = (out == STDOUT && fstati (out, F_REDIR) == NO)
	iferr (scrwidth = envgeti ("ttyncols"))
	    scrwidth = DEF_SCREENWIDTH

	ncols = a_ncols
	if (ncols <= 0)
	    ncols = DEF_NCOLS
	maxch = scrwidth / ncols

	nfiles = 0
	nchars = 0
	nlines = 0
	nflush = 1

	# Set up the pattern matching code.  We recognize selecting all files
	# with a particular extension as a special case, since this case is
	# very common and can be done much more efficiently if we don't use
	# the general pattern matching code.

	if (have_pattern) {
	    match_extension = (strncmp (pat, "*.", 2) == 0 &&
		stridxs (PATCHARS, pat[3]) <= 0)
	    if (match_extension)
		patlen = strlen (pat)
	    else {
		# Convert file matching pattern into general pattern string.
		Memc[fname] = '^'
		op = fname + 1
		lastch = 0
		for (ip=1;  pat[ip] != EOS;  ip=ip+1) {
		    if (pat[ip] == '*' && lastch != '?' && lastch != ']') {
			Memc[op] = '?'
			op = op + 1
		    }
		    lastch = pat[ip]
		    Memc[op] = lastch
		    op = op + 1
		}
		Memc[op] = '$'
		op = op + 1
		Memc[op] = EOS
			
		# Compile the pattern.
		junk = patmake (Memc[fname], Memc[patbuf], SZ_LINE)
	    }
	}

	# List the directory.

	while (nchars != EOF) {
	    # Compose the next line of the directory listing.
	    op = obuf
	    for (col=1;  col <= ncols;  ) {
		nchars = getline (fd, Memc[fname])
		if (nchars <= 1)
		    break

		# Cancel the newline.
		nchars = nchars - 1
		Memc[fname+nchars] = EOS

		# Check if file matches the given pattern.
		if (have_pattern)
		    if (match_extension) {
			if (nchars < patlen)
			    next
			ep = fname + nchars - 1
			for (ip=patlen;  ip > 2;  ip=ip-1) {
			    if (Memc[ep] != pat[ip])
				break
			    ep = ep - 1
			}
			if (pat[ip] != '.' || Memc[ep] != '.')
			    next
		    } else if (patmatch (Memc[fname], Memc[patbuf]) <= 0)
			next

		if (op-obuf + nchars > scrwidth && col > 1) {
		    # Filename too long to fit in remainder of line.
		    call ungetline (fd, Memc[fname])
		    break

		} else {
		    # Append filename to output line.
		    nfiles = nfiles + 1
		    nchars = min (nchars, scrwidth - (op - obuf))

		    # Copy the filename.
		    do ip = 1, nchars {
			Memc[op] = Memc[fname+ip-1]
			op = op + 1
		    }

		    # Advance to the next column.
		    colwidth = maxch
		    while (col < ncols) {
			nblanks = colwidth - nchars
			if (nblanks < 1) {
			    col = col + 1		# grab another column
			    colwidth = colwidth + maxch
			} else {
			    do ip = 1, nblanks {
				Memc[op] = ' '
				op = op + 1
			    }
			    break
			}
		    }
		}

		col = col + 1
	    }

	    # Output one line of the directory listing.
	    if (op > obuf) {
		Memc[op] = '\n'
		op = op + 1
		Memc[op] = EOS

		call putline (out, Memc[obuf])

		nlines = nlines + 1
		if (flushlines && nlines >= nflush) {
		    call flush (out)
		    nflush = min (NLINES_FLUSH, nflush + 1)
		    nlines = 0
		}
	    }
	}

	# Was the directory empty?
	if (nfiles == 0)
	    call printf ("no files found\n")

	call sfree (sp)
end


# DIR_PUTSTR -- Put a string (filename) into the file list.  If the buffer
# fills up allocate a larger buffer.

procedure dir_putstr (fp, fname, nchars)

pointer	fp			# file list pointer
char	fname[ARB]		# file name to be put in list
int	nchars			# nchars in file name

int	op
int	fileno

begin
	# Increment file count.
	fileno = FL_NFILES(fp) + 1
	FL_NFILES(fp) = fileno

	# Increase size of file list if it overflows.
	if (fileno >= FL_MAXFILES(fp)) {
	    FL_MAXFILES(fp) = FL_MAXFILES(fp) * 2
	    call realloc (FL_OFFBP(fp), FL_MAXFILES(fp), TY_INT)
	}

	op = FL_NEXTOFF(fp)
	FL_OFFSET(fp,fileno) = op

	# Increase size of string buffer if it overflows.
	if (op + nchars + 1 >= FL_SZSBUF(fp)) {
	    FL_SZSBUF(fp) = FL_SZSBUF(fp) * 2
	    call realloc (FL_SBUFP(fp), FL_SZSBUF(fp), TY_CHAR)
	}

	call strcpy (fname, FL_FNAME(fp,fileno), nchars)
	FL_NEXTOFF(fp) = op + nchars + 1
end


# DIR_PFILEDATA -- Print file info in a UNIX like long directory format, on
# the output file OUT.  Format, e.g.,: "-t-rwr-r- tody 5117 May 15 19:29 fio.x"

procedure dir_pfiledata (fname, out)

char	fname[ARB]			# file name
int	out				# output file

pointer	sp, date
long	fi[LEN_FINFO]
int	protected, ftypes[4], i
int	finfo(), access(), bitupk(), protect()
data	ftypes /'-', 'd', 'x', 's'/

begin
	call smark (sp)
	call salloc (date, SZ_DATE, TY_CHAR)

	# Get file directory information.
	if (finfo (fname, fi) != OK) {
	    call eprintf ("Cannot get info on file `%s'\n")
		call pargstr (fname)
	    call sfree (sp)
	    return
	}

	# Query file protection.
	protected = protect (fname, QUERY_PROTECTION)

	# Output file mode bit flags.
	call dir_putci (out, ftypes[FI_TYPE(fi)], '-', YES)
	call dir_putci (out, 't', 'b', access (fname,0,TEXT_FILE))
	call dir_putci (out, 'p', '-', protected)

	# Output user,group,world protections bit flags.
	do i = 1, 5, 2 {
	    call dir_putci (out, 'r', '-', bitupk (int(FI_PERM(fi)),i,1))
	    call dir_putci (out, 'w', '-', bitupk (int(FI_PERM(fi)),i+1,1))
	}

	# Output file owner, size, date, and name.
	call cnvdate (FI_MTIME(fi), Memc[date], SZ_DATE)
	call fprintf (out, " %-8.8s%8d %12.12s %-32.32s\n")
	    call pargstr (FI_OWNER(fi))
	    call pargl (FI_SIZE(fi))
	    call pargstr (Memc[date])
	    call pargstr (fname)

	# This is slow so flush the output after each file to give the user
	# some immediate gratification.

	call flush (out)
	call sfree (sp)
end


# DIR_PUTCI -- Handy procedure used to generate --rw-r-r- type strings.

procedure dir_putci (fd, ch_t, ch_f, condition)

int	fd			# output file
int	ch_t			# character to output if condition is true
int	ch_f			# character to output if condition is false
int	condition

begin
	if (condition == NO)
	    call putci (fd, ch_f)
	else
	    call putci (fd, ch_t)
end
