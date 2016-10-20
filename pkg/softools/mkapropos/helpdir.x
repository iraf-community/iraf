# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<error.h>
include	"help.h"
include	"helpdir.h"

.help helpdir
.nf ___________________________________________________________________________
HELPDIR -- Routines for accessing helpdir files.  A helpdir lists the
modules in a package, as well as the files containing the help text and
source for each module.  There are two kinds of helpdir files: the 'packhelp'
file lists the packages, and each package has its own helpdir file listing
the modules within that package.  The following functions are required to
access help module lists:

	hp =        hd_open (fname)
		   hd_close (hp)

	modnum = hd_findmod (hp, modname)
	nchars = hd_getname (hp, modnum, field, outstr, maxch)

The HD_OPEN function opens the helpdir file and decodes the contents, producing
a binary structure pointed to by hp.  HD_FINDMOD searches for a module by name,
returning the module number within the helpdir.  HD_GETNAME fetches either
the module name (field "mod") or a file name (fields "hlp", "sys", etc.).
File names are returned as OS pathnames, with logical directory expansion taking
place within the helpdir package.

Helpdir files have the following structure:

	$defdir = pathname|ldir
	$ldir1  = pathname|ldir
	$ldir2  = pathname|ldir
			...
	$ldirN  = pathname|ldir

	module1 hlp=file, sys=file, src=file, pkg=file, men=file
	module2 hlp=file, etc.
			...
	moduleN hlp=file, etc.

The dollar signs are required to make it easy to distinguish logical directory
declarations from module entries.  Logical directories defined local to the
help directory file are not expanded recursively.  If the last nonwhite char
on a line is a comma, the file list is assumed to be continued on the next
line.  If two or more files in a module list are the same, all but the first
may be set to ".." and the last file name given will be used.  Quotes are
optional.  All file assignments are optional, and they may occur in any order.
.endhelp _______________________________________________________________________


# HD_OPEN -- Open the helpdir file, allocate descriptor and decode file
# into descriptor.  Sort module list when done, unless sorting is disabled
# by inclusion of the directive ".nosort" in the text. Ignore comment lines and
# blank lines.  Ldir declarations and module entries may be given in any
# order.

pointer procedure hd_open (helpdir_file)

char	helpdir_file[ARB]
bool	sort_modules
pointer	sp, hp, ip, lbuf, defdir, word
int	fd, junk

bool	streq()
int	open(), getline(), fnroot(), fnldir(), ctowrd()
pointer	hd_putstr()
errchk	salloc, open, calloc, malloc, getline, hd_putstr
errchk	hd_putldiry, hd_putmodule, hd_sort_modules

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (defdir, SZ_PATHNAME, TY_CHAR)
	call salloc (word, SZ_FNAME, TY_CHAR)

	# If helpdir file is not yet installed, print warning message
	# and return the NULL pointer, indicating that the help directory
	# could not be loaded.

	iferr (fd = open (helpdir_file, READ_ONLY, TEXT_FILE)) {
	    call sfree (sp)
	    call erract (EA_ERROR)
	    return (NULL)
	}

	# Allocate and initialize descriptor and string buffer.  Must init
	# nextch to 1 because 0 is the null index.

	call calloc (hp, LEN_HDSTRUCT, TY_STRUCT)
	call malloc (HD_SBUF(hp), SZ_SBUF, TY_CHAR)
	HD_DEFDIR(hp) = NULL
	HD_NEXTCH(hp) = 1
	HD_SZSBUF(hp) = SZ_SBUF
	HD_MAXMODULES(hp) = MAX_MODULES
	HD_LENHD(hp) = LEN_HDSTRUCT
	sort_modules = true

	# Extract package name into string buffer.  The package name is assumed
	# to be the root of the filename.

	junk = fnroot (helpdir_file, Memc[lbuf], SZ_LINE)
	HD_PAKNAME(hp) = hd_putstr (hp, Memc[lbuf])

	# Extract directory prefix of the package help directory file.  Make
	# it the default directory prefix for all filenames in the directory.

	if (fnldir (helpdir_file, Memc[defdir], SZ_PATHNAME) == 0) {
	    call fpathname (helpdir_file, Memc[lbuf], SZ_LINE)
	    junk = fnldir (Memc[lbuf], Memc[defdir], SZ_PATHNAME)
	}
	call sprintf (Memc[lbuf], SZ_LINE, "defdir = %s\n")
	    call pargstr (Memc[defdir])
	call hd_putldiry (hp, Memc[lbuf])

	# Compile the file, processing all logical directory definitions,
	# set option statments, and module declarations. Ignore blank lines
	# and comment lines.

	while (getline (fd, Memc[lbuf]) != EOF) {
	    for (ip=lbuf;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '#' || Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    # A leading dollar sign denotes a logical directory declaration.
	    # A leading period denotes a compiler directive.
	    # Anything else is a module entry.

	    if (Memc[ip] == '$') {
		call hd_putldiry (hp, Memc[ip+1])
	    } else if (Memc[ip] == '.') {
		ip = ip + 1
		if (ctowrd (Memc, ip, Memc[word], SZ_FNAME) <= 0)
		    next
		# The directive ".nosort" disables sorting of the module list.
		# There are no other such directives at present, but they are
		# easy to add.
		if (streq (Memc[word], "nosort"))
		    sort_modules = false
		else {
		    call eprintf ("Warning: unknown directive %s in helpdir\n")
			call pargstr (Memc[word])
		}
		next
	    } else
		call hd_putmodule (hp, fd, Memc[lbuf])
	}

	# We are all done with the helpdir file, so close it.  Sort module
	# list alphabetically by name.

	call close (fd)
	if (sort_modules && HD_NMODULES(hp) > 1)
	    call hd_sort_modules (hp)

	# Return any unused space in string buffer.
	call realloc (HD_SBUF(hp), HD_NEXTCH(hp), TY_CHAR)
	HD_SZSBUF(hp) = HD_NEXTCH(hp)

	# Return any unused module descriptors.
	HD_LENHD(hp) = HD_LENHD(hp) -
	    LEN_MODSTRUCT * (HD_MAXMODULES(hp) - HD_NMODULES(hp))
	HD_MAXMODULES(hp) = HD_NMODULES(hp)
	call realloc (hp, HD_LENHD(hp), TY_STRUCT)

	call sfree (sp)
	return (hp)
end


# HD_CLOSE -- Close the helpdir descriptor.  The helpdir file has already
# been closed; all we need do is return the string buffer and the helpdir
# descriptor structure.

procedure hd_close (hp)

pointer	hp

begin
	if (hp != NULL) {
	    call mfree (HD_SBUF(hp), TY_CHAR)
	    call mfree (hp, TY_STRUCT)
	}
end


# HD_PUTLDIRY -- Decode a logical directory declaration and store it away
# in the descriptor.  We are passed the declaration minus the leading
# dollar sign.  Format: "variable = string".

procedure hd_putldiry (hp, decl)

pointer	hp
char	decl[ARB]

int	ip, nldir, strp
pointer	sp, buf, op
char	hd_getc()
int	strncmp(), hd_putstr()
errchk	salloc, hd_getc, hd_putstr

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Do nothing if null declaration.
	for (ip=1;  IS_WHITE(decl[ip]);  ip=ip+1)
	    ;
	if (decl[ip] == '\n' || decl[ip] == EOS || decl[ip] == '#') {
	    call sfree (sp)
	    return
	}

	# Extract "ldir=string", eliminating all whitespace, and deleting
	# the newline at the end of the statement.  Quotes around the string
	# are optional and are deleted in hl_getc.

	for (op=buf;  hd_getc (decl, ip, Memc[op]) != EOS;  op=op+1)
	    if (Memc[op] == '\n')
		op = op - 1

	# Deposit the "ldir=string" in the string buffer, and set the
	# appropriate pointers for either defdir or a new ldir.

	strp = hd_putstr (hp, Memc[buf])
	if (strncmp (Memc[buf], "defdir", 6) == 0)
	    HD_DEFDIR(hp) = strp
	else {
	    nldir = HD_NLDIRS(hp) + 1
	    if (nldir > MAX_LDIRS)
		call error (8, "Too many ldir declarations in helpdir")
	    HD_NLDIRS(hp) = nldir
	    HD_LDIR(hp,nldir) = strp
	}

	call sfree (sp)
end


# HD_PUTMODULE -- Put a module declaration into the helpdir descriptor.
# Start new entry; save module name; process file names.  If line ends
# in a comma, get a new line.  Print warning if an unknown file type
# keyword is encountered.

procedure hd_putmodule (hp, fd, lbuf)

pointer	hp
int	fd
char	lbuf[ARB]

char	ch
int	ip, junk, m, ftype, strp
pointer	sp, buf, op, sbuf, mp

bool	streq()
char	hd_getc(), hd_peekc()
int	getline(), ctowrd(), hd_putstr()
errchk	salloc, getline, hd_putstr, hd_getc

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	sbuf = HD_SBUF(hp)

	# Fetch module name.  Cannot be null or line is blank and we would
	# not have been called.
	ip = 1
	junk = ctowrd (lbuf, ip, Memc[buf], SZ_LINE)

	# Check if this is a redefinition of a module already defined.
	# If so, overwrite descriptor of earlier module, else allocate
	# a new descriptor.

	for (m=1;  m <= HD_NMODULES(hp);  m=m+1) {
	    mp = HD_MODULE(hp,m)
	    if (streq (Memc[buf], Memc[sbuf+M_NAME(mp)]))
		break
	}

	if (m > HD_NMODULES(hp)) {
	    # If we are out of space for modules, increase the descriptor
	    # structure size to allow more module descriptors.
	    if (m > HD_MAXMODULES(hp)) {
		HD_LENHD(hp) = HD_LENHD(hp) + (INC_MODULES * LEN_MODSTRUCT)
		call realloc (hp, HD_LENHD(hp), TY_STRUCT)
		HD_MAXMODULES(hp) = HD_MAXMODULES(hp) + INC_MODULES
	    }
	    HD_NMODULES(hp) = m
	}

	mp = HD_MODULE(hp,m)
	call aclri (Memi[mp], LEN_MODSTRUCT)

	# Put module name in string buffer and save index of string in descr.
	M_NAME(mp) = hd_putstr (hp, Memc[buf])

	# Process file name fields, if any.  Unrecognized file type keywords
	# cause a warning to be issued.  Redundant entires overwrite old
	# entries.  Order makes no diff, absence is ok.

	op = buf
	ftype = TY_UNKNOWN
	strp = 0

	repeat {
	    ch = hd_getc (lbuf, ip, ch)

	    switch (ch) {
	    case '=':
		# Buffer contains the code word for the file being set.
		Memc[op] = EOS
		call strlwr (Memc[buf])
		if (     streq (Memc[buf], "hlp"))
		    ftype = TY_HLP
		else if (streq (Memc[buf], "sys"))
		    ftype = TY_SYS
		else if (streq (Memc[buf], "src"))
		    ftype = TY_SRC
		else if (streq (Memc[buf], "pkg"))
		    ftype = TY_PKG
		else if (streq (Memc[buf], "men"))
		    ftype = TY_MEN
		else {
		    ftype = TY_UNKNOWN
		    call eprintf ("Warning: bad file type `%s' in helpdir\n")
			call pargstr (Memc[buf])
		}
		op = buf

	    case ',', '\n', EOS:
		# Buffer contains the file name string.  Put it in the string
		# buffer and save pointer in appropriate field of module
		# descriptor.

		Memc[op] = EOS

		# If filename is "..", i.e., "sys=..", the filename is identical
		# to that last specified.  Use the file name pointer.

		if (streq (Memc[buf], "..")) {
		    if (strp == 0)
			call error (9, "helpdir: `..' reference, no prev file")
		} else
		    strp = hd_putstr (hp, Memc[buf])

		switch (ftype) {
		case TY_HLP:
		    M_HLP(mp) = strp
		case TY_SYS:
		    M_SYS(mp) = strp
		case TY_SRC:
		    M_SRC(mp) = strp
		case TY_PKG:
		    M_PKG(mp) = strp
		case TY_MEN:
		    M_MEN(mp) = strp
		}
		op = buf

		if (ch == '\n' || ch == EOS)
		    break				# end of statement
		else {
		    # Check for continuation on next line.  Read a new line into
		    # the buffer if end of line follows comma.

		    if (hd_peekc(lbuf,ip) == '\n' || hd_peekc(lbuf,ip) == EOS) {
			if (getline (fd, lbuf) == EOF) {
			    call eprintf ("Unexpected EOF in helpdir file\n")
			    call sfree (sp)
			    return
			}
			ip = 1
		    }
		}

	    default:
		# Regular character.  Deposit in buffer.
		if (op >= buf+SZ_LINE)
		    call error (10, "helpdir: buffer overflow reading modspec")
		Memc[op] = ch
		op = op + 1
	    }
	}

	call sfree (sp)
end


# HD_GETC -- Get next nonwhite character from the input line.  Leave pointer
# pointing to next character.  Ignore quotes, so that file name strings can
# be quoted without penalty.

char procedure hd_getc (lbuf, ip, ch)

char	lbuf[ARB]
int	ip
char	ch
char	hd_peekc()

begin
	ch = hd_peekc (lbuf, ip)
	if (ch != EOS)
	    ip = ip + 1
	
	return (ch)
end


# HD_PEEKC -- Peek at the next nonwhite character on a line, but do not
# advance the input pointer past the character.  Ignore quote characters
# and comments.

char procedure hd_peekc (lbuf, ip)

char	lbuf[ARB]
int	ip
char	ch

begin
	for (ch=lbuf[ip];  ch != EOS;  ch=lbuf[ip])
	    if (IS_WHITE(ch) || ch == '\'' || ch == '"') {
		ip = ip + 1
	    } else if (ch == '#') {
		while (lbuf[ip] != '\n' && lbuf[ip] != EOS)
		    ip = ip + 1
	    } else
		break
	
	return (ch)
end


# HD_PUTSTR -- Put a string (incl EOS) in the string buffer at nextch.
# If there is not enough space in the buffer, reallocate a larger buffer.
# Return the index of the string in the string buffer.

int procedure hd_putstr (hp, str)

pointer	hp
char	str[ARB]
int	nextch, nchars, strlen()
errchk	realloc

begin
	# Null strings are not stored and cause a null index to be returned.
	nchars = strlen (str)
	if (nchars == 0)
	    return (0)

	nextch = HD_NEXTCH(hp)
	if (nextch + nchars + 1 > HD_SZSBUF(hp)) {
	    HD_SZSBUF(hp) = HD_SZSBUF(hp) + INC_SZSBUF
	    call realloc (HD_SBUF(hp), HD_SZSBUF(hp), TY_CHAR)
	}

	call strcpy (str, Memc[HD_SBUF(hp) + nextch], ARB)
	HD_NEXTCH(hp) = nextch + nchars + 1

	return (nextch)
end


# HD_SORT_MODULES -- Sort the module list alphabetically by name.
# A simple exchange sort is ok because the sort time is negligible
# compared to all the file accesses, Lroff etc.

procedure hd_sort_modules (hp)

pointer	hp

bool	sorted
int	nmodules, m, mlen, i, temp
pointer	sbuf, mp1, mp2
bool	strgt()

begin
	nmodules = HD_NMODULES(hp)
	sbuf = HD_SBUF(hp)
	mlen = LEN_MODSTRUCT
	if (nmodules < 2)
	    return

	repeat {
	    sorted = true
	    do m = 1, nmodules-1 {
		mp1 = HD_MODULE(hp,m)
		mp2 = mp1 + mlen
		if (strgt (Memc[sbuf+M_NAME(mp1)], Memc[sbuf+M_NAME(mp2)])) {
		    do i = 0, mlen-1 {
			temp = Memi[mp1+i]
			Memi[mp1+i] = Memi[mp2+i]
			Memi[mp2+i] = temp
		    }
		    sorted = false
		}
	    }
	} until (sorted)
end


# HD_FINDMOD -- Search for the named module and return the module number
# if found.  Abbreviations are permitted.  An ambiguous abbreviation is
# an error.

int procedure hd_findmod (hp, modname)

pointer	hp
char	modname[ARB]
int	m, namelen, module
pointer	mp, sbuf
int	strlen(), strncmp()

begin
	namelen = strlen (modname)
	if (namelen == 0)
	    return (0)
	module = 0
	sbuf = HD_SBUF(hp)

	for (m=1;  m <= HD_NMODULES(hp);  m=m+1) {
	    mp = HD_MODULE(hp,m)

	    if (strncmp (Memc[sbuf+M_NAME(mp)], modname, namelen) == 0) {
		if (strlen (Memc[sbuf+M_NAME(mp)]) == namelen) {
		    return (m)				# exact match
		} else if (module != 0) {
		    call eprintf ("\n--> %s <--\n")
			call pargstr (modname)
		    call error (11, "Ambiguous module name abbreviation")
		} else
		    module = m
	    }
	}

	return (module)
end


# HD_GETNAME -- Get the module name or a filename.  If a filename is requested,
# check if filename contains a logical directory reference.  If yes, try to
# satisfy the reference from the local list of ldirs (but no recursive refs
# permitted here).  If no, prepend the defdir string.

define	nullstr_	91
define	nodefdir_	92

int procedure hd_getname (hp, m, field, outstr, maxch)

pointer	hp
int	m				# module number
int	field				# field code
char	outstr[ARB]
int	maxch

int	len_ldir, op
pointer	mp, sp, ldir, sbuf, fname_ptr, ip, subdir
int	strncmp(), gstrcpy(), hd_getldir(), fnldir()
errchk	salloc, hd_getldir

begin
	call smark (sp)
	call salloc (ldir, SZ_PATHNAME, TY_CHAR)
	call salloc (subdir, SZ_FNAME, TY_CHAR)

	if (hp == NULL)
	    call error (12, "hd_getname: bad helpdir descriptor")

	if (m < 1 || m > HD_NMODULES(hp)) {
nullstr_    call sfree (sp)
	    outstr[1] = EOS
	    return (0)
	}

	mp = HD_MODULE(hp,m)
	sbuf = HD_SBUF(hp)

	switch (field) {
	case TY_MODNAME:
	    call sfree (sp)
	    return (gstrcpy (Memc[sbuf+M_NAME(mp)], outstr, maxch))
	case TY_HLP:
	    fname_ptr = M_HLP(mp)
	case TY_SYS:
	    fname_ptr = M_SYS(mp)
	case TY_SRC:
	    fname_ptr = M_SRC(mp)
	case TY_PKG:
	    fname_ptr = M_PKG(mp)
	case TY_MEN:
	    fname_ptr = M_MEN(mp)
	default:
	    goto nullstr_
	}

	# If index is zero, no filename was given.
	if (fname_ptr == 0)
	    goto nullstr_

	# Get ldir substring, if any, from filename.  If no ldir, prepend
	# defdir and quit.  Otherwise lookup ldir in local list.  If found,
	# prepend value.  Otherwise the ldir is a CL global one, and return
	# filename without modification.  If the given ldir string begins
	# with "./", substitute the value of defdir for the ".".

	len_ldir = fnldir (Memc[sbuf+fname_ptr], Memc[ldir], SZ_PATHNAME)

	if (len_ldir == 0) {
	    if (HD_DEFDIR(hp) == 0)
		goto nodefdir_
	    for (ip = sbuf + HD_DEFDIR(hp);  Memc[ip] != '=';  ip=ip+1)
		;
	    op = gstrcpy (Memc[ip+1], outstr, maxch) + 1
	    ip = sbuf + fname_ptr
	} else if (hd_getldir (hp, Memc[ldir], Memc[subdir], SZ_FNAME) == 0) {
	    op = 1
	    ip = sbuf + fname_ptr
	} else {
	    if (strncmp (Memc[subdir], "./", 2) == 0) {
		if (HD_DEFDIR(hp) == 0)
nodefdir_	    call error (13, "Default directory omitted in helpdir")
		for (ip = sbuf + HD_DEFDIR(hp);  Memc[ip] != '=';  ip=ip+1)
		    ;
		op = gstrcpy (Memc[ip+1], outstr, maxch) + 1
		ip = subdir + 2
	    } else {
		op = 1
		ip = subdir
	    }
	    op = op + gstrcpy (Memc[ip], outstr[op], maxch - op + 1)
	    ip = sbuf + fname_ptr + len_ldir
	}

	call sfree (sp)
	return (gstrcpy (Memc[ip], outstr[op], maxch - op + 1))
end


# HD_GETLDIR -- Search the logical directory list for the package helpdir
# file for the given ldir name.  Return value in output string if found.

int procedure hd_getldir (hp, ldir, outstr, maxch)

pointer	hp
char	ldir[ARB]
char	outstr[ARB]
int	maxch

int	i
pointer	sp, ip, op, sbuf, envvar, filvar
bool	streq()
int	gstrcpy()

begin
	call smark (sp)
	call salloc (envvar, SZ_FNAME, TY_CHAR)
	call salloc (filvar, SZ_FNAME, TY_CHAR)

	sbuf = HD_SBUF(hp)

	# Ldir string has the form "ldir$".  Extract the ldir name into the
	# filvar buffer, omitting the $ delimiter.
	i = 1
	for (op=filvar;  ldir[i] != '$' && ldir[i] != EOS;  op=op+1) {
	    Memc[op] = ldir[i]
	    i = i + 1
	}
	Memc[op] = EOS

	for (i=1;  i <= HD_NLDIRS(hp);  i=i+1) {
	    # Extract the helpdir env variable name into the envvar buffer,
	    # stopping when the '=' is reached.

	    ip = sbuf + HD_LDIR(hp,i)
	    for (op=envvar;  Memc[ip] != '=';  op=op+1) {
		if (Memc[ip] == EOS)
		    call error (14, "helpdir: missing '=' in ldir declaration")
		Memc[op] = Memc[ip]
		ip = ip + 1
	    }
	    Memc[op] = EOS

	    # Return whatever follows the '=' if we have a match.  The input
	    # pointer is left pointing at the '='.

	    if (streq (Memc[filvar], Memc[envvar])) {
		call sfree (sp)
		return (gstrcpy (Memc[ip+1], outstr, maxch))
	    }
	}

	outstr[1] = EOS
	call sfree (sp)
	return (0)
end
