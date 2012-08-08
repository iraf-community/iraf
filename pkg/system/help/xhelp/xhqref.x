# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<finfo.h>
include	<ctype.h>
include	"../help.h"
include	"xhelp.h"

define	MAX_PKGS	200
define	SZ_PKG		32
define	EXTPKG		"hlib$extern.pkg"


# XH_UPDT_QUICKREF -- Update a quick reference file for the help database
# if needed.

procedure xh_updt_quickref (xh)

pointer	xh

long	fiq[LEN_FINFO], fie[LEN_FINFO]

int	access()
long	finfo()
errchk	access, finfo

begin
	# If the quickref file exists, and it is older than the
	# hlib$extern.pkg file which defines the helpdb rebuild
	# the quickref file so we're current, otherwise rebuild 
	# the quickref file anyway.
	if ((access (QUICKREF(xh), 0, 0) == NO) ||
	    (access (PKGFILE, 0, 0) == NO)) {
		call xh_make_quickref (xh, QUICKREF(xh))
	} else {
            if (finfo (QUICKREF(xh), fiq) == ERR)
            	call filerr (QUICKREF(xh), SYS_FOPNNEXFIL)
            if (finfo (EXTPKG, fie) == ERR)
            	call filerr (EXTPKG, SYS_FOPNNEXFIL)

	    if (FI_MTIME[fie] > FI_MTIME[fiq]) {
	        call delete (QUICKREF(xh))
	        call xh_make_quickref (xh, QUICKREF(xh))
	    }
	}
end


# XH_MAKE_QUICKREF -- Make a quick reference file for the help database.

procedure xh_make_quickref (xh, quickref)

pointer	xh					#i task struct pointer
char	quickref[ARB]				#i quickref filename

pointer	pkglist, pp
int	fdi, fdo, fd_err
int	sz_pbuf, npkgs
char	fname[SZ_FNAME], efname[SZ_FNAME]
char	pkg[SZ_PKG], lastpkg[SZ_PKG], line[SZ_LINE]

int	open(), access(), getline(), gstrcpy(), strsearch(), strcmp()
errchk	open

begin
	# Tell the GUI we're actually busy doing something....
	call gmsg (XH_GP(xh), "alert", 
	    "Please wait,\ngenerating QuickRef file...")
	call gflush (XH_GP(xh))

	# Open the STDERR stream onto a dummy file to catch any
	# "no help available" messages that may interfere with the
	# textout GUI parameter.
	call mktemp ("tmp$xerr", efname, SZ_FNAME)
	fd_err = open (efname, NEW_FILE, TEXT_FILE)
	call frediro (STDERR, fd_err)

        # Open a temp file with the raw search information.
        call mktemp ("tmp$xhelpq", fname, SZ_FNAME)
        fdi = open (fname, NEW_FILE, TEXT_FILE)

        call xh_get_help (fdi, "[a-z]*.", "", "", HF_HTML, HELPDB(xh), "help",
	    "references")
        call close (fdi)

	# Close the error file, swap back the descriptors first and delete it.
	call fswapfd (STDERR, fd_err)
	call close (fd_err)
	call delete (efname)

	# Delete existing files.
	if (access (quickref, 0, 0) == YES)
	    call delete (quickref)

	# Open the references file.
	iferr (fdo = open (quickref, NEW_FILE, TEXT_FILE)) {
	    call sprintf (line, SZ_LINE, "Cannot create quickref file `%s'.")
     		call pargstr (quickref)
	    call error (0, line)
	}
        fdi = open (fname, READ_ONLY, TEXT_FILE)

	# Initialize for the package list.
	pkg[1] = EOS
	lastpkg[1] = EOS
	npkgs = 0
	sz_pbuf = MAX_PKGS * SZ_PKG
	call malloc (pkglist, sz_pbuf, TY_CHAR)
	pp = pkglist

	# Loop over the lines in the file, save the ones that are the
	# descriptions.  Descriptions are assumed to contain a '-' since
	# this is standard for .men files, to be safe we also required 
	# the bracketed package name generated with a 'references' query.
	# The package name is used to create the package list symtab we
	# use to show the item as a task or package.

	while (getline(fdi,line) != EOF) {
	    if (strsearch (line, " - ") != 0 && strsearch (line, "[") != 0) {
		call putline (fdo, line)

		# Extract the package name.  The results aren't sorted at
		# this point so we see whether this package is the same as
		# the last before adding it to the package list.  This 
		# gives us a unique list of unsorted package names.

		call xh_gname (line, pkg)
		if (lastpkg[1] == EOS)
		    call strcpy (pkg, lastpkg, SZ_PKG)
		if (strcmp (pkg, lastpkg) != 0) {
		    npkgs = npkgs + 1

		    # Protect against overflowing the buffer.
		    if ((pp - pkglist) > sz_pbuf)
			call error (1, "Package buffer overflow.")

		    pp = pp + gstrcpy (pkg, Memc[pp], SZ_PKG)
		    pp = pp + gstrcpy (" ", Memc[pp], SZ_PKG)
		    call strcpy (pkg, lastpkg, SZ_PKG)
		}
	    }
	}
	Memc[pp] = EOS

	# Close the new references file.
	call close (fdo)

	# Close and delete the references temp files.
	call close (fdi)
	call delete (fname)

	# Sort the lines of the references file.
	call xh_file_sort (quickref)

	# Now send the package list off to be stored for the next time.
	call xh_make_pkglist (Memc[pkglist])

	call mfree (pkglist, TY_CHAR)

	# Tell the GUI we're done.
	call gmsg (XH_GP(xh), "alert", "dismiss")
	call gflush (XH_GP(xh))
end


# XH_MAKE_PKGLIST -- Update the package symtab file.  We are only called
# when updating the quickref database so delete any existing file before
# writing the new one.

procedure xh_make_pkglist (list)

char	list[ARB]				#i package list

pointer	sp, err, pkg
pointer	stp, sym
int	i, ip, fd

pointer	stopen(), stenter()
int	open(), access(), strlen()
errchk	open

begin
	call smark (sp)
	call salloc (err, SZ_LINE, TY_CHAR)
	call salloc (pkg, SZ_FNAME, TY_CHAR)

	# Delete existing files.
	if (access(PKGFILE,0,0) == YES)	
	    call delete (PKGFILE)

	# Open the package symtab file.
	iferr (fd = open (PKGFILE, NEW_FILE, BINARY_FILE)) {
	    call sprintf (Memc[err], SZ_LINE, "Can't create pkg symtab `%s'.")
     		call pargstr (PKGFILE)
	    call error (0, Memc[err])
	}

	# Sort the list before making the symtab.
	call xh_sort_list (list)

	# Open the symtab.
	stp = stopen ("package list", LEN_INDEX, LEN_STAB, SZ_SBUF)

	# Enter strings in the symtab.
	i = 0
	for (ip=1; list[ip] != EOS; ip=ip+1) {
	    if (list[ip] == ' ') {
	    	Memc[pkg+i] = EOS
		sym = stenter (stp, Memc[pkg], strlen (Memc[pkg]) + 1)
		i = 0
		ip = ip + 1
	    }
	    Memc[pkg+i] = list[ip]
	    i = i + 1
	}

	# Save the symtab file.
	call stsqueeze (stp)
	call stsave (stp, fd)

	# Clean up.
	call stclose (stp)
	call close (fd)
	call sfree (sp)
end


# XH_GNAME -- Extract the package name from a 'references' line.

procedure xh_gname (line, pkg)

char	line[ARB]				#i references line
char	pkg[ARB]				#o package name in line

int	ip, strlen()

begin
	# Clear trailing whitespace back to the ']' at end of the 
	# package name.
	for (ip=strlen(line); line[ip] != ']' || IS_WHITE(line[ip]); )
	    ip = ip - 1
	line[ip] = EOS

	# Move back to starting '[' of package name.
	while (line[ip] != '[' && ip > 0)
	    ip = ip - 1

	# What's left is the package name, copy it to the output.
	call strcpy (line[ip+1], pkg, SZ_PKG)
end
