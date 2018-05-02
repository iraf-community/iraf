include	<syserr.h>
include	<error.h>
include	"help.h"
include	"helpdir.h"

define	MAX_DEPTH	20		# max nesting of packages


# T_MKAPROPOS -- Make the apropos database.

procedure t_mkapopos()

bool	verbose
pointer	sp, helpdir, hlpdir, aproposdb, tempname, pkglist
int 	ic				# list counter for word_fetch 
int 	fc				# File counter
int	access()
bool	clgetb()
int	word_fetch()

begin
	call smark (sp)
	call salloc (pkglist, SZ_PATHNAME + 1, TY_CHAR)
	call salloc (hlpdir, SZ_FNAME + 1, TY_CHAR)
	call salloc (helpdir, SZ_FNAME + 1, TY_CHAR)
	call salloc (aproposdb, SZ_FNAME + 1, TY_CHAR)
	call salloc (tempname, SZ_FNAME + 1, TY_CHAR)

	# Fetch the names of the packages 
	# and the new database file.
	call clgstr ("pkglist", Memc[pkglist], SZ_PATHNAME)
	call clgstr ("helpdir", Memc[hlpdir], SZ_FNAME)
	call clgstr ("aproposdb", Memc[aproposdb], SZ_FNAME)

	# Get verbose flag and echo parameters
	verbose = clgetb ("verbose")
	if (verbose) {
	    call printf ("helpdir = %s\n")
		call pargstr (Memc[helpdir])
	    call printf ("aproposdb = %s\n")
		call pargstr (Memc[aproposdb])
	    call flush (STDOUT)
	}

	# The database is compiled into a temporary file to protect the current
	# database in the event the compile is aborted.  When the compile has
	# successfully completed we will delete the original and replace it
	# with the new database.  This also has the advantage that the switch
	# takes very little time, making it less likely that anyone will notice
	# when the database is updated.

	call mktemp ("tmp$hdb", Memc[tempname], SZ_FNAME)

	# Now, for each package on the list, build the file name for the root
	# helpdb and Compile the help strings

	ic=1	
	fc=1
	while(word_fetch (Memc[pkglist], ic, Memc[helpdir], SZ_PATHNAME) > 0 ) {
	    # Append the help directory to the package name
	    call strcat ("$", Memc[helpdir], SZ_FNAME)
	    call strcat (Memc[hlpdir], Memc[helpdir], SZ_FNAME)
	    # Perform the compilation.
	    call app_compile (Memc[helpdir], Memc[tempname], fc, verbose)
	}

	# Now attempt to delete the old database and replace it with the new.
	# If the old file cannot be deleted we delete the new database and
	# abort.

	    if (access (Memc[aproposdb], 0, 0) == YES) {
	         iferr (call delete (Memc[aproposdb])) {
	            call delete (Memc[tempname])
	            call erract (EA_ERROR)
	        }
	    }

	call rename (Memc[tempname], Memc[aproposdb])
	call sfree (sp)
end


# APP_COMPILE - Compile the help directory file, and generate the apropos
# databse.

procedure app_compile (root_helpdir_file, app_db, fc, verbose)

char	root_helpdir_file[ARB]	# name of root help directory file
char	app_db[ARB]		# output apropos database name
bool	verbose			# print notes on structure of database

bool	found_a_subpackage
pointer	hp_stk[MAX_DEPTH]	# help directory pointer stack
int	pk_stk[MAX_DEPTH]	# subpackage index stack
char	fname[SZ_FNAME]		# helpdir filename
int	fd, sp, pk, fc
pointer	hp, modname

int	hd_getname()
int	open(), access()
pointer	hd_open()
errchk	hd_getname

begin
	# Initialize the stacks and current file name
	# processed, i.e., the root directory file.
	sp = 0
	pk = 1
	call strcpy (root_helpdir_file, fname, SZ_FNAME)

	# Open root help file
	iferr (hp = hd_open (root_helpdir_file)) {
		call printf ("file: %s")
		call pargstr (root_helpdir_file) 
		call error (0, "cannot open root help directory file")
	}

	# Open output file name
	    if (fc == 1) {	
	    iferr (fd = open (app_db, NEW_FILE, TEXT_FILE))
	        call error (0, "cannot open apropos database file")
	    fc=2
	    } else {
	        iferr (fd = open (app_db, APPEND, TEXT_FILE))
	        call error (0, "cannot open apropos database file")
	    }
	# We enter the compile loop ready to scan the next help directory file,
	# which has been pushed onto the top of the stack.  The help directory
	# file is first entered into the database, then we scan the directory
	# until a subdirectory is found.  If a subdirectory is found it is
	# opened and pushed onto the stack and the process repeats (hd_open
	# does not leave the physical file open so running out of file
	# descriptors is not a problem).  When the end of a directory is
	# reached the stack is popped, closing the current directory, and 
	# we continue to scan the previous directory until another subdirectory
	# is found.

	repeat {
	    call printf ("%3d %15s (%s): %d help modules\n")
		call pargi (sp + 1)
		if (HD_PAKNAME (hp) != NULL)
		    call pargstr (Memc[HD_SBUF (hp) + HD_PAKNAME (hp)])
		else
		    call pargstr ("")
		call pargstr (fname)
		call pargi (HD_NMODULES (hp))
	    call flush (STDOUT)

	    # Now scan the directory for subdirectories.  If one is found,
	    # open it and push it on the stack, otherwise pop the stack and
	    # resume scanning the previous directory.

	    repeat {
		# Search for a module which is a subpackage.
		found_a_subpackage = false
		for (;  pk <= HD_NMODULES(hp);  pk=pk+1) {
		    if (verbose) {
			modname = M_NAME(HD_MODULE(hp,pk))
			call printf ("\t\t[%d.%02d] %s\n")
			    call pargi (sp + 1)
			    call pargi (pk)
			    call pargstr (Memc[HD_SBUF(hp) + modname])
		    }
		    if (hd_getname (hp, pk, TY_PKG, fname, SZ_FNAME) > 0) {
			found_a_subpackage = true
			break
		    } 
		}

		if (found_a_subpackage) {
		    if (access (fname, READ_ONLY, TEXT_FILE) == NO) {
			call printf ("\t\t%4w(cannot access '%s')\n")
			    call pargstr (fname)
			# ...and continue searching the current helpdir
			pk = pk + 1

		    } else {
			# Got one; push it on the stack.  Bump PK so that we
			# resume scanning the package with the module following
			# the subpackage.
			sp = sp + 1
			if (sp > MAX_DEPTH)
			    call fatal (3, "packages nested too deeply")
			hp_stk[sp] = hp
			pk_stk[sp] = pk + 1
			iferr (call app_men (fd, hp_stk, pk_stk, sp))
			    call erract (EA_WARN)
			iferr (hp = hd_open (fname)) {
			    hp = hp_stk[sp]
			    sp = sp - 1
			    call printf ("cannot open helpdir '%s'\n")
				call pargstr (fname)
			    next
			}
			pk = 1
			break			# go process new helpdir
		    }

		} else {
		    # Helpdir has been exhausted.  Close it and pop the
		    # stack, continue scanning on the previous helpdir.

		    call hd_close (hp)
		    if (sp > 0) {
			hp = hp_stk[sp]
			pk = pk_stk[sp]
			sp = sp - 1
			if (verbose)
			    call printf ("\t\t\t[end of package]\n")
		    } else {
			call close (fd)
			return			# ALL DONE
		    }
		}
		call flush (STDOUT)
	    }
	}
end


# APP_MEN - Write a menu file in a proper format to the output file.

procedure app_men (ofd, hp_stk, pk_stk, sp)

int	ofd			# output file descritor
int	hp_stk[ARB]		# help descriptors stack
int	pk_stk[ARB]		# current package number
int	sp			# stack pointer

char	fname[SZ_FNAME]
char	pkname[SZ_FNAME]
char	pkpath[SZ_LINE]
char	line[SZ_LINE]
char	name[SZ_LINE]
char	text[SZ_LINE]
int	fd			# output file descriptor
int	i, ip, pk, pkindex
pointer	hp

bool	streq(), strne()
int	open(), fscan()
int	hd_getname()
int	stridx(), stridxs(), strext()
errchk	open(), fscan(), hd_getname()

begin
	# Get help structure and package
	# index from top of stack
	hp = hp_stk[sp]
	pk = pk_stk[sp] - 1

	# Get name of menu file for the package
	if (hd_getname (hp, pk, TY_MEN, fname, SZ_FNAME) > 0) {

	    # Build package path
	    call strcpy ("", pkpath, SZ_LINE)
	    do i = 1, sp {

		# Get package name
		hp = hp_stk[i]
		pk = pk_stk[i] - 1
		pkindex = M_NAME (HD_MODULE( hp, pk))
		call strcpy (Memc[HD_SBUF (hp) + pkindex], pkname, SZ_LINE)

		# Throw away the first component if it begins
		# with an underscore.
		if (i == 1 && stridx ("_", pkname) == 1)
		    next

		# Compress the name "clpackage" to "cl" to make
		# it more familiar to the user.
		if (streq (pkname, "clpackage"))
		    call strcpy ("cl", pkname, SZ_LINE)

		# Concatenate the package name delimited with
		# a colon, if necessary. 
		if (strne (pkpath, ""))
		    call strcat (".", pkpath, SZ_LINE)
		call strcat (pkname, pkpath, SZ_LINE)
	    }

	    # Try to open menu file
	    fd = open (fname, READ_ONLY, TEXT_FILE)
	    while (fscan (fd) != EOF) {

		# Read line from file and split it
		# into components
		call gargstr (line, SZ_LINE)
		if (stridxs (line, "-*") > 0) {
		    ip = 1
		    if (strext (line, ip, " -*", YES, name, SZ_LINE) == 0)
			next
		    if (strext (line, ip, "", YES, text, SZ_LINE) == 0)
			next
		} else
		    next
		    
	        call fprintf (ofd, "%10s - %s (%s)\n")
		    call pargstr (name)
		    call pargstr (text)
		    call pargstr (pkpath)
	    }

	    # Close input file
	    call close (fd)
	}
end
