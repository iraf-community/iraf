# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<error.h>
include	<finfo.h>
include	<fset.h>
include	<time.h>
include	<mii.h>
include	"help.h"
include	"helpdir.h"

.help helpdb
.nf ___________________________________________________________________________
HELPDB -- Code to manage the help database.  The help database is a set of
compiled package help directories.  Each help directory file is compiled with
HD_OPEN and the resultant structure is saved in the database, indexed by the
name of the directory file.  At run time, instead of opening and interpreting
multiple help directory files to expand templates, we merely read in the
compiled database and search the index for the entry for the named package,
returning a pointer to the compiled help directory structure for the package.
The help database must be recompiled with HDB_COMPILE whenever the system help
directory or a package help directory is modified.

		hdb_compile (root_helpdir_filename, database_filename)

	db =	   hdb_open (database)
		  hdb_close (db)
	hp =	   hdb_load (db, help_directory_file)
		   hdb_free (db, hp)

There is no provision for updating the database; the whole thing must be
recompiled with HDB_COMPILE.  At HELP time the root help directory is opened
with HDB_OPEN, naming either a raw root help directory file or a compiled
help database file.  Normally a precompiled help database is used, but if
desired the help directory files will be accessed directly.  The directory
for an individual package is accessed with HDB_LOAD, which is functionally
equivalent to HD_OPEN (helpdir.x).  HDB_FREE is functionally equivalent to
HD_CLOSE.

Currently the entire database is read into memory to speed up database
searches.  In the future this implementation should be changed to use the
DBIO faciltities if the database becomes large.
.endhelp ______________________________________________________________________

define	DATA_OFFSET	513		# offset to data area, chars
define	MAX_ENTRIES	100		# initial max db entries
define	INC_ENTRIES	50		# increment if overflow
define	MAX_DEPTH	20		# max nesting of packages
define	MAX_MENUSIZE	500		# max modules in a table
define	MAX_NAMELEN	20		# max chars in a module name in table
define	FIRST_COL	6		# indentation of tables

# Help database header structure.  Stored at the beginning of a help
# database file.

define	LEN_HDBHEADER	14
define	HDB_MAGICVAL	110104B

define	HDB_MAGIC	Memi[$1]	# helpdb file type code
define	HDB_RAW		Memi[$1+1]	# access compiled or raw database
define	HDB_RHD		Memi[$1+2]	# if raw, HP of root help directory
define	HDB_INDEX	Memi[$1+3]	# index of root help directory
define	HDB_CRDATE	Meml[$1+4]	# creation date
define	HDB_NENTRIES	Memi[$1+5]	# number of help directories in db
define	HDB_MAXENTRIES	Memi[$1+6]	# maximum no. of help directories in db
define	HDB_NMODULES	Memi[$1+7]	# count of the total number of modules
define	HDB_INDEXOFFSET	Meml[$1+8]	# file offset of index, chars
define	HDB_INDEXPTR	Memi[$1+9]	# pointer to loaded index, ty_struct
define	HDB_INDEXLEN	Memi[$1+10]	# length of index structure, su
define	HDB_DATAOFFSET	Meml[$1+11]	# file offset of data area, chars
define	HDB_DATAPTR	Memi[$1+12]	# pointer to loaded data area, ty_struct
define	HDB_DATALEN	Memi[$1+13]	# length of data area, struct units

# Index structure.  Identifies the contents of the database and tells where
# they are stored.  There is one index entry for each help directory, i.e.,
# for each package.

define	LEN_HDBINDEX	34
define	SZ_DBIKEY	63
define	LEN_DBIDATA	2

define	DBI_KEY		Memc[P2C($1)]	# entry name
define	DBI_OFFSET	Memi[$1+32]	# offset of entry into data area, su
define	DBI_MTIME	Meml[$1+33]	# modification date of entry


# HDB_COMPILE -- Compile a set of help directories to produce a new help
# database.  The root help directory is read first and entered into
# the database, followed by each subdirectory named in the root help
# directory.

procedure hdb_compile (root_helpdir_file, helpdb_file, verbose)

char	root_helpdir_file[ARB]	# name of root help directory file
char	helpdb_file[ARB]	# name of new database file
bool	verbose			# print informative messages

pointer	db, index, p1, p2, ix
bool	no_entries_interchanged
int	fd, temp[LEN_HDBINDEX], i

int	open()
bool	strgt(), streq()
long	clktime(), note(), fstatl()
errchk	open, note, seek, hdb_compile_rhd, mii_writec, mii_writei

begin
	# Open the output database file.
	fd = open (helpdb_file, NEW_FILE, BINARY_FILE)

	# Allocate and initialize the database header and index structures.
	# The data area begins at a fixed offset and will be written out
	# as we go.  The index and db header are written out after all
	# help directories have been processed.

	call calloc (db, LEN_HDBHEADER, TY_STRUCT)
	call calloc (index, MAX_ENTRIES * LEN_HDBINDEX, TY_STRUCT)

	HDB_MAGIC(db)      = HDB_MAGICVAL
	HDB_MAXENTRIES(db) = MAX_ENTRIES
	HDB_CRDATE(db)     = clktime (long(0))

	# Write zeros into the header area of the file, so that the next
	# sequential write will place the first data record at the offset
	# DATA_OFFSET.  We assume that the empty index is larger than the
	# header area.

	call write (fd, Memi[index], DATA_OFFSET - 1)
	HDB_DATAOFFSET(db) = note (fd)

	# Compile root directory followed by all subdirectories.
	# The root directory is expanded into a set of package help
	# directories which are compiled and written into the data
	# area of the database.  The database descriptor and index
	# structures are returned.

	call hdb_compile_rhd (fd, root_helpdir_file, db, index, verbose)

	# Sort the index and append it to the database file.  A simple
	# interchange sort is sufficient here; we have assumed that the
	# database is small in this implementation of helpdb.

	if (HDB_NENTRIES(db) > 1)
	    repeat {
		no_entries_interchanged = true
		do i = 1, HDB_NENTRIES(db) - 1 {
		    p1 = index + (i - 1) * LEN_HDBINDEX
		    p2 = p1 + LEN_HDBINDEX
		    if (strgt (DBI_KEY(p1), DBI_KEY(p2))) {
			call amovi (Memi[p1], temp,     LEN_HDBINDEX)
			call amovi (Memi[p2], Memi[p1], LEN_HDBINDEX)
			call amovi (temp,     Memi[p2], LEN_HDBINDEX)
			no_entries_interchanged = false
		    }
		}
	    } until (no_entries_interchanged)

	HDB_INDEXOFFSET(db) = note (fd)
	HDB_INDEXLEN(db) = HDB_NENTRIES(db) * LEN_HDBINDEX
	HDB_MAXENTRIES(db) = HDB_NENTRIES(db)

	# Get the offset into the index of the "_index" help directory
	# (package name list).

	do i = 1, HDB_NENTRIES(db) {
	    ix = index + (i - 1) * LEN_HDBINDEX
	    if (streq (DBI_KEY(ix), "_index")) {
		HDB_INDEX(db) = i
		break
	    }
	}
	if (HDB_INDEX(db) == 0)
	    call eprintf ("Help warning: cannot find _index\n")

	# Write the index structure to the database file.
	do i = 1, HDB_NENTRIES(db) {
	    ix = index + (i - 1) * LEN_HDBINDEX
	    call mii_writec (fd, DBI_KEY(ix), SZ_DBIKEY + 1)
	    call mii_writei (fd, DBI_OFFSET(ix), LEN_DBIDATA)
	}

	# Update the database file header.
	call seek (fd, BOFL)
	call mii_writei (fd, Memi[db], LEN_HDBHEADER)

	call printf ("\nTotal of %d help modules in %d packages, ")
	    call pargi (HDB_NMODULES(db))
	    call pargi (HDB_NENTRIES(db) - 1)
	call printf ("file size %d bytes\n")
	    call pargl (fstatl (fd, F_FILESIZE) * SZB_CHAR)
	    
	# All done; close files and deallocate buffers.
	call close (fd)
	call mfree (index, TY_STRUCT)
	call mfree (db, TY_STRUCT)
end


# HDB_COMPILE_RHD -- Compile the root help directory and all subdirectories
# into the database.  The package structure is hierarchical but the database
# is linear.  Subdirectories (subpackages) are recursively expanded starting
# with all packages in the root help directory.  Each subdirectory is entered
# into the database as it is found.  We end up with an inverted index keyed
# by the filename of the help directory file for each package.  Since the
# key is a filename each key is guaranteed to be unique.

procedure hdb_compile_rhd (fd, root_helpdir_file, db, index, verbose)

int	fd			# database file, written sequentially
char	root_helpdir_file[ARB]	# name of root help directory file
pointer	db			# database descriptor
pointer	index			# database index
bool	verbose			# print notes on structure of database

bool	found_a_subpackage
pointer	hp_stk[MAX_DEPTH]	# help directory pointer stack
int	pk_stk[MAX_DEPTH]	# subpackage index stack
char	fname[SZ_FNAME]		# helpdir filename

int	sp, pk, len_index
pointer	ix, hp, modname, data, sv_sbuf
long	mtime, fi[LEN_FINFO], savepos

long	note(), clktime()
int	hd_getname(), finfo()
pointer	hd_open(), hdb_make_rhd()
errchk	finfo, seek, note, mii_writei
errchk	hd_getname, hdb_make_rhd, hdb_getdata

begin
	# Initialize the stacks and open the next help directory file to be
	# processed, i.e., the root directory file.

	sp = 0
	pk = 1
	call strcpy (root_helpdir_file, fname, SZ_FNAME)

	iferr (hp = hd_open (root_helpdir_file))
	    call error (2, "cannot open root help directory file")
	if (finfo (root_helpdir_file, fi) == ERR)
	    call error (3, "cannot get finfo on root help directory file")
	mtime = FI_MTIME(fi)

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
	    # Append the current compiled package help directory to the
	    # database.  Update index; make index larger if it overflows.

	    HDB_NMODULES(db) = HDB_NMODULES(db) + HD_NMODULES(hp)
	    HDB_NENTRIES(db) = HDB_NENTRIES(db) + 1

	    if (HDB_NENTRIES(db) > HDB_MAXENTRIES(db)) {
		HDB_MAXENTRIES(db) = HDB_MAXENTRIES(db) + INC_ENTRIES
		len_index = HDB_MAXENTRIES(db) * LEN_HDBINDEX
		iferr (call realloc (index, len_index, TY_STRUCT))
		    call fatal (1, "cannot reallocate index buffer")
	    }
	    ix = index + ((HDB_NENTRIES(db) - 1) * LEN_HDBINDEX)

	    # Prepare the index entry for the help directory file.
	    call strcpy (fname, DBI_KEY(ix), SZ_DBIKEY)
	    DBI_MTIME(ix) = mtime
	    DBI_OFFSET(ix) = HDB_DATALEN(db)

	    # Write the compiled help directory structure to the database
	    # file.  The directory structure consists of the HD structure
	    # and a string buffer.  We write out the HD structure followed
	    # by the string buffer.  Each must be aligned to TY_STRUCT.
	    # The offset of the string buffer from the start of the helpdir
	    # struct is saved in the HD_NEXTCH field of the HD structure,
	    # to be used when loaded and referenced to compute a pointer to
	    # the buffer.

	    HD_NEXTCH(hp) = HD_LENHD(hp)
	    sv_sbuf = HD_SBUF(hp)
	    HD_SBUF(hp) = 0
	    call mii_writei (fd, Memi[hp], HD_LENHD(hp))
	    HD_SBUF(hp) = sv_sbuf
	    call mii_writec (fd, Memc[HD_SBUF(hp)], HD_SZSBUF(hp))

	    # Keep track of the amount of struct storage that will be
	    # required later to hold the UNPACKED helpdir data.

	    #HDB_DATALEN(db) = HDB_DATALEN(db) +
	    #	HD_LENHD(hp) + ((HD_SZSBUF(hp) + SZ_STRUCT-1) / SZ_STRUCT)
	    HDB_DATALEN(db) = HDB_DATALEN(db) +
	    	HD_LENHD(hp) + ((HD_SZSBUF(hp) + SZ_STRUCT32-1) / SZ_STRUCT32)

	    call printf ("%3d %15s (%s): %d help modules\n")
		call pargi (sp + 1)
		if (HD_PAKNAME(hp) != NULL)
		    call pargstr (Memc[HD_SBUF(hp)+HD_PAKNAME(hp)])
		else
		    call pargstr ("")
		call pargstr (fname)
		call pargi (HD_NMODULES(hp))
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
		    if (finfo (fname, fi) == ERR) {
			call eprintf ("\t\t%4w(cannot access `%s')\n")
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
			iferr (hp = hd_open (fname)) {
			    hp = hp_stk[sp]
			    sp = sp - 1
			    call eprintf ("cannot open helpdir `%s'\n")
				call pargstr (fname)
			    next
			}
			pk = 1
	    		mtime = FI_MTIME(fi)
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

		    } else if (sp == 0) {
			# Root helpdir file has been fully expanded.  Scan
			# all compiled helpdirs and produce a master helpdir
			# containing an entry for each package in the database.
			# This is similar to the root helpdir, but contains
			# entries for packages at all levels, not just at the
			# root.  Note that we must save and restore the file
			# position since hdb_make_rhd accesses the file.

			call flush (fd)
			savepos = note (fd)

			# Load the database into memory.
			
			call seek (fd, HDB_DATAOFFSET(db))
			call calloc (data, 4*HDB_DATALEN(db), TY_STRUCT)
			call hdb_getdata (fd, data, HDB_DATALEN(db))

			hp = hdb_make_rhd (db, data, index)
			pk = HD_NMODULES(hp) + 1
			call strcpy ("_index", fname, SZ_FNAME)
			mtime = clktime (long (0))
			sp = -1

			call mfree (data, TY_STRUCT)
			call seek (fd, savepos)
			break

		    } else
			return			# ALL DONE
		}
		call flush (STDOUT)
	    }
	}
end
	    

# HDB_MAKE_RHD -- Make a dummy root help directory for the database.  This
#   entry looks just like any other compiled help directory, but serves as an
#   index to all packages in the database.  Each module in root is a package,
#   and every package in the system, regardless of its level in the package
#   hierarchy, has an entry in root.  We could also use the database index
#   for this purpose, but it is keyed by filename not package name, and the
#   help code considers the root help directory to be conceptually just another
#   package help directory (the db index is in the wrong format).  The root
#   directory is equivalent to the compiled lib$root.hd, except that it
#   contains entries for all subpackages as well.
#
# We purposely do NOT sort the package list, because the list is accessed
#   sequentially when templates are expanded, hence the order determines the
#   search order for the database.  Since the order in which packages are
#   entered into the database is determined by the order in which they are
#   encountered in a depth first search of the package hierarchy, the order of
#   the packages in the root help directory determines the search order.
#   It is desirable to search those packages most visible to the user (e.g.,
#   clpackage) before the more technical packages (e.g., sys).
#
# N.B.: This procedure is functionally similar to HD_OPEN and the HD_CLOSE
# procedure may be used to close the HD structure returned by either.

pointer procedure hdb_make_rhd (db, data, index)

pointer	db			#I database descriptor
pointer	data			#I data buffer (compiled help directories)
pointer	index			#I database index

int	i, j, len_modlist, pos
pointer	hp, o_hp, mp, ix, sbuf, o_mp, c_modlist, hdfile

bool	streq()
pointer	coerce()
int	hd_putstr(), strncmp()
errchk	hdb_putmodule

begin
	# Allocate and initialize descriptor and string buffer.  Must init
	# nextch to 1 because 0 is the null index.

	call calloc (hp, LEN_HDSTRUCT, TY_STRUCT)
	call calloc (sbuf, SZ_SBUF, TY_CHAR)

	HD_SBUF(hp)   = sbuf
	HD_DEFDIR(hp) = NULL
	HD_NEXTCH(hp) = 1
	HD_SZSBUF(hp) = SZ_SBUF
	HD_LENHD(hp)  = LEN_HDSTRUCT
	HD_MAXMODULES(hp) = MAX_MODULES

	# The root help directory is the first module.  Since the root is
	# not a subpackage of any other package we cannot enter it in the
	# loop below.  We must handcraft this first entry.

	HD_NMODULES(hp) = 1
	mp = HD_MODULE(hp,1)
	call aclri (Memi[mp], LEN_MODSTRUCT)
	M_NAME(mp) = hd_putstr (hp, "_root")
	M_PKG(mp)  = hd_putstr (hp, DBI_KEY(index))

	# Examine each compiled helpdir for subpackages.  Add each subpackage
	# found to the current directory.  Do not add the index entries
	# themselves because they are already referenced in the help
	# directories.

	for (i=1;  i <= HDB_NENTRIES(db);  i=i+1) {
	    ix = index + (i - 1) * LEN_HDBINDEX
	    if (strncmp (DBI_KEY(ix), "_index", 6) == 0)
		next

	    o_hp = data + DBI_OFFSET(ix)
	    HD_SBUF(o_hp) = coerce (o_hp + HD_NEXTCH(o_hp), TY_STRUCT, TY_CHAR)

	    for (j=1;  j <= HD_NMODULES(o_hp);  j=j+1) {
		mp = HD_MODULE(o_hp,j)
		if (M_PKG(mp) != NULL)
		    call hdb_putmodule (hp, o_hp, j)
	    }
	}

	# Our procedure for building the _index module list has changed the
	# ordering of the packages from the depth first order of the database
	# index.  We want the depth first order to be our search order so
	# we must reorder the module list to agree with the DBI index.
	# Make a copy of the modlist, then write a new one, overwriting the
	# old, looking up each package in the index to determine its order
	# in the new modlist.

	len_modlist = HD_NMODULES(hp) * LEN_MODSTRUCT
	call calloc (c_modlist, len_modlist, TY_STRUCT)
	call amovi (Memi[HD_MODULE(hp,1)], Memi[c_modlist], len_modlist)

	pos = 0
	do j = 1, HDB_NENTRIES(db) {
	    # Find next valid index entry.
	    ix = index + (j - 1) * LEN_HDBINDEX
	    if (strncmp (DBI_KEY(ix), "_index", 6) == 0)
		next

	    # Locate corresponding helpdir entry, if any.
	    do i = 1, HD_NMODULES(hp) {
		o_mp = c_modlist + (i - 1) * LEN_MODSTRUCT
		hdfile = HD_SBUF(hp) + M_PKG(o_mp)
		if (Memc[hdfile] == EOS)
		    next
		else if (streq (DBI_KEY(ix), Memc[hdfile])) {
		    # Append entry to output helpdir.
		    pos = pos + 1
		    call amovi (Memi[o_mp], Memi[HD_MODULE(hp,pos)],
			LEN_MODSTRUCT)
		}
	    }
	}

	call mfree (c_modlist, TY_STRUCT)
	HD_NMODULES(hp) = pos

	# Return any unused space in string buffer.
	call realloc (HD_SBUF(hp), HD_NEXTCH(hp), TY_CHAR)
	HD_SZSBUF(hp) = HD_NEXTCH(hp)

	# Return any unused module descriptors.
        HD_LENHD(hp) = HD_LENHD(hp) -
	    LEN_MODSTRUCT * (HD_MAXMODULES(hp) - HD_NMODULES(hp))
	call realloc (hp, HD_LENHD(hp), TY_STRUCT)
	HD_MAXMODULES(hp) = HD_NMODULES(hp)

	return (hp)
end


# HDB_OPEN -- Open the help database.  We can either read the precompiled
# help database or the distributed, raw help database (.hd textfiles).
# If the precompiled database is to be used access will be faster, but
# packages and modules added since the database was compiled with not be
# accessible.  To use the precompiled database we read the database index
# into memory; the actual help files are accessed via this index at runtime.
# If multiple compiled databases are specified they are combined to form one
# large database.

pointer procedure hdb_open (database)

char	database[ARB]		#I name of database to be opened

bool	no_entries_interchanged
pointer	sp, fname, files, hp, db, d_op, i_op, ix, p1, p2, db_save
int	nfiles, nints, list, fd, d_len, i_len, i, temp[LEN_HDBINDEX]

long	clktime()
bool	streq(), strgt()
pointer	hd_open(), hdb_make_rhd()
int	open(), mii_readi(), mii_readc()
int	envgets(), access(), fntopnb(), fntgfnb()
errchk	calloc, realloc, open, seek, syserrs
errchk	hd_open, fntopnb, fntgfnb, hdb_make_rhd, hdb_getdata
define	rejectfile_ 91
define	readerr_  92

begin
	call smark (sp)
	call salloc (files, SZ_HELPDB, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (hp, LEN_HDBHEADER, TY_STRUCT)
	call salloc (db_save, LEN_HDBHEADER, TY_STRUCT)

	# Allocate database descriptor.
	call calloc (db, LEN_HDBHEADER, TY_STRUCT)

	# If the database name is "helpdir", raw access is desired and the
	# name of the root help directory file is given by the environment
	# variable "helpdir".  If the database name is "helpdb", the
	# precompiled database is to be used and the name of the database
	# file or files is given by the environment variable "helpdb".
	# Otherwise the database name is assumed to be the name of a raw or
	# precompiled database file.

	if (streq (database, "helpdir")) {
	    HDB_RAW(db) = YES
	    if (envgets ("helpdir", Memc[files], SZ_HELPDB) <= 0)
		call syserrs (SYS_ENVNF, "helpdir")
	} else if (streq (database, "helpdb")) {
	    HDB_RAW(db) = NO
	    if (envgets ("helpdb", Memc[files], SZ_HELPDB) <= 0)
		call syserrs (SYS_ENVNF, "helpdb")
	} else {
	    HDB_RAW(db) = access (database, 0, TEXT_FILE)
	    call strcpy (database, Memc[files], SZ_HELPDB)
	}

	# We now have the filename or file list; if it is a raw help directory
	# file, open it with HD_OPEN and we are all done for now.  Otherwise
	# read the helpdb files and construct the help database index.

	if (HDB_RAW(db) == YES) {
	    iferr (HDB_RHD(db) = hd_open (Memc[files]))
		call fatal (1, "cannot open root help directory file")

	    call sfree (sp)
	    return (db)
	}

	# Allocate and initialize empty database header and index structures.
	HDB_MAGIC(db)	   = HDB_MAGICVAL
	HDB_CRDATE(db)     = clktime (long(0))

	HDB_NENTRIES(db)   = 0
	HDB_MAXENTRIES(db) = MAX_ENTRIES
	HDB_NMODULES(db)   = 0
	HDB_DATAPTR(db)    = NULL
	HDB_DATALEN(db)    = 0
	HDB_INDEXPTR(db)   = NULL
	HDB_INDEXLEN(db)   = 0
	HDB_MAXENTRIES(db) = HDB_NENTRIES(db)

	# Link a binary help database; open each precompiled database and
	# link it into the full runtime database.

	list = fntopnb (Memc[files], YES)
	nfiles = 0

	while (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call eprintf ("Cannot open help database file %s\n")
		    call pargstr (Memc[fname])
		next
	    }

	    # Save descriptor in case we cannot read this file.
	    call amovi (Memi[db], Memi[db_save], LEN_HDBHEADER)

	    # Read the database file header.
	    nints = LEN_HDBHEADER
	    if (mii_readi (fd, Memi[hp], nints) < nints) {
		call eprintf ("Cannot read help database file header (%s)\n")
		    call pargstr (Memc[fname])
		goto rejectfile_
	    }

	    # Verify the file type.
	    if (HDB_MAGIC(hp) != HDB_MAGICVAL) {
		call eprintf ("Not a help database file (%s)\n")
		    call pargstr (Memc[fname])
		goto rejectfile_
	    }

	    # Merge the headers.
	    HDB_NENTRIES(db) = HDB_NENTRIES(db) + HDB_NENTRIES(hp)
	    HDB_NMODULES(db) = HDB_NMODULES(db) + HDB_NMODULES(hp)
	    HDB_MAXENTRIES(db) = HDB_NENTRIES(db)

	    d_len = HDB_DATALEN(db)
	    i_len = HDB_INDEXLEN(db)

	    # Make room for the new data and index entries.
	    iferr {
		HDB_DATALEN(db) = HDB_DATALEN(db) + HDB_DATALEN(hp)
		call realloc (HDB_DATAPTR(db), HDB_DATALEN(db), TY_STRUCT)
		HDB_INDEXLEN(db) = HDB_INDEXLEN(db) + HDB_INDEXLEN(hp)
		call realloc (HDB_INDEXPTR(db), HDB_INDEXLEN(db), TY_STRUCT)
	    } then
		call erract (EA_WARN)

	    d_op = HDB_DATAPTR(db) + d_len
	    i_op = HDB_INDEXPTR(db) + i_len

	    # Append the data area of the new database file to the end of
	    # the data buffer.

	    call seek (fd, HDB_DATAOFFSET(hp))
	    call hdb_getdata (fd, d_op, HDB_DATALEN(hp))

	    # Append the index area of the new database file to the end of
	    # the index buffer.

	    nints = HDB_INDEXLEN(hp)
	    call seek (fd, HDB_INDEXOFFSET(hp))

	    do i = 1, HDB_NENTRIES(hp) {
		ix = i_op + (i - 1) * LEN_HDBINDEX
		if (mii_readc (fd, DBI_KEY(ix), SZ_DBIKEY + 1) < SZ_DBIKEY + 1)
		    goto readerr_
		if (mii_readi (fd, DBI_OFFSET(ix), LEN_DBIDATA) < LEN_DBIDATA) {
readerr_	    call eprintf ("Cannot read database index (%s)\n")
			call pargstr (Memc[fname])
		    goto rejectfile_
		}

		# Patch the index entry to reflect the new offset of the
		# directory entry in the data buffer.  Rename the _index
		# entries in the old (input) databases, since we will be
		# building a new _index for the final composite database.

		DBI_OFFSET(ix) = DBI_OFFSET(ix) + d_len
		if (streq (DBI_KEY(ix), "_index")) {
		    call sprintf (DBI_KEY(ix), SZ_DBIKEY, "_index.%s")
			call pargstr (Memc[fname])
		}
	    }

	    nfiles = nfiles + 1
	    call close (fd)
	    next
rejectfile_
	    # Could not read file; restore the descriptor to the state it
	    # was in before we tried to read the file, to repair any damage.

	    d_op = HDB_DATAPTR(db);  i_op = HDB_INDEXPTR(db)
	    call amovi (Memi[db_save], Memi[db], LEN_HDBHEADER)
	    HDB_DATAPTR(db) = d_op;  HDB_INDEXPTR(db) = i_op

	    call close (fd)
	}

	# Verify that there was at least one valid file in the list.
	if (nfiles <= 0)
	    call error (5, "invalid help database file list")

	# Build the package name index (root helpdir) for the new database.
	hp = hdb_make_rhd (db, HDB_DATAPTR(db), HDB_INDEXPTR(db))

	# Append the compiled package help directory to the database.
	HDB_NMODULES(db) = HDB_NMODULES(db) + HD_NMODULES(hp)
	HDB_NENTRIES(db) = HDB_NENTRIES(db) + 1

	# Append the compiled _index helpdir and associated string buffer
	# to the database data buffer, as if these data structures had been
	# read from the help database file (all helpdir access codes assume
	# this structure).

	d_len = HDB_DATALEN(db)
	nints = HD_LENHD(hp) + (HD_SZSBUF(hp) + SZ_STRUCT32-1) / SZ_STRUCT32
	HDB_DATALEN(db) = HDB_DATALEN(db) + nints
	call realloc (HDB_DATAPTR(db), HDB_DATALEN(db), TY_STRUCT)
	d_op = HDB_DATAPTR(db) + d_len

	HD_NEXTCH(hp) = HD_LENHD(hp)
	call amovi (Memi[hp], Memi[d_op], HD_LENHD(hp))
	call amovc (Memc[HD_SBUF(hp)], Memi[d_op+HD_LENHD(hp)], HD_SZSBUF(hp))

	# Add an index entry for the _index helpdir.
	if (HDB_NENTRIES(db) > HDB_MAXENTRIES(db)) {
	    HDB_MAXENTRIES(db) = HDB_MAXENTRIES(db) + 1
	    nints = HDB_MAXENTRIES(db) * LEN_HDBINDEX
	    iferr (call realloc (HDB_INDEXPTR(db), nints, TY_STRUCT))
		call fatal (1, "cannot reallocate index buffer")
	}

	ix = HDB_INDEXPTR(db) + (HDB_NENTRIES(db) - 1) * LEN_HDBINDEX
	call strcpy ("_index", DBI_KEY(ix), SZ_DBIKEY)
	DBI_MTIME(ix)  = clktime (long(0))
	DBI_OFFSET(ix) = d_op - HDB_DATAPTR(db)

	# Free dedicated hp/sbuf, since descriptor is in data buffer now.
	call hd_close (hp)

	# Sort the index; a crude sort is adequate here.
	if (HDB_NENTRIES(db) > 1) {
	    repeat {
		no_entries_interchanged = true
		do i = 1, HDB_NENTRIES(db) - 1 {
		    p1 = HDB_INDEXPTR(db) + (i - 1) * LEN_HDBINDEX
		    p2 = p1 + LEN_HDBINDEX
		    if (strgt (DBI_KEY(p1), DBI_KEY(p2))) {
			call amovi (Memi[p1], temp,     LEN_HDBINDEX)
			call amovi (Memi[p2], Memi[p1], LEN_HDBINDEX)
			call amovi (temp,     Memi[p2], LEN_HDBINDEX)
			no_entries_interchanged = false
		    }
		}
	    } until (no_entries_interchanged)
	}

	# Get the index offset of the NEW "_index" help directory.
	HDB_INDEX(db) = 0
	do i = 1, HDB_NENTRIES(db) {
	    ix = HDB_INDEXPTR(db) + (i - 1) * LEN_HDBINDEX
	    if (streq (DBI_KEY(ix), "_index")) {
		HDB_INDEX(db) = i
		break
	    }
	}
	if (HDB_INDEX(db) == 0)
	    call eprintf ("Help warning: cannot find _index")

	call fntclsb (list)
	call sfree (sp)

	return (db)
end


# HDB_CLOSE -- Close the help database.  If raw database is being accessed,
# this means close root help directory and free DB struct.  If precompiled,
# deallocate all buffers used by the database.

procedure hdb_close (db)

pointer	db			# database descriptor

begin
	if (HDB_RAW(db) == YES)
	    call hd_close (HDB_RHD(db))
	else {
	    call mfree (HDB_DATAPTR(db), TY_STRUCT)
	    call mfree (HDB_INDEXPTR(db), TY_STRUCT)
	}
	call mfree (db, TY_STRUCT)
end


# HDB_LOAD -- Load the named help directory.  Help directories are referred
# to by the name of the directory file, and the database is keyed by the name
# of the file.  If the database is being accessed raw, we open and compile
# the named file directly, otherwise we look up the compiled directory in the
# database.  In either case we return a HD pointer to the compiled directory.
# The directory "_index" is special, being the package index directory for the
# entire database.

pointer procedure hdb_load (db, helpdir)

pointer	db			# database descriptor
char	helpdir[ARB]		# help directory to be accessed.

bool	index
pointer	hp, ix, sp, errmsg

bool	streq()
int	hdb_search()
pointer	hd_open(), coerce()
errchk	hdb_open

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	index = (streq (helpdir, "_index") || streq (helpdir, "_root"))

	if (HDB_RAW(db) == YES) {
	    if (index)
		hp = HDB_RHD(db)
	    else
		hp = hd_open (helpdir)

	} else {
	    # Compute and return pointer to compiled HD.  Fix up pointer to
	    # the string buffer sbuf, since the pointer value depends on the
	    # value of the pointer to the newly allocated data area.  If not
	    # found, return NULL pointer.

	    if (index) {
		ix = HDB_INDEXPTR(db) + (HDB_INDEX(db) - 1) * LEN_HDBINDEX

	    } else if (hdb_search (db, helpdir, ix) == ERR) {
		# There should be a better way to do this...  Format error
		# message and pass to the error handling code, then restore
		# stack before taking the error action.

		call sprintf (Memc[errmsg], SZ_LINE,
		"help directory `%s' not found")
		    call pargstr (helpdir)
	        iferr (call error (6, Memc[errmsg])) {
		    call sfree (sp)
		    call erract (EA_ERROR)
		    return (NULL)
		}
	    }

	    hp = HDB_DATAPTR(db) + DBI_OFFSET(ix)
	    HD_SBUF(hp) = coerce (hp + HD_NEXTCH(hp), TY_STRUCT, TY_CHAR)
	}

	call sfree (sp)
	return (hp)
end


# HDB_FREE -- Free space for a help directory loaded with HDB_LOAD.  If we are
# using raw access, we let the helpdir package free what ever it wants to since
# it did the allocating.  If we are accessing the compiled database then there
# is nothing to free, since everything is maintained in memory.

procedure hdb_free (db, hp)

pointer	db			# database descriptor
pointer	hp			# help directory

begin
	if (HDB_RAW(db) == YES)
	    call hd_close (hp)
end


# HDB_SEARCH -- Search the database index for the given key.  Since the index
# has been sorted we can use a binary search.  If the key is found we return
# a pointer to the associated index as an output argument, and OK as the
# function value.

int procedure hdb_search (db, key, ix)

pointer	db			# database descriptor
char	key[ARB]		# filename key to be located
pointer	ix			# pointer to index entry (output)

int	low, high, pos
pointer	ixoff
bool	strle(), streq()

begin
	ixoff = HDB_INDEXPTR(db)
	low   = 1
	high  = HDB_NENTRIES(db)

	# Cut range of search in half until range is narrowed to two values (if
	# we go until HIGH-LOW >= 1 an infinite loop can occur).

	while (high - low > 1) {
	    pos = (high + low) / 2
	    if (strle (key, DBI_KEY(ixoff + (pos-1) * LEN_HDBINDEX)))
		high = pos
	    else
		low  = pos
	} 

	ix = ixoff + (high - 1) * LEN_HDBINDEX
	if (streq (key, DBI_KEY(ix)))
	    return (high)
	ix = ixoff + (low - 1) * LEN_HDBINDEX
	if (streq (key, DBI_KEY(ix)))
	    return (low)

	return (ERR)
end


# HDB_EXAMINE -- Examine the structure of the compiled database.  A description
# of the contents is printed on the output file.

procedure hdb_examine (fd, helpdb, verbose)

int	fd			# output file
char	helpdb[ARB]		# filename of database to be examined
bool	verbose			# print menus as well

int	list, i
long	fi[LEN_FINFO]
pointer	sp, fname, date, db, ixoff, ix

bool	strne()
pointer	hdb_open()
int	finfo(), fntopnb(), fntgfnb()
errchk	hdb_open, hdb_printpack, fntopnb, fntgfnb

begin
	call smark (sp)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	db = hdb_open (helpdb)
	ixoff = HDB_INDEXPTR(db)

	call cnvdate (HDB_CRDATE(db), Memc[date], SZ_DATE)

	list = fntopnb (helpdb, YES)
	while (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
	    if (finfo (Memc[fname], fi) == ERR) {
		call eprintf ("Cannot get info on file `%s'\n")
		    call pargstr (Memc[fname])
		next
	    }

	    call fprintf (fd, "Help database %s created %s by %s, size=%d\n")
		call pargstr (Memc[fname])
		call pargstr (Memc[date])
		call pargstr (FI_OWNER(fi))
		call pargl (FI_SIZE(fi))
	}
	call fntclsb (list)

	call fprintf (fd, "Total of %d modules in %d packages\n")
	    call pargi (HDB_NMODULES(db))
	    call pargi (HDB_NENTRIES(db) - 1)
	
	do i = 1, HDB_NENTRIES(db) {
	    ix = ixoff + (i - 1) * LEN_HDBINDEX
	    if (strne (DBI_KEY(ix), "_index"))
		call hdb_printpack (fd, db, ix, verbose)
	}

	call hdb_close (db)
	call sfree (sp)
end


# HDB_PRINTPACK -- Print a description of a single package on the output
# file.

procedure hdb_printpack (fd, db, ix, verbose)

int	fd			# output file
pointer	db			# database descriptor
pointer	ix			# database index descriptor of package
bool	verbose			# print menus

int	m
pointer	sp, hp, paknames, date
long	fi[LEN_FINFO]
int	hd_getname(), envgeti(), finfo()
pointer	hdb_load()
errchk	hd_getname

begin
	call smark (sp)
	call salloc (paknames, MAX_MENUSIZE, TY_POINTER)
	call salloc (date, SZ_DATE, TY_CHAR)

	iferr (hp = hdb_load (db, DBI_KEY(ix))) {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}

	call cnvdate (DBI_MTIME(ix), Memc[date], SZ_DATE)
	if (finfo (DBI_KEY(ix), fi) == ERR)
	    FI_OWNER(fi) = EOS

	if (verbose)
	    call fprintf (fd, "\n%s %s %s %s\n")
	else
	    call fprintf (fd, "%-12s %s %-8s %s\n")

	if (HD_PAKNAME(hp) != 0)
	    call pargstr (Memc[HD_SBUF(hp) + HD_PAKNAME(hp)])
	else
	    call pargstr ("")
	call pargstr (Memc[date])
	call pargstr (FI_OWNER(fi))
	call pargstr (DBI_KEY(ix))

	if (verbose) {
	    # Extract the names of the modules in the package.  Save the
	    # pointers in an array for the table print routine.

	    for (m=0;  m < MAX_MENUSIZE;  m=m+1) {
		call salloc (Memi[paknames+m], MAX_NAMELEN, TY_CHAR)
		if (hd_getname (hp, m+1, TY_MODNAME, Memc[Memi[paknames+m]],
		    MAX_NAMELEN) <= 0)
			break
	    }

	    # Now print the table.  It is not necessary to sort the table,
	    # because the "helpdir" code (which reads the help directory) has
	    # already done so.

	    call strtbl (fd, Memc, Memi[paknames], m, FIRST_COL,
		envgeti ("ttyncols"), MAX_NAMELEN, 0)
	}

	call hdb_free (db, hp)
	call sfree (sp)
end


# HDB_PUTMODULE -- Put a module (subpackage) into the root help directory.
# Add new entry, increasing space if necessary.  Expand all filenames to
# remove help ldir references and place filenames in our string buffer,
# sbuf offsets into module descriptor.  Increase size of sbuf if it fills.

procedure hdb_putmodule (hp, o_hp, pk)

pointer	hp		# new help directory being extended
pointer	o_hp		# old help directory
int	pk		# module number in old directory

int	firstch, m
pointer	sp, fname, sbuf, o_sbuf, mp, o_mp, pakname
int	hd_getname(), hd_putstr()
bool	streq()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	sbuf    = HD_SBUF(hp)
	o_sbuf  = HD_SBUF(o_hp)
	o_mp    = HD_MODULE(o_hp,pk)
	pakname = o_sbuf + M_NAME(o_mp)

	# Check if this is a redefinition of a module already defined.
	# If so, warn user that new package does not have a unique name,
	# and omit package.

	firstch = Memc[pakname]
	for (m=1;  m <= HD_NMODULES(hp);  m=m+1) {
	    mp = HD_MODULE(hp,m)
	    if (Memc[sbuf+M_NAME(mp)] == firstch)
		if (streq (Memc[sbuf+M_NAME(mp)], Memc[pakname])) {
		    call eprintf ("package name `%s' (hd=%s) is not unique\n")
			call pargstr (Memc[pakname])
			call pargstr (Memc[o_sbuf+M_PKG(o_mp)])
		    call sfree (sp)
		    return
		}
	}

	# If we are out of space for modules, increase the descriptor
	# structure size to allow more module descriptors.

	if (m > HD_NMODULES(hp)) {
	    if (m > HD_MAXMODULES(hp)) {
		HD_LENHD(hp) = HD_LENHD(hp) + (INC_MODULES * LEN_MODSTRUCT)
		call realloc (hp, HD_LENHD(hp), TY_STRUCT)
		HD_MAXMODULES(hp) = HD_MAXMODULES(hp) + INC_MODULES
	    }
	    HD_NMODULES(hp) = m
	}

	mp = HD_MODULE(hp,m)
	call aclri (Memi[mp], LEN_MODSTRUCT)

	# Put module name in string buffer and save index of string in module
	# descriptor.

	M_NAME(mp) = hd_putstr (hp, Memc[pakname])

	# Extract all filenames and move into string buffer.  Call hd_getname to
	# extract filenames from old directory, so that help-ldir references
	# are expanded.

	if (hd_getname (o_hp, pk, TY_HLP, Memc[fname], SZ_FNAME) > 0)
	    M_HLP(mp) = hd_putstr (hp, Memc[fname])
	if (hd_getname (o_hp, pk, TY_SYS, Memc[fname], SZ_FNAME) > 0)
	    M_SYS(mp) = hd_putstr (hp, Memc[fname])
	if (hd_getname (o_hp, pk, TY_SRC, Memc[fname], SZ_FNAME) > 0)
	    M_SRC(mp) = hd_putstr (hp, Memc[fname])
	if (hd_getname (o_hp, pk, TY_PKG, Memc[fname], SZ_FNAME) > 0)
	    M_PKG(mp) = hd_putstr (hp, Memc[fname])
	if (hd_getname (o_hp, pk, TY_MEN, Memc[fname], SZ_FNAME) > 0)
	    M_MEN(mp) = hd_putstr (hp, Memc[fname])

	call sfree (sp)
end


# HDB_GETDATA -- Read a stored series of compiled help directories, stored
# externally in a machine independent format, into the given data buffer.
# Each stored help directory consists of a fixed sized MII-32 header followed
# by a MII byte packed string buffer of arbitrary length.  Reading begins
# at the current file position.

procedure hdb_getdata (fd, obuf, buflen)

int	fd			#I input file
pointer	obuf			#O receives unpacked helpdir data
int	buflen			#O max su out

int	i, nelem, nr, sz_mii_struct
pointer	op, hp
int	mii_readi(), mii_readc()
errchk	mii_readi, mii_readc
define	readerr_ 91

begin
	nr = 0
	for (op=obuf;  nr < buflen;  ) {
	    hp = op

	    # Get fixed size helpdir header.
	    if (mii_readi (fd, Memi[op], LEN_BASEHD) < LEN_BASEHD)
		goto readerr_
	    nr = nr + LEN_BASEHD

	    # Get module entries.
	    op = op + LEN_BASEHD
	    nelem = HD_LENHD(hp) - LEN_BASEHD
	    if (mii_readi (fd, Memi[op], nelem) < nelem)
		goto readerr_
	    nr = nr + nelem

	    # Get string buffer.
	    op = op + nelem
	    nelem = HD_SZSBUF(hp) # / (SZ_INT / SZ_INT32)
	    if (mii_readc (fd, Memi[op], nelem) < nelem)
		goto readerr_

	    nr = nr + ((nelem + SZ_STRUCT32-1) / SZ_STRUCT32)
            sz_mii_struct = MII_INT / NBITS_BYTE / SZB_CHAR
            op = op + ((nelem + sz_mii_struct-1) / sz_mii_struct)
	}

	return

readerr_
	# Common read error code.
	call error (1, "Cannot read help database data\n")
end
