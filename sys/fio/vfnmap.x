# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<ctype.h>
include	<mach.h>
include	<error.h>
include	<syserr.h>
include	<config.h>
include	<fio.h>

.help vfnmap
.nf ___________________________________________________________________________
VFNMAP -- A package for mapping virtual filenames to and from OS filenames.
The abstract datatype dealt with here is the VFN.  The operations defined for
a VFN are [1] map to OSFN, [2] add a new VFN to the VFN database, and [3] delete
a VFN from the VFN database.  The VFN database is manipulated only by this
package.  This is an internal package, not a user package -- the semantics of
locking parts of the VFN database are delicate.

A VFN must be opened separately for each file to be accessed, except when
reading a directory in which case the vfnmap must be opened separately for
each directory to be scanned.  Only a single VFN may be opened for writing by
a process at any one time (any number of VFN's, including directories, may be
opened for reading at any one time).  The mapping file is not physically opened
unless the escape sequence encoded filename is degenerate.  The mapping file is
locked only if the vfn is degenerate and the access mode is VFN_WRITE.  The
recognized vfn access modes are VFN_READ, VFN_WRITE, and VFN_UNMAP (for reading
directories).

It is intended that THE VFN WILL BE OPENED FOR ONLY A BRIEF PERIOD OF TIME TO
MINIMIZE THE AMOUNT OF TIME THAT THE MAPPING FILE IS LOCKED.  Furthermore,
while the VFN is locked we must avoid any operations that involve waiting for
system resources and hence introduce the possibility of deadlock.

	vp =	 vfnopen (vfn, mode)
		vfnclose (vp, update)
	stat =    vfnmap (vp, osfn, maxch)
	stat =	  vfnadd (vp, osfn, maxch)
	stat =    vfndel (vp, osfn, maxch)
	stat =	vfnunmap (vp, osfn, vfn, maxch)

		  fmapfn (vfn, osfn, maxch)	[=:vfnopen/RO,vfnmap,vfnclose]

A distinction is made between mapping the filename and opening and closing
the vfn to permit efficient and secure error recovery.  The mapping file is
not updated on disk until the physical file operation (create, delete, etc)
has succeeded.  If the operation fails vfnclose is called with VFN_NOUPDATE
and the mapping file is not touched.  If the vfn was opened VFN_READ the
update flag is ignored.  No vfn disk data structures will be modified if a
vfn is closed with VFN_NOUPDATE set.  If updating is enabled, ".zmd" dependency
files may be created or deleted, the mapping file may be created, deleted,
or updated.

The VFNMAP, VFNADD, VFNDEL, and VFNUNMAP procedures all perform a mapping
operation, returning OK if the filename could be mapped and ERR if the
mapping fails and no OSFN or VFN is returned.  A VFNMAP, VFNADD, or VFNDEL
mapping can only return ERR if the VFN is degenerate and either no entry
was found in the mapping file (VFNMAP, VFNDEL) or there already was an entry
(VFNADD).  OSFN is returned as a packed string, VFN as a normal string.

NOTE1 -- (Dec84) The "degeneracy flag files" are no longer used, but some of
the code has been left in place, to avoid having to modify and test the code
after its removal.  This code should be removed when other modifications are
required which will require careful testing of the package.

NOTE2 -- Interrupts and automatic error checking should be disabled while a
VFN is open to prevent corruption of the mapping file, failure to remove a
file lock, or failure to close the mapping file.
.endhelp ______________________________________________________________________

define	SZ_VFN		255		# max chars in V_VFN field
define	LEN_FN		128		# no. chars allocated to VFNFN field
define	SZ_FNPAIR	(LEN_FN*2)	# size of filename pair 2(+EOS+align)
define	MAX_LONGFNAMES	100		# max filename pairs in FNMAP
define	SZ_ZFD		4		# size of ".zfd" extension
define	MAX_READS	5		# max trys to read mapping file
define	MAX_DEGENERACY	50		# max VFN's mapping to same OSFN
define	MAX_DIGITS	2		# max digits in degeneracy index

define	V_MAP		1		# VFN opcodes
define	V_ADD		2
define	V_DEL		3
define	V_UNMAP		4

# VFD -- VFN descriptor structure.  Assumes an 80 char (or less) OSDIR field
# and 35 char (or less) VFN, ROOT and EXTN fields (see fio.h).

define	LEN_VFD		778

define	V_MFD		Memi[$1]		# ptr to mapping file descriptor
define	V_ACMODE	Memi[$1+1]		# access mode
define	V_LENOSDIR	Memi[$1+2]		# length of OSDIR string
define	V_LENROOT	Memi[$1+3]		# length of ROOT string
define	V_LENEXTN	Memi[$1+4]		# length of EXTN string
define	V_LONGROOT	Memi[$1+5]		# root field exceeds OS limit
define	V_VFN		Memc[P2C($1+10)]	# VFN - ldir
define	V_OSDIR		Memc[P2C($1+266)]	# OS directory
define	V_ROOT		Memc[P2C($1+522)]	# OS root filename
define	V_EXTN		Memc[P2C($1+650)]	# OS extension

# MFD -- Mapping file descriptor structure.  An upper limit is placed on
# the number of filename pairs in the descriptor because it is assumed that
# long filenames are rare.  Note that this places a limit on the number of long
# filenames in the directory, not on the number of files in the directory.
# If this is a problem the code is not difficult to generalize.

define	LEN_MFD		(250+MAX_LONGFNAMES*SZ_FNPAIR/SZ_STRUCT)
define	MIN_LENMFD	(250+1*SZ_FNPAIR/SZ_STRUCT)
define	SZ_MAPFNAME	(240*SZ_STRUCT-1)

define	M_CHECKSUM	Memi[$1]		# checksum of file when written
define	M_CHAN		Memi[$1+1]		# OS channel of mapping file
define	M_LOCKTIME	Meml[$1+2]		# clktime when lock set
define	M_NFILES	Memi[$1+3]		# no. filename pairs in map
define	M_LASTOP	Memi[$1+4]		# code for last op on database
define	M_MODIFIED	Memi[$1+5]		# YES if database modified
define	M_ADDZMD	Memi[$1+6]		# create .zmd file at update
define	M_DELZMD	Memi[$1+7]		# delete .zmd file at update
define	M_MAPFNAME	Memc[P2C($1+10)]	# name of map file
define	M_FNMAP		(P2C($1+250))		# filename pairs

# Subscript the (VFN,OSFN) filename pairs.  For example, FN_VFN(mfd,n)
# references the VFN field of filename pair N of the mapping file MFD.

define	FN_VFN		Memc[M_FNMAP($1)+(($2)*2-2)*LEN_FN]
define	FN_OSFN		Memc[M_FNMAP($1)+(($2)*2-1)*LEN_FN]


# VFNOPEN -- Open a VFN.  Allocate VFD and convert the VFN into OSFN, ROOT,
# and EXTN fields.  The EXTN field is mapped to the OS extension, but the
# ROOT field may be longer than is permitted by the OS.  The mapping file
# is not referenced until the OSFN is requested in a map, add, or del op.

pointer procedure vfnopen (vfn, mode)

char	vfn[ARB]		# virtual filename
int	mode			# access mode for VFN database

bool	first_time
int	n_open_vfns, root_offset, extn_offset
pointer	def_vfd, vfd
data	first_time /true/
common	/vfncom/ n_open_vfns
errchk	syserrs, malloc, calloc, vfn_translate, vvfn_readmapfile

begin
	# After the first call a single VFD will be allocated at all times.
	# This eliminates the need to allocate and free a descriptor in each
	# call.

	if (first_time) {
	    call malloc (def_vfd, LEN_VFD, TY_STRUCT)
	    n_open_vfns = 0
	    first_time = false
	}

	# Allocate and initialize the VFD.

	if (n_open_vfns <= 0) {
	    vfd = def_vfd
	    call aclri (Memi[vfd], LEN_VFD)
	} else
	    call calloc (vfd, LEN_VFD, TY_STRUCT)
	n_open_vfns = n_open_vfns + 1

	# Break the VFN into its component parts.  Map using escape sequence
	# encoding, but do not squeeze the OSFN.  Most calls are read only
	# accesses that do not involve accessing the VFN database.  The
	# following is what takes all the time (string concatenation and
	# packing in VFNMAP is also a factor).

	call vfn_translate (vfn, V_OSDIR(vfd), V_LENOSDIR(vfd),
				 V_ROOT(vfd),  V_LENROOT(vfd),
				 V_EXTN(vfd),  V_LENEXTN(vfd))

	# Determine whether the length of the root exceeds the max host system
	# filename length, and set flag if so.  If longroot, squeeze the root
	# because the unsqueezed root is not useful for anything.  The V_VFN
	# field is used as a temporary.

	if (V_LENROOT(vfd) > MAX_ROOTLEN) {
	    call vfn_squeeze (V_ROOT(vfd), V_VFN(vfd), MAX_ROOTLEN)
	    call strcpy (V_VFN(vfd), V_ROOT(vfd), MAX_ROOTLEN)
	    V_LENROOT(vfd) = MAX_ROOTLEN
	    V_LONGROOT(vfd) = YES
	} else
	    V_LONGROOT(vfd) = NO

	# Set access mode and save VFN.
	V_ACMODE(vfd) = mode

	switch (mode) {
	case VFN_READ, VFN_WRITE:
	    call zfnbrk (vfn, root_offset, extn_offset)
	    call strcpy (vfn[root_offset], V_VFN(vfd), SZ_VFN)
	case VFN_UNMAP:
	    call vvfn_readmapfile (vfd)
	default:
	    call syserrs (SYS_FVFNMODE, vfn)
	}
	
	return (vfd)
end


# VFNMAP -- Map and pack the VFN into an OSFN, but do not modify the database.
# The mapping file is accessed only if the OS filename is degenerate, i.e.,
# if the directory contains more than one VFN mapping to the same OSFN after
# escape sequence encoding and squeezing.

int procedure vfnmap (vfd, osfn, maxch)

pointer	vfd			# pointer to VFD descriptor
char	osfn[ARB]		# char buffer to receive packed OSFN
int	maxch

int	status
int	vfnmapu()

begin
	status = vfnmapu (vfd, osfn, maxch)
	call osfn_pkfname (osfn, osfn, maxch)

	return (status)
end


# VFNMAPU -- Map but do not pack a VFN into an OSFN.  Call VFNMAP if you want
# a packed osfn.

int procedure vfnmapu (vfd, osfn, maxch)

pointer	vfd			# pointer to VFD descriptor
char	osfn[maxch]		# char buffer to receive unpacked OSFN
int	maxch

int	op, status
int	gstrcpy(), vfn_getosfn()
errchk	vfn_getosfn, vvfn_readmapfile
define	degenerate_ 91

begin
	# The OSDIR and ROOT fields are used twice below, so we concatenate
	# them here.

	op = gstrcpy (V_OSDIR(vfd), osfn, maxch) + 1
	op = op + gstrcpy (V_ROOT(vfd), osfn[op], maxch-op+1)

	# If the root field of the osfn is within the length limit for a host
	# system filename all we have to do is concatenate and pack, returning
	# the packed osfn.  If the root has been squeezed we have to look to
	# see if it is unique within the directory; if it is then we do not
	# have to read the mapping file.  Filename mapping is fast provided
	# we do not have to read the mapping file.

	if (V_LONGROOT(vfd) == YES)
	    goto degenerate_

	# Concatenate the final osfn.
	if (V_LENEXTN(vfd) > 0 && op < maxch) {
	    osfn[op] = EXTN_DELIMITER
	    op = op + 1
	    call strcpy (V_EXTN(vfd), osfn[op], maxch-op+1)
	} else
	    osfn[op] = EOS

	return (OK)


degenerate_
	# If we get here then the squeezed filename is degenerate and we have
	# to read the mapping file to get the OSFN assigned by VFNADD.  If the
	# mapping file does not exist and the VFN is open with write perm,
	# then we were probably called by VFNADD and we go ahead and create
	# a new mapping file.

	call vvfn_readmapfile (vfd)

	# Search the file name list for the named VFN.
	if (vfn_getosfn (vfd, V_VFN(vfd), osfn, maxch) <= 0)
	    status = ERR
	else
	    status = OK

	M_LASTOP(V_MFD(vfd)) = V_MAP

	return (status)
end


# VFNADD -- Map a VFN to an OSFN and add the VFN,OSFN pair to the VFN database
# if the OSFN is long.  An entry must be made whether or not the filename is
# degenerate, to permit the inverse mapping.

int procedure vfnadd (vfd, osfn, maxch)

pointer	vfd			# pointer to VFN descriptor
char	osfn[maxch]		# buffer to receive packed OSFN
int	maxch

int	file_exists
int	vfnmap(), vfn_enter()
errchk	vfnmap

begin
	# Call VFNMAP to perform the mapping and possibly open the database.
	# If VFNMAP returns ERR then the filename was degenerate but was not
	# found in the database, which is what we want since we are adding
	# the file.  We return ERR if the file already exists, whether or
	# not the name is degenerate.

	if (vfnmap (vfd, osfn, maxch) == ERR) {
	    # Long filename but no entry found in database; we have to add
	    # a new entry.
	    return (vfn_enter (vfd, osfn, maxch))
	} else if (V_LONGROOT(vfd) == NO) {
	    # Short filename; see if physical file exists.
	    call zfacss (osfn, 0, 0, file_exists)
	    if (file_exists == YES)
		return (ERR)
	    else
		return (OK)
	} else
	    # VFN found in database and filename is long.
	    return (ERR)
end


# VFNDEL -- Map a VFN to an OSFN and delete the VFN,OSFN pair from the VFN
# database if the OSFN is long.

int procedure vfndel (vfd, osfn, maxch)

pointer	vfd			# pointer to VFN descriptor
char	osfn[maxch]		# buffer to receive packed OSFN
int	maxch

char	first_char
int	fn, fn_index, ip, junk
pointer	sp, root, extn, mfd, vfnp
bool	streq()
int	vfnmap()
errchk	vfnmap

begin
	call smark (sp)
	call salloc (root, SZ_VFNFN, TY_CHAR)
	call salloc (extn, SZ_VFNFN, TY_CHAR)

	# Call VFNMAP to perform the mapping and possibly open the database.
	# If VFNMAP returns ERR then the filename was degenerate but was not
	# found in the database and we are done.  If VFNMAP returns OK we
	# are done unless the filename is long.

	if (vfnmap (vfd, osfn, maxch) == ERR) {
	    # Long filename but no entry found in database; nothing to delete.
	    call sfree (sp)
	    return (ERR)
	} else if (V_LONGROOT(vfd) == NO) {
	    # Short filename; nothing to delete but it is not an error.
	    call sfree (sp)
	    return (OK)
	}

	# If we get here the VFN was found in the database and the filename
	# is long.  Locate the VFN entry and determine if there are any
	# other entries mapping to the same squeezed root.

	mfd = V_MFD(vfd)
	vfnp = M_FNMAP(mfd)
	first_char = V_VFN(vfd)
	fn_index = 0
	M_DELZMD(mfd) = YES

	do fn = 1, M_NFILES(mfd) {
	    if (Memc[vfnp] == first_char) {
		if (fn_index == 0)
		    if (streq (Memc[vfnp], V_VFN(vfd)))
			fn_index = fn
		ip = 1
		call vfn_encode (Memc[vfnp], ip, Memc[root], junk, Memc[extn],
		    junk)
		if (streq (Memc[root], V_ROOT(vfd))) {
		    M_DELZMD(mfd) = NO
		    if (fn_index != 0)
			break
		}
	    }
	    vfnp = vfnp + SZ_FNPAIR
	}

	# Delete the filename pair from the database.  Deletion is effected by
	# shifting the higher indexed filename pairs back one filepair.
	# We are more concerned here about saving space in the mapping file
	# and in the MFD, than in making set deletion efficient.

	for (fn = fn_index + 1;  fn <= M_NFILES(mfd);  fn=fn+1)
	    call amovc (FN_VFN(mfd,fn), FN_VFN(mfd,fn-1), SZ_FNPAIR)
	M_NFILES(mfd) = M_NFILES(mfd) - 1

	M_LASTOP(mfd) = V_DEL
	M_MODIFIED(mfd) = YES

	call sfree (sp)
	return (OK)
end


# VFNUNMAP -- Convert an OSFN into a VFN.  Search the MFD file list for the
# named OSFN, and if found return the associated VFN as an output argument and
# the length of the VFN string as the function value.  If entry is not found
# perform the inverse transformation (map extension, invert escape sequence
# encoding).  The VFN returned does not include a logical directory prefix.
# This function is called to perform the inverse mapping when reading
# directories.

int procedure vfnunmap (vfd, osfn, vfn, maxch)

pointer	vfd			# VFN descriptor
char	osfn[maxch]		# OS filename to be searched for (packed)
char	vfn[ARB]		# receives unpacked VFN
int	maxch

char	first_char
int	fn, op, extn_offset
pointer	mfd, osfnp, sp, osfname, ip
bool	streq()
int	gstrcpy(), vfn_decode()

begin
	call smark (sp)
	call salloc (osfname, SZ_PATHNAME, TY_CHAR)

	call strupk (osfn, Memc[osfname], SZ_PATHNAME)
	if (CASE_INSENSITIVE && HOST_CASE != 'L')
	    call strlwr (Memc[osfname])

	# Search mapping file for OSFN and return VFN if found.

	mfd = V_MFD(vfd)
	osfnp = M_FNMAP(mfd) + LEN_FN
	first_char = Memc[osfname]

	do fn = 1, M_NFILES(mfd) {
	    if (Memc[osfnp] == first_char)
		if (streq (Memc[osfnp], Memc[osfname])) {
		    call sfree (sp)
		    return (gstrcpy (FN_VFN(mfd,fn), vfn, maxch))
		}
	    osfnp = osfnp + SZ_FNPAIR
	}

	# No entry in mapping file, so we must perform the inverse
	# transformation.  Decode the root, unmap and decode the extension,
	# and return VFN.  If there are multiple EXTN_DELIMITER delimited
	# fields only the final one is mapped as an extension, but all are
	# decoded.

	vfn[1] = EOS
	extn_offset = 0
	ip = osfname
	op = 1

	while (Memc[ip] != EOS) {
	    op = op + vfn_decode (Memc, ip, vfn[op], maxch-op+1)
	    if (Memc[ip] == EXTN_DELIMITER) {
		ip = ip + 1
		vfn[op] = '.'
		op = op + 1
		vfn[op] = EOS
		extn_offset = op
	    }
	}

	# Add mapped filename extension.  If the OS extension maps into a
	# null VFN extension omit the trailing period.  If the . is preceded
	# by another dot it is not considered an extension delimiter.

	if (extn_offset > 0) {
	    call vfn_unmap_extension (vfn[extn_offset], vfn[extn_offset],
		SZ_VFNFN - extn_offset + 1)
	    if (vfn[extn_offset] != EOS) {
		for (op=extn_offset;  vfn[op] != EOS;  op=op+1)
		    ;
	    } else if (extn_offset<=2 || vfn[extn_offset-2] == EXTN_DELIMITER) {
		op = extn_offset
	    } else {
		vfn[extn_offset-1] = EOS
		op = extn_offset - 1
	    }
	}

	call sfree (sp)
	return (op - 1)
end


# VFNCLOSE -- Close a VFN.  Update the VFN database if the MFD has been
# modified and updating is enabled.  Release the lock on the directory and
# return all storage.

procedure vfnclose (vfd, update_enable)

pointer	vfd			# VFN descriptor
int	update_enable		# update the database?

int	n_open_vfns, lastop, junk, len_struct
int	status
pointer	sp, fname, osfn, mfd

int	osfn_unlock(), osfn_timeleft()
int	vfnadd(), vfndel(), vvfn_checksum()
common	/vfncom/ n_open_vfns
errchk	osfn_unlock, osfn_timeleft, vfnadd, vfndel, syserrs
define	freemfd_ 91
define	freevfd_ 92
define	unlock_ 93

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn,  SZ_PATHNAME, TY_CHAR)

	# If the mapping file was never referenced or the database was not
	# modified in the MFD, just return buffers and quit.

	mfd = V_MFD(vfd)
	n_open_vfns = n_open_vfns - 1

	if (mfd == NULL)
	    goto freevfd_
	else if (M_MODIFIED(mfd) == NO || update_enable == VFN_NOUPDATE) {
	    if (V_ACMODE(vfd) == VFN_WRITE)
		goto unlock_
	    else
		goto freemfd_
	}

	# If we get here then the mapping file is open with write permission,
	# a transaction has been performed which modified the database, and we
	# were called with updating enabled.  If there is not enough time
	# remaining on the lock to permit the update, rollback (repeat) the
	# last transaction, otherwise update the database on disk.

	call osfn_pkfname (M_MAPFNAME(mfd), Memc[osfn], SZ_PATHNAME)

	while (osfn_timeleft (Memc[osfn], M_LOCKTIME(mfd)) < MIN_TIMELEFT) {
	    # Rollback transaction.  Hopefully it wont take so long this time
	    # (should only take a second or so).

	    junk = osfn_unlock (Memc[osfn], M_LOCKTIME(mfd))
	    lastop = M_LASTOP(mfd)
	    call mfree (mfd, TY_STRUCT)

	    switch (lastop) {
	    case V_ADD:
		junk = vfnadd (vfd, Memc[fname], SZ_PATHNAME)
	    case V_DEL:
		junk = vfndel (vfd, Memc[fname], SZ_PATHNAME)
	    }
	}

	# From here on we are committed.  Update and close the mapping file.
	# Add checksum to ensure correct reads.

	len_struct = LEN_MFD - (MAX_LONGFNAMES - M_NFILES(mfd)) *
	    (SZ_FNPAIR / SZ_STRUCT)
	M_CHECKSUM(mfd) = vvfn_checksum (Memi[mfd+1], (len_struct - 1) * SZ_INT)

	call zawrbf (M_CHAN(mfd), Memi[mfd], len_struct * SZ_STRUCT * SZB_CHAR,
	    long(1))
	call zawtbf (M_CHAN(mfd), status)
	if (status == ERR)
	    call syserrs (SYS_FWRITE, M_MAPFNAME(mfd))
unlock_
	call zclsbf (M_CHAN(mfd), status)
	if (status == ERR)
	    call syserrs (SYS_FCLOSE, M_MAPFNAME(mfd))

	# All done!  Unlock the directory.  If there are no files left in
	# the mapping file, delete the file and all lock files.

	call osfn_pkfname (M_MAPFNAME(mfd), Memc[osfn], SZ_PATHNAME)
	if (M_NFILES(mfd) == 0) {
	    call zfdele (Memc[osfn], junk)
	    call osfn_rmlock (Memc[osfn])
	} else if (osfn_unlock (Memc[osfn], M_LOCKTIME(mfd)) == ERR) {
	    iferr (call syserrs (SYS_FNOLOCK, M_MAPFNAME(mfd)))
		call erract (EA_WARN)
	}

freemfd_
	call mfree (mfd, TY_STRUCT)
freevfd_
	if (n_open_vfns > 0)
	    call mfree (vfd, TY_STRUCT)
	call sfree (sp)
end


# VVFN_READMAPFILE -- Open and read the mapping file.  In VFN_WRITE mode a
# new mapping file is created if necessary.

procedure vvfn_readmapfile (vfd)

pointer	vfd			# pointer to VFD descriptor

int	new_struct_size, checksum, file_exists, maxbytes, new_mapping_file
int	nbytes, len_file, junk, chan, ntrys, errnum, status
long	locktime
pointer	sp, mfd, fname, pkosfn

int	vvfn_checksum()
long	osfn_lock()
errchk	calloc, syserrs, osfn_lock, osfn_init
define	cleanup_ 91
define	reallynew_ 92

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (pkosfn, SZ_PATHNAME, TY_CHAR)

	call calloc (mfd, LEN_MFD, TY_STRUCT)
	V_MFD(vfd)	= mfd

	# Make OSFN of mapping file.  If the mode is VFN_UNMAP then the root
	# field, if any, is the filename of the directory containing the
	# mapping file.

	call strcpy (V_OSDIR(vfd), M_MAPFNAME(mfd), SZ_MAPFNAME)
	if (V_ACMODE(vfd) == VFN_UNMAP && V_LENROOT(vfd) > 0)
	    call zfsubd (M_MAPFNAME(mfd), SZ_MAPFNAME, V_ROOT(vfd), junk)
	call strcat (FNMAPPING_FILE, M_MAPFNAME(mfd), SZ_MAPFNAME)
	call strlwr (M_MAPFNAME(mfd))
	call osfn_pkfname (M_MAPFNAME(mfd), Memc[fname], SZ_PATHNAME)

	# Open or create mapping file.  Create must precede lock as lock will
	# abort if the file to be locked does not exist.  OSFN_LOCK will call
	# error if no write perm on directory.  If file locking is implemented
	# by host, open will return ERR if file is write locked by another
	# process, in which case we wait until the file can be opened.

	call zfacss (Memc[fname], 0, 0, file_exists)
	new_mapping_file = NO

	switch (V_ACMODE(vfd)) {
	case VFN_WRITE:
	    # Determine whether or not the mapping file exists.
	    call osfn_pkfname (M_MAPFNAME(mfd), Memc[pkosfn], SZ_PATHNAME)

	    if (file_exists == YES) {
		# Open an existing mapping file for exclusive access.
		iferr (locktime = osfn_lock (Memc[pkosfn])) {
		    call mfree (mfd, TY_STRUCT)
		    V_MFD(vfd) = NULL
		    call erract (EA_ERROR)
		}
		repeat {
		    call zopnbf (Memc[fname], READ_WRITE, chan)
		    if (chan == ERR)
			call zwmsec (1000)
		} until (chan != ERR || !OS_FILELOCKING)

	    } else {
		# Create a new mapping file and init the locks.
		new_mapping_file = YES
		call zopnbf (Memc[fname], NEW_FILE, chan)
		if (chan != ERR) {
		    call osfn_initlock (Memc[pkosfn])
		    locktime = osfn_lock (Memc[pkosfn])
		} else {
		    errnum = SYS_FOPEN
		    goto cleanup_
		}
	    }
	default:
	    if (file_exists == YES)
		call zopnbf (Memc[fname], READ_ONLY, chan)
	}

	if (file_exists == YES && chan == ERR) {
	    errnum = SYS_FOPEN
	    goto cleanup_
	}

	# Read mapping file into descriptor.  Repeat the read if the
	# checksum is invalid, indicating that our read occurred while
	# an update was in progress (locking need not lockout reads).

	if (file_exists == YES) {
	    ntrys = 0

	    repeat {
		# Read the file into the MFD.
		maxbytes = LEN_MFD * SZ_STRUCT * SZB_CHAR
		call zardbf (chan, Memi[mfd], maxbytes, long(1))
		call zawtbf (chan, nbytes)

		# The mapping file can be zero length if it was opened for
		# updating but never written into.

		if (nbytes == 0)
		    goto reallynew_

		len_file = nbytes / SZB_CHAR / SZ_STRUCT
		if (len_file < MIN_LENMFD) {
		    errnum = SYS_FREAD
		    goto cleanup_
		}

		# The checksum excludes the checksum field of MFD, but the
		# entire MFD is written to the mapping file.  Note that the
		# file will contain garbage at the end following a file
		# deletion (the file list gets shorter but the file does not).
		# Compute checksum using only the valid file data, since that
		# is how it is computed when the file is updated.

		len_file = LEN_MFD - (MAX_LONGFNAMES - M_NFILES(mfd)) *
		    (SZ_FNPAIR / SZ_STRUCT)
		checksum = vvfn_checksum (Memi[mfd+1], (len_file-1) * SZ_INT)

		ntrys = ntrys + 1
	    } until (checksum == M_CHECKSUM(mfd) || ntrys > MAX_READS)

	    if (ntrys > MAX_READS) {
		errnum = SYS_FVFNCHKSUM
		goto cleanup_
	    }
	}

reallynew_

	# Close the mapping file if it is never going to be updated, and return
	# any unused space in the mapping file descriptor.

	if (V_ACMODE(vfd) != VFN_WRITE) {
	    if (file_exists == YES) {
		call zclsbf (chan, status)
		if (status == ERR)
		    call syserrs (SYS_FCLOSE, M_MAPFNAME(mfd))
	    }
	    new_struct_size = LEN_MFD -
		(MAX_LONGFNAMES - M_NFILES(mfd)) * (SZ_FNPAIR/SZ_STRUCT)
	    call realloc (mfd, new_struct_size, TY_STRUCT)
	    V_MFD(vfd) = mfd
	} else {
	    M_CHAN(mfd) = chan
	    M_LOCKTIME(mfd) = locktime
	}

	call sfree (sp)
	return

cleanup_
	call strcpy (M_MAPFNAME(mfd), Memc[fname], SZ_PATHNAME)
	call mfree (mfd, TY_STRUCT)
	V_MFD(vfd) = NULL
	call syserrs (errnum, Memc[fname])
end


# VFN_ENTER -- Add a new filename pair to the mapping file.  The VFN was not
# found in the database but that does not mean that there is not already an
# occurrence of the OSFN in the database and in the directory; if the OSFN is
# already in use, the filename is degenerate.  If the OSFN exists in the
# directory then create ".zmd" degeneracy file and generate a unique OSFN,
# adding a new VFN,OSFN pair to the database.

int procedure vfn_enter (vfd, osfn, maxch)

pointer	vfd			# pointer to VFN descriptor
char	osfn[maxch]		# packaged OS filename (in/out)
int	maxch

int	file_exists, op, ndigits, m, n, num, offset, fn
pointer	sp, fname, numbuf, mfd
int	gstrcpy(), itoc()
errchk	syserrs

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (numbuf, MAX_DIGITS, TY_CHAR)

	# Generate the first attempt at the OSFN of the new file.

	op = gstrcpy (V_OSDIR(vfd), Memc[fname], SZ_PATHNAME)
	op = op + gstrcpy (V_ROOT(vfd), Memc[fname+op], SZ_PATHNAME-op)
	if (V_LENEXTN(vfd) > 0) {
	    Memc[fname+op] = EXTN_DELIMITER
	    op = op + 1
	    call strcpy (V_EXTN(vfd), Memc[fname+op], SZ_PATHNAME-op)
	}
	offset = V_LENOSDIR(vfd) + 1

	# Determine if a file already exists with the new OSFN.  If so we
	# must flag the file as degenerate and generate a unique OSFN.

	call osfn_pkfname (Memc[fname], osfn, maxch)
	call zfacss (osfn, 0, 0, file_exists)

	if (file_exists == YES) {
	    # Set flag to create degeneracy flag file at update time.
	    M_ADDZMD(mfd) = YES

	    # Generate a unique OSFN for the new file.  This is done by
	    # overwriting the 2nd and following characters of the root with
	    # a number until a unique name results.  Nines are preferred as
	    # they occur least frequently in ordinary filenames.

	    for (m=0;  file_exists == YES && m * 10 < MAX_DEGENERACY;  m=m+1)
		for (n=9;  file_exists == YES && n >= 0;  n=n-1) {
		    num = m * 10 + n
		    ndigits = itoc (num, Memc[numbuf], MAX_DIGITS)
		    call amovc (Memc[numbuf], Memc[fname+offset], ndigits)
		    call osfn_pkfname (Memc[fname], osfn, maxch)
		    call zfacss (osfn, 0, 0, file_exists)
		}

	    if (m * 10 >= MAX_DEGENERACY)
		call syserrs (SYS_FDEGEN, Memc[fname])
	}

	# Add the filename pair to the database.  The directory prefix is
	# omitted.  If we run out of room in the mapping file we just abort.

	mfd = V_MFD(vfd)
	fn = M_NFILES(mfd) + 1
	M_NFILES(mfd) = fn
	if (fn > MAX_LONGFNAMES)
	    call syserrs (SYS_FTMLONGFN, Memc[fname])

	# Save the VFN and OSFN, minus the directory prefix, in the mapping
	# file structure.

	call strcpy (V_VFN(vfd),  FN_VFN(mfd,fn),  SZ_VFNFN)
	call strcpy (Memc[fname+offset-1], FN_OSFN(mfd,fn), SZ_VFNFN)

	M_LASTOP(mfd) = V_ADD
	M_MODIFIED(mfd) = YES

	call sfree (sp)
	return (OK)
end


# VFN_GETOSFN -- Search the MFD file list for the named VFN, and if found
# return the assigned OSFN as an output argument and the length of the OSFN
# string as the function value.  ERR is returned if the entry cannot be found.
# The OSFN includes the OSDIR prefix.

int procedure vfn_getosfn (vfd, vfn, osfn, maxch)

pointer	vfd			# VFN descriptor
char	vfn[ARB]		# virtual filename to be searched for
char	osfn[maxch]		# receives unpacked OSFN
int	maxch

char	first_char
int	fn, op
pointer	mfd, vfnp
bool	streq()
int	gstrcpy()

begin
	mfd = V_MFD(vfd)
	vfnp = M_FNMAP(mfd)
	first_char = vfn[1]

	do fn = 1, M_NFILES(mfd) {
	    if (Memc[vfnp] == first_char)
		if (streq (Memc[vfnp], vfn)) {
		    op = gstrcpy (V_OSDIR(vfd), osfn, maxch) + 1
		    op = op + gstrcpy (FN_OSFN(mfd,fn), osfn[op], maxch-op+1)
		    return (op - 1)
		}
	    vfnp = vfnp + SZ_FNPAIR
	}

	return (ERR)
end


# VVFN_CHECKSUM -- Compute the integer checksum of a char array.

int procedure vvfn_checksum (a, nchars)

char	a[nchars]		# array to be summed
int	nchars			# length of array
int	i, sum

begin
	sum = 0
	do i = 1, nchars
	    sum = sum + a[i]

	return (sum)
end
