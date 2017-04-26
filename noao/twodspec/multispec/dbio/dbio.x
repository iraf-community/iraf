
include	<fset.h>
include	<error.h>
include "dbio.h"

.help dbio 2 "Subset Database I/O Procedures"
.sh
1. Introduction

     These DBIO procedures are a subset of the general
DBIO design described in "Specifications of the IRAF DBIO Interface" by
Doug Tody (Oct 1983).  It is designed to allow programs written using
the subset DBIO to be easily converted to the full DBIO.  It's features
are:
.ls 4 1.
Database open and close.
.le
.ls 4 2.
Reference to entries by a (possibly) subscripted record name string.
.le
.ls 4 3.
Ability to add new record types as desired.
.le
.ls 4 4.
Error recovery procedure to cleanup after an uncaught error.
.le

The primary limitations are:
.ls 4 1.
No aliases.
.le
.ls 4 2.
No datatyping and no self-describing structures.
.le
.ls 4 3.
No deletions of entries.
.le
.sh
2. Procedures

.nf
		db = dbopen (file_name, mode, max_entries)
		db = dbreopen (db)
		     dbclose (db)
		     dbenter (db, record_name, sz_elem, nreserve)
	       y/n = dbaccess (db, record_name)
	     nread = dbread (db, reference, buf, maxelems)
	  	     dbwrite (db, reference, buf, nelems)
.fi

     A new, empty database is created by opening with access modes NEW_FILE
or TEMP_FILE.  The dictionary will be intialized to allow max_entries
number of dictionary entries.  The other legal access modes are READ_ONLY and
READ_WRITE.  The max_entries argument is not used with these modes.  To create
a new entry in the database the name of the record, the size of a record
element, and the maximum number of such records to be stored are specified.
This differs from the full DBIO specification in that a record is described
only by a size instead of a datatype.  Also it is not possible to increase
the number of elements once it has been entered.  The database read and
write procedures do no type conversion.  They read procedure returns
the number of elements read.  If a reference is not found in the
dictionary in either reading or writing an error condition occurs.
Also an attempt to read or write an element exceeding the dimension
entered in the dictionary will create an error condition.
.endhelp


# DBOPEN, DBREOPEN -- Open a database file.

pointer procedure dbopen (file_name, ac_mode, db_nentries)

# Procedure dbopen parameters:
char	file_name[SZ_FNAME]		# Database filename
int	ac_mode				# Access mode (new,temp,r,rw)
int	db_nentries			# Creation dictionary size

# Entry dbreopen parameters:
pointer	dbreopen			# Function type
pointer	db_old				# Old database descriptor

int	mode
pointer	fd, db				# FIO descriptor and DBIO descriptor
pointer	dic
int	nelem, nentries

bool	strne()
int	open(), dbread(), reopen()
errchk	db_getdes, calloc, dbenter, dbread, db_init

begin
	# Check for valid access mode.  Valid modes require read permission.
	# If a valid access mode open database with FIO.
	mode = ac_mode
	if ((mode == WRITE_ONLY) || (mode == APPEND))
	    call error (DB_ERRCODE + 0, "Invalid database access mode")
	fd = open (file_name, mode, BINARY_FILE)
	goto 10

entry dbreopen (db_old) 

	fd = reopen (DB_FD(db_old), mode)

	# Get DBIO descriptor
10	call db_getdes (db)
	DB_FD(db) = fd

	# If the database is being created enter the dictionary in the file.
	# If the database already exists read the current dictionary and
	# check to see if the file is a database.
	switch (mode) {
	case NEW_FILE, TEMP_FILE:
	    # Allocate dictionary space and enter it in the database.
	    # The request entries is increased by one for the dictionary
	    # database entry itself.
	    nentries = db_nentries + 1
	    call calloc (dic, nentries * LEN_DB_ENTRY, TY_STRUCT)
	    DB_DIC(db) = dic
	    call dbenter (db, "db_dictionary", LEN_DB_ENTRY * SZ_STRUCT,
		nentries)
	case READ_ONLY, READ_WRITE:
	    # Read dictionary.
	    call db_init (db, 1)
	    dic = DB_DIC(db)
	    nelem = dbread (db, "db_dictionary", Memi[dic], 1)
	    if (nelem != 1)
		call error (DB_ERRCODE + 1, "Error reading database dictionary")
	    if (strne (DB_KEY(dic, 1), "db_dictionary"))
		call error (DB_ERRCODE + 2, "File is not a database")

	    nentries = DB_DIM(dic, 1)
	    call db_init (db, nentries)
	    dic = DB_DIC(db)
	    nelem = dbread (db, "db_dictionary", Memi[dic], nentries)
	    if (nelem != nentries)
		call error (DB_ERRCODE + 3, "Error reading database dictionary")
	}

	return (db)
end


# DB_INIT -- Initialize the program dictionary space

procedure db_init (db, db_nentries)

pointer	db
int	db_nentries

pointer	dic

long	note()
errchk	mfree, calloc, seek

begin
	# Allocate dictionary memory
	dic = DB_DIC(db)
	if (dic != NULL)
	    call mfree (dic, TY_STRUCT)
	call calloc (dic, db_nentries * LEN_DB_ENTRY, TY_STRUCT)
	DB_DIC(db) = dic

	# Fill in dictionary entry
	call strcpy ("db_dictionary", DB_KEY(dic, 1), SZ_DB_KEY)
	DB_SZ_ELEM(dic, 1) = LEN_DB_ENTRY * SZ_STRUCT
	DB_DIM(dic, 1) = db_nentries
	call seek (DB_FD(db), BOF)
	DB_OFFSET(dic, 1) = note (DB_FD(db))
end

# DBENTER -- Make a new entry in the database dictionary and reserve
# file space in the database.

procedure dbenter (db, record_name, sz_elem, nreserve)

pointer	db				# DBIO descriptor
char	record_name[SZ_DB_KEY]		# Record name string
int	sz_elem				# Size of record element in CHARS
int	nreserve			# Number of record elements to reserve

int	i
int	sz_reserve, sz_buf
pointer	dic, buf

bool	streq()
int	fstati()
long	note()

errchk	calloc, dbclose, write, seek

begin
	# Check access mode
	if (fstati(DB_FD(db), F_WRITE) == NO)
	    call error (DB_ERRCODE + 4, "Database is read only")

	# Find the last entry.  Check for attempts to redefine an
	# entry and to overflow the dictionary.
	dic = DB_DIC(db)
	for (i = 1; i <= DB_DIM(dic, 1); i = i + 1) {
	    if (DB_DIM(dic, i) == 0)
		break
	    if (streq (record_name, DB_KEY(dic, i)))
		call error (DB_ERRCODE + 5, "Attempt to redefine dictionary entry")
	}
	if ((i > 1) && (i > DB_DIM(dic, 1)))
	    call error (DB_ERRCODE + 6, "Database dictionary is full")

	# Make dictionary entry
	call strcpy (record_name, DB_KEY(dic, i), SZ_DB_KEY)
	DB_SZ_ELEM(dic, i) = sz_elem
	DB_DIM(dic, i) = nreserve
	call seek (DB_FD(db), EOF)
	DB_OFFSET(dic, i) = note (DB_FD(db))
	DB_UPDATE(db) = YES

	# Initialize file space to zero.  Zero file blocks for efficiency.
	sz_reserve = sz_elem * nreserve
	sz_buf = min (fstati (DB_FD(db), F_BLKSIZE), sz_reserve)
	call calloc (buf, sz_buf, TY_CHAR)

	while (sz_reserve > 0) {
	    call write (DB_FD(db), Memc[buf], sz_buf)
	    sz_reserve = sz_reserve - sz_buf
	    sz_buf = min (sz_buf, sz_reserve)
	}
	call mfree (buf, TY_CHAR)
end

# DBACCESS -- Is data reference in the database?

bool procedure dbaccess (db, record_name)

pointer	db				# DBIO descriptor
char	record_name[SZ_DB_KEY]		# Record name string

int	i
pointer	dic

bool	streq()

begin
	dic = DB_DIC(db)
	for (i = 1; i <= DB_DIM(dic, 1); i = i + 1) {
	    if (DB_DIM(dic, i) == 0)
		return (FALSE)
	    if (streq (record_name, DB_KEY(dic, i)))
		return (TRUE)
	}
	return (FALSE)
end


# DBNEXTNAME -- Return name of the next dictionary entry.

int procedure dbnextname (db, previous, outstr, maxch)

pointer	db				# DBIO descriptor
char	previous[ARB]
char	outstr[ARB]
int	maxch

int	i
pointer	dic

bool	streq(), strne()

begin
	dic = DB_DIC(db)
	i = 1
	if (strne (previous, "")) {
	    for (; i <= DB_DIM(dic, 1); i = i + 1) {
	        if (DB_DIM(dic, i) == 0)
		    return (EOF)
	        if (streq (previous, DB_KEY(dic, i)))
		    break
	    }
	}
	i = i + 1
	if ((i > DB_DIM(dic, 1)) || (DB_DIM(dic, i) == 0))
	    return (EOF)
	else
	    call strcpy (DB_KEY(dic, i), outstr, maxch)

	return (OK)
end


#DBREAD - Read data from the database.
# The number of data elements read is returned.

int procedure dbread (db, ref, buf, maxelems)

pointer	db			# Database file descriptor
char	ref[ARB]		# Data reference
char	buf[ARB]		# Data buffer
int	maxelems		# Number of elements to be read

int	i, j
int	stat, sz_elem, index, nread
long	offset
pointer	dic

int	strncmp(), strlen(), stridxs(), ctoi()
bool	streq()
int	read()
errchk	read, dbclose

begin
	dic = DB_DIC(db)

	# Decode the data reference and set the file offset and the size
	# of the data element.  If a valid data reference is not found
	# then a read status of 0 is returned.

	j = stridxs ("[", ref)
	for (i = 1; i <= DB_DIM(dic, 1); i = i + 1) {
	    if (DB_DIM(dic, i) == 0)
		call error (DB_ERRCODE + 7, "Database request not found")
	    if (j == 0) {
	        if (streq (ref, DB_KEY(dic, i)))
		    break
	    } else {
		if (strlen (DB_KEY(dic, i)) == j - 1)
		    if (strncmp (ref, DB_KEY(dic, i), j - 1) == 0)
		        break
	    }
	}

	offset = DB_OFFSET(dic, i)
	sz_elem = DB_SZ_ELEM(dic, i)
	nread = maxelems
	if (j > 0) {
	    j = ctoi (ref, j + 1, index)
	    if (j > 0) {
		if (maxelems > DB_DIM(dic, i) - index + 1) {
		    call error (DB_ERRCODE + 8, "Database request out of bounds")
		}
		offset = offset + (index - 1) * sz_elem
	    }
	}

	# Seek and read the data
	call seek (DB_FD(db), offset)
	stat = read (DB_FD(db), buf, sz_elem * nread) / sz_elem
	return (stat)
end


# DBWRITE - Write data to the database.

procedure dbwrite (db, ref, buf, nelems)

pointer	db			# DBIO descriptor
char	ref[ARB]		# Data reference
char	buf[ARB]		# Data buffer
int	nelems			# Number of elements to written

int	i, j
int	sz_elem, index, nwritten
long	offset
pointer	dic

int	strncmp(), strlen(), stridxs(), ctoi()
bool	streq()
errchk	write, dbclose

begin
	dic = DB_DIC(db)

	# Decode the data reference and set the file offset and the size
	# of the data element.  If a valid data reference is not found
	# then the data is not written and a write status of 0 is returned.

	j = stridxs ("[", ref)
	for (i = 1; i <= DB_DIM(dic, 1); i = i + 1) {
	    if (DB_DIM(dic, i) == 0)
		call error (DB_ERRCODE + 9, "Database request not found")
	    if (j == 0) {
	        if (streq (ref, DB_KEY(dic, i)))
		    break
	    } else {
		if (strlen (DB_KEY(dic, i)) == j - 1)
		    if (strncmp (ref, DB_KEY(dic, i), j - 1) == 0)
		        break
	    }
	}

	offset = DB_OFFSET(dic, i)
	sz_elem = DB_SZ_ELEM(dic, i)
	nwritten = nelems
	if (j > 0) {
	    j = ctoi (ref, j + 1, index)
	    if (j > 0) {
		if (nelems > DB_DIM(dic, i) - index + 1) {
		    call error (DB_ERRCODE + 10, "Database request out of bounds")
		}
		offset = offset + (index - 1) * sz_elem
	    }
	}

	# Seek and write the data
	call seek (DB_FD(db), offset)
	call write (DB_FD(db), buf, sz_elem * nwritten)
	return
end


# DBCLOSE -- Update the dictionary in the database, close the database
# and free DBIO descriptor.

procedure dbclose (db)

pointer	db

begin
	# Update dictionary in database
	if (DB_UPDATE(db) == YES)
	    call dbwrite (db, "db_dictionary", Memi[DB_DIC(db)],
		DB_DIM(DB_DIC(db), 1))

	call close (DB_FD(db))
	call db_freedes (db)
end


# Procedures accessing the DBIO descriptor list.
#
# DB_GETDES  -- Allocate and return a DBIO descriptor.  Post error recovery.
# DB_FREEDES -- Close a database and free allocated memory.
# DB_ERROR   -- Take error recovery action by closing all open databases.

procedure db_getdes (db)

pointer	db			# Allocated DBIO descriptor

extern	db_error()
errchk	malloc()

int	ndes			# Number of allocated DBIO descriptors
pointer	dbdes[MAX_DB_DES]	# DBIO descriptor list

common	/dbiocom/ ndes, dbdes

int	init
data	init/YES/

begin
	if (init == YES) {
	    ndes = 0
	    init = NO
	}

	# Check to see if the requested descriptor would overflow the descriptor
	# list.  If not allocate memory for the descriptor otherwise
	# start error handling.  On the first call post the error handler.

	if (ndes == MAX_DB_DES)
	    call error (DB_ERRCODE + 11, "Attempt to open too many database files")

	ndes = ndes + 1
	call malloc (dbdes[ndes], LEN_DB_DES, TY_STRUCT)
	db = dbdes[ndes]
	DB_FD(db) = NULL
	DB_DIC(db) = NULL
	DB_UPDATE(db) = NO

	if (ndes == 1)
	    call onerror (db_error)
end


# DB_FREEDES -- Close a database and free allocated memory.

procedure db_freedes (db)

pointer	db			# DBIO descriptor to be freed

int	i

int	ndes			# Number of allocated DBIO descriptors
pointer	dbdes[MAX_DB_DES]	# DBIO descriptor list

common	/dbiocom/ ndes, dbdes

begin

	# Locate the specified descriptor in the descriptor list.
	# If the descriptor is not in the list do nothing.
	# If the descriptor is in the list free allocated
	# memory and remove the entry from the list.

	for (i = 1; (i <= ndes) && (db != dbdes[i]); i = i + 1)
	    ;
	if (i > ndes)
	    return

	if (DB_DIC(db) != NULL)
	    call mfree (DB_DIC(db), TY_STRUCT)
	call mfree (db, TY_STRUCT)

	if (i < ndes)
	    dbdes[i] = dbdes[ndes]
	ndes = ndes - 1
end


# DB_ERROR   -- Take error recovery action by closing all open databases.

procedure db_error (error_code)

int	error_code		# Error code

int	i

int	ndes			# Number of allocated DBIO descriptors
pointer	dbdes[MAX_DB_DES]	# DBIO descriptor list

common	/dbiocom/ ndes, dbdes

begin
	# Let fio_cleanup deal with the open files and the system
	# restart deal with freeing the stack.  This procedure
	# cleans up the dbio descriptors and updates the database
	# dictionary.

	do i = 1, ndes
	    # Update dictionary in database.  Catch errors.
	    if (DB_UPDATE(dbdes[i]) == YES)
	        iferr (call dbwrite (dbdes[i], "db_dictionary",
		    Memi[DB_DIC(dbdes[i])], DB_DIM(DB_DIC(dbdes[i]), 1)))
	            call erract (EA_WARN)

	    call db_freedes (dbdes[i])
end


int procedure dbgeti (db, key, type)

pointer	db
char	key[ARB]
char	type[ARB]

int	i
pointer	dic

bool	streq()

begin
	dic = DB_DIC(db)
	for (i = 1; i <= DB_DIM(dic, 1); i = i + 1) {
	    if (DB_DIM(dic, i) == 0)
		call error (0, "Key not in database")
	    if (streq (key, DB_KEY(dic, i)))
		break
	}
	if (i > DB_DIM(dic, 1))
	    call error (0, "Key not in database")
	
	if (streq (type, "r_len"))
	    return (DB_DIM(dic, i))
	else if (streq (type, "r_size"))
	    return (DB_SZ_ELEM(dic, i))
	else
	    call error (0, "Unknown database key attribute")
end
