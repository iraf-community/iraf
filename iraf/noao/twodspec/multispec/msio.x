include	<error.h>
include	<imhdr.h>
include "ms.h"

# MSIO -- MULTISPEC interface to DBMS.
#
# MSMAP		-- Map a MULTISPEC database.
# MSUNMAP	-- Close MULTISPEC database and free MSIO memory allocation.
# MSGDES	-- Allocate and return a MSIO descriptor.  Post error recovery.
# MS_FREE_DES	-- Close a database and free allocated memory.
# MS_ERROR	-- Take error recovery action by closing all open databases.


# MSMAP -- Map a MULTISPEC database.
#
# The database name is formed by adding the extension '.db' to the image.
#
# For a new database:
#	Create the database, make entries for the header and comments,
#	allocate memory for the header and comments and return MSIO descriptor.
# For an existing database:
#	Open the database, allocate memory and read the header, comments, and
#	sample line array, and return MSIO descriptor.

pointer procedure msmap (image, mode, max_entries)

# Procedure msmap parameters:
char	image[ARB]			# Image
int	mode				# Access mode for database
int	max_entries			# Maximum number of entries

char	database[SZ_FNAME]		# MULTISPEC database filename
pointer	db, ms

pointer	dbopen()

begin
	# Create the database filename.
	call sprintf (database, SZ_FNAME, "%s.db")
	    call pargstr (image)

	# Open the database with specified mode and max_entries.
	db = dbopen (database, mode, max_entries)

	# Get an MSIO descriptor.
	call msgdes (ms)
	MS_DB(ms) = db

	if (mode == NEW_FILE) {
	    # For a NEW_FILE enter the header and comment records and
	    # call msghdr and msgcomments to allocate memory.
	    call dbenter (db, NAME(ms, HDR), LEN_MS_HDR * SZ_STRUCT, 1)
	    call dbenter (db, NAME(ms, COMMENTS), SZ_MS_COMMENTS + 1, 1)
	    call msghdr (ms)
	    call msgcomments (ms)
	} else {
	    # For an existing database read the header, comments, and
	    # sample line array.
	    call msghdr (ms)
	    call msgcomments (ms)
	    call msgsample (ms)
	}

	# Return MSIO descriptor.
	return (ms)
end


# MSUNMAP -- Close MULTISPEC database and free MSIO memory allocation.

procedure msunmap (ms)

pointer	ms			# MSIO descriptor

begin
	call dbclose (MS_DB(ms))
	call ms_free_des (ms)
end


# Procedures accessing the MSIO descriptor list.
#
# MSGDES  -- Allocate and return a MSIO descriptor.  Post error recovery.
# MS_FREE_DES -- Close a database and free allocated memory.
# MS_ERROR   -- Take error recovery action by closing all open databases.

procedure msgdes (ms)

pointer	ms			# MSIO descriptor

int	init

extern	ms_error()

int	ndes			# Number of allocated MSIO descriptors
pointer	msdes[MS_MAX_DES]	# MSIO descriptor list

common	/msiocom/ ndes, msdes

data	init/YES/

begin
	# Initialize and post error recovery.
	if (init == YES) {
	    ndes = 0
	    call onerror (ms_error)
	    init = NO
	}

	# Check if requested descriptor would overflow the descriptor list.
	if (ndes == MS_MAX_DES)
	    call error (MS_ERROR, "Attempt to open too many MULTISPEC files")

	# Allocate memory for the descriptor and enter in pointer in list.
	ndes = ndes + 1
	call malloc (msdes[ndes], LEN_MS_DES, TY_STRUCT)
	ms = msdes[ndes]

	# Initialize descriptor to NULL.
	call amovki (NULL, Memi[ms], LEN_MS_DES)

	# Initialize the MULTISPEC database name list.
	call msnames (ms)
end

# MS_FREE_DES -- Close a database and free allocated memory.

procedure ms_free_des (ms)

pointer	ms			# MSIO descriptor to be freed

int	i, j

int	ndes			# Number of allocated MSIO descriptors
pointer	msdes[MS_MAX_DES]	# MSIO descriptor list

common	/msiocom/ ndes, msdes

begin
	# Locate the specified descriptor in the descriptor list.
	# If the descriptor is not in the list do nothing.
	# If the descriptor is in the list free allocated memory and remove
	# the entry from the list.

	for (i = 1; (i <= ndes) && (ms != msdes[i]); i = i + 1)
	    ;
	if (i > ndes)
	    return

	call mfree (MS_DATA(ms, HDR), TY_STRUCT)
	call mfree (MS_DATA(ms, COMMENTS), TY_CHAR)
	call mfree (MS_DATA(ms, SAMPLE), TY_INT)
	call mfree (MS_DATA(ms, I0), TY_REAL)
	call mfree (MS_DATA(ms, X0), TY_REAL)
	call mfree (MS_DATA(ms, S0), TY_REAL)
	call mfree (MS_DATA(ms, S1), TY_REAL)
	call mfree (MS_DATA(ms, S2), TY_REAL)
	if (MS_DATA(ms, X0_FIT) != NULL) {
	    do j = 1, MS_NSPECTRA(ms)
	        if (CV(ms, X0_FIT, j) != NULL)
		    call cvfree (CV(ms, X0_FIT, j))
	    call mfree (MS_DATA(ms, X0_FIT), TY_INT)
	}
	call mfree (ms, TY_STRUCT)

	if (i < ndes)
	    msdes[i] = msdes[ndes]
	ndes = ndes - 1
end

# MS_ERROR   -- Take error recovery action by closing all open databases.

procedure ms_error (error_code)

int	error_code		# Error code for error recovery

int	i, ndes1

int	ndes			# Number of allocated MSIO descriptors
pointer	msdes[MS_MAX_DES]	# MSIO descriptor list

common	/msiocom/ ndes, msdes

begin
	# Let DBMS deal with the database descriptor,
	# fio_cleanup deal with the open files, and the system
	# restart deal with freeing the stack.  This procedure
	# cleans up the msio descriptors and memory allocations.
	# The system may eventually deal with heap memory recovery.

	ndes1 = ndes
	do i = 1, ndes1
	    call ms_free_des (msdes[i])
end
