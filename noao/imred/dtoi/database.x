include	<time.h>
include	<ctype.h>
include	<ctotok.h>
include	<finfo.h>

# DATABASE.X -- This file contains source for the database utilities used by
# the DTOI package.  They are based on Frank Valdes's dtext utilities.

# Definition for dbtext structure.

define	DT_LEN		5

define	DT		Memi[$1]	# FIO channel
define	DT_NRECS	Memi[$1+1]	# Number of records
define	DT_OFFSETS	Memi[$1+2]	# Pointer to record offsets
define	DT_NAMES	Memi[$1+3]	# Pointer to name indices
define	DT_MAP		Memi[$1+4]	# Pointer to record names

define	DT_OFFSET	Meml[DT_OFFSETS($1)+$2-1]
define	DT_NAMEI	Memi[DT_NAMES($1)+$2-1]
define	DT_NAME		Memc[DT_MAP($1)+DT_NAMEI($1,$2)]

define	DT_ALLOC	20		# Allocation block size


# DDB_MAP -- Map the database.

pointer procedure ddb_map (database, mode)

char	database[ARB]			# Database file
int	mode				# FIO mode

int	i, nrec
int	dt_alloc1, dt_alloc2
pointer	db, str

int	open(), fscan(), strlen()
bool	streq()
long	note()
errchk	delete, open

begin
	call calloc (db, DT_LEN, TY_STRUCT)

	if (mode == NEW_FILE)
	    iferr (call delete (database))
		;

	DT(db) = open (database, mode, TEXT_FILE)

	if (mode != READ_ONLY)
	    return (db)

	dt_alloc1 = DT_ALLOC
	dt_alloc2 = DT_ALLOC * SZ_LINE
	call malloc (DT_OFFSETS(db), dt_alloc1, TY_LONG)
	call malloc (DT_NAMES(db), dt_alloc1, TY_INT)
	call malloc (DT_MAP(db), dt_alloc2, TY_CHAR)
	call malloc (str, SZ_LINE, TY_CHAR)

	nrec = 1
	DT_NRECS(db) = 0
	DT_NAMEI(db, nrec) = 0

	while (fscan (DT(db)) != EOF) {
	    call gargwrd (DT_NAME(db, nrec), SZ_LINE)

	    if (streq (DT_NAME(db, nrec), "begin")) {
		call gargstr (Memc[str], SZ_LINE)
		for (i=str; IS_WHITE(Memc[i]); i=i+1)
		    ;
		call strcpy (Memc[i], DT_NAME(db, nrec), SZ_LINE)

		for (i = 1; i < nrec; i = i + 1)
		    if (streq (DT_NAME(db, i), DT_NAME(db, nrec)))
			break

		if (i < nrec)
		    DT_OFFSET(db, i) = note (DT(db))
		else {
		    DT_NRECS(db) = nrec
		    DT_OFFSET(db, nrec) = note (DT(db))
		    DT_NAMEI(db, nrec+1) = DT_NAMEI(db, nrec) +
		        strlen (DT_NAME(db, nrec)) + 1
		    nrec = nrec + 1
		}

		if (nrec == dt_alloc1) {
		    dt_alloc1 = dt_alloc1 + DT_ALLOC
		    call realloc (DT_OFFSETS(db), dt_alloc1, TY_LONG)
		    call realloc (DT_NAMES(db), dt_alloc1, TY_INT)
	        }

	        if (DT_NAMEI(db, nrec) + SZ_LINE >= dt_alloc2) {
		    dt_alloc2 = dt_alloc2 + DT_ALLOC * SZ_LINE
		    call realloc (DT_MAP(db), dt_alloc2, TY_CHAR)
		}
	    }
	}

	call realloc (DT_MAP(db), DT_NAMEI(db, nrec), TY_CHAR)
	call realloc (DT_OFFSETS(db), DT_NRECS(db), TY_LONG)
	call realloc (DT_NAMES(db), DT_NRECS(db), TY_INT)

	return (db)
end


# DDB_UNMAP -- Unmap the database.

procedure ddb_unmap (db)

pointer	db				# Database file descriptor

begin
	call close (DT(db))
	call mfree (DT_MAP(db), TY_CHAR)
	call mfree (DT_OFFSETS(db), TY_LONG)
	call mfree (DT_NAMES(db), TY_INT)
	call mfree (db, TY_STRUCT)
end


# DDB_PREC -- Write a record to the database.

procedure ddb_prec (db, record)

pointer	db		# Pointer to database
char	record[ARB]	# Name of record to enter

pointer	sp, entry

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "begin\t%s\n")
	    call pargstr (record)

	call fprintf (DT(db), Memc[entry])

	call sfree (sp)
end


# DDB_PUTR -- Write a real valued field into the current record.

procedure ddb_putr (db, field, value)

pointer	db		# Pointer to database
char	field[ARB]	# Format string
real	value		# Real value to output

pointer	sp, entry

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%g\n")
	    call pargstr (field)
	    call pargr (value)
	
	call fprintf (DT(db), Memc[entry])

	call sfree (sp)
end

# DDB_PUTD -- Write a double valued field into the current record.

procedure ddb_putd (db, field, value)

pointer	db		# Pointer to database
char	field[ARB]	# Format string
double	value		# Real value to output

pointer	sp, entry

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%g\n")
	    call pargstr (field)
	    call pargd (value)
	
	call fprintf (DT(db), Memc[entry])

	call sfree (sp)
end


# DDB_PUTI -- Write an integer valued field into the current record.

procedure ddb_puti (db, field, value)

pointer	db		# Pointer to database
char	field[ARB]	# Format string
int	value		# Integer value to output

pointer	sp, entry

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%d\n")
	    call pargstr (field)
	    call pargi (value)
	
	call fprintf (DT(db), Memc[entry])

	call sfree (sp)
end


# DDB_PSTR -- Write a string valued field into the current record.

procedure ddb_pstr (db, field, value)

pointer	db		# Pointer to database
char	field[ARB]	# Format string
char	value[ARB]	# String field

pointer	sp, entry

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%s\n")
	    call pargstr (field)
	    call pargstr (value)
	
	call fprintf (DT(db), Memc[entry])

	call sfree (sp)
end


# DDB_PAR -- Put an array of real values into a field in the current record.

procedure ddb_par (db, field, array, nvals)

pointer	db		# Pointer to database structure
char	field[ARB]	# Name of field to be added
real	array[nvals]	# Array of real values 
int	nvals		# Number of values in array

pointer	sp, entry
int	i

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%d\n")
	    call pargstr (field)
	    call pargi (nvals)

	call fprintf (DT(db), Memc[entry])

	do i = 1, nvals {
	    call sprintf (Memc[entry], SZ_LINE, "\t\t%g\n")
		call pargr (array[i])
	    
	    call fprintf (DT(db), Memc[entry])
	}

	call sfree (sp)
end


# DDB_PAD -- Put an array of double values into a field in the current record.

procedure ddb_pad (db, field, array, nvals)

pointer	db		# Pointer to database structure
char	field[ARB]	# Name of field to be added
double	array[nvals]	# Array of double values 
int	nvals		# Number of values in array

pointer	sp, entry
int	i

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%d\n")
	    call pargstr (field)
	    call pargi (nvals)

	call fprintf (DT(db), Memc[entry])

	do i = 1, nvals {
	    call sprintf (Memc[entry], SZ_LINE, "\t\t%g\n")
		call pargd (array[i])
	    
	    call fprintf (DT(db), Memc[entry])
	}

	call sfree (sp)
end


# DDB_PAI -- Put an array of integer values into a field in the current 
# record.

procedure ddb_pai (db, field, array, nvals)

pointer	db		# Pointer to database structure
char	field[ARB]	# Name of field to be added
int	array[nvals]	# Array of integer values 
int	nvals		# Number of values in array

pointer	sp, entry
int	i

begin
	call smark (sp)
	call salloc (entry, SZ_LINE, TY_CHAR)

	call sprintf (Memc[entry], SZ_LINE, "\t%s\t%d\n")
	    call pargstr (field)
	    call pargi (nvals)

	call fprintf (DT(db), Memc[entry])

	do i = 1, nvals {
	    call sprintf (Memc[entry], SZ_LINE, "\t\t%d\n")
		call pargi (array[i])
	    
	    call fprintf (DT(db), Memc[entry])
	}

	call sfree (sp)
end


# DDB_LOCATE -- Locate a database record.

int procedure ddb_locate (db, name)

pointer	db				# DTTEXT pointer
char	name[ARB]			# Record name

int	i

bool	streq()
pointer	sp, err_string

begin
	do i = 1, DT_NRECS(db) {
	    if (streq (name, DT_NAME(db, i)))
		return (i)
	}

	# The record was not found

	call smark (sp)
	call salloc (err_string, SZ_LINE, TY_CHAR)

	call sprintf (Memc[err_string], SZ_LINE, 
	    "DDB_LOCATE: Database record '%s' not found")
	        call pargstr (name)

	call error (21, Memc[err_string])
	call sfree (sp)
end


# DDB_GSTR -- Get a string field

procedure ddb_gstr (db, record, field, str, maxchar)

pointer	db				# Database file descriptor
int	record				# Database index
char	field[ARB]			# Database field
char	str[maxchar]			# String value
int	maxchar				# Maximum characters for string

char	name[SZ_LINE]

pointer	sp, esp
int	i, fscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(db)))
	    call error (22, "Database record request out of bounds")

	call seek (DT(db), DT_OFFSET(db, record))

	while (fscan (DT(db)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargstr (str, maxchar)
		for (i=1; IS_WHITE(str[i]); i=i+1)
		    ;
		if (i>1)
		    call strcpy (str[i], str, maxchar)
		return
	    }
	}

	call smark (sp)
	call salloc (esp, SZ_LINE, TY_CHAR)

	call sprintf (Memc[esp], SZ_LINE, 
	    "DDB_GSTR: Database field '%s' not found")
		call pargstr (field)

	call error (23, Memc[esp])
	call sfree (sp)
end


# DDB_GETI -- Get an integer field.

int procedure ddb_geti (db, record, field)

pointer	db				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field

real	rval
bool    fp_equalr()
real	ddb_getr()

errchk	ddb_getr

begin
	rval = ddb_getr (db, record, field)

	if (fp_equalr (rval, INDEFR))
	    return (INDEFI)
	else 
	    return (int (rval))
end


# DDB_GETR -- Get an real field

real procedure ddb_getr (db, record, field)

pointer	db				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field

pointer	sp, esp
real	rval
char	name[SZ_LINE]

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(db)))
	    call error (24, "Database record request out of bounds")

	call seek (DT(db), DT_OFFSET(db, record))

	while (fscan (DT(db)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargr (rval)
		if (nscan() == 2)
		   return (rval)
		else
		   call error (25, "Error in database field value")
	    }
	}

	call smark (sp)
	call salloc (esp, SZ_LINE, TY_CHAR)

	call sprintf (Memc[esp], SZ_LINE, 
	    "DDB_GET: Database field '%s' not found")
		call pargstr (field)

	call error (26, Memc[esp])
	call sfree (sp)
end


# DDB_GAR -- Get a real array field

procedure ddb_gar (db, record, field, array, len_array, npts)

pointer	db				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field
real	array[len_array]		# Array values
int	len_array			# Length of array
int	npts				# Number of values in the field

int	i
double	tmp
pointer	sp, dubble
bool	fp_equald()

begin
	call smark (sp)
	call salloc (dubble, npts, TY_DOUBLE)

	call ddb_gad (db, record, field, Memd[dubble], len_array, npts)
	do i = 1, npts {
	    tmp = Memd[dubble+i-1]
	    if (fp_equald (tmp, INDEFD))
		array[i] = INDEFR
	    else
		array[i] = real (tmp)
	}

	call sfree (sp)
end


# DDB_GAD -- Get a double array field

procedure ddb_gad (db, record, field, array, len_array, npts)

pointer	db				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field
double	array[len_array]		# Array values
int	len_array			# Length of array
int	npts				# Number of points in the array

pointer	sp, esp
char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(db)))
	    call error (27, "Database record request out of bounds")

	call seek (DT(db), DT_OFFSET(db, record))

	while (fscan (DT(db)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (28, "Error in database field value")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (DT(db)) == EOF)
		        call error (29, "Error in database field value")

		    call gargd (array[i])
		    if (nscan() != 1)
		        call error (30, "Error in database field value")
		}
		return
	    }
	}

	call smark (sp)
	call salloc (esp, SZ_LINE, TY_CHAR)

	call sprintf (Memc[esp], SZ_LINE, 
	    "DDB_GA: Database field '%s' not found")
	        call pargstr (field)

	call error (31, Memc[esp])
	call sfree (sp)
end


# DDB_PTIME -- Put a time string with a comment

procedure ddb_ptime (db)

pointer	db				# DTTEXT pointer

char	timestr[SZ_TIME]
long	time, clktime()

begin
	time = clktime (0)
	call cnvtime (time, timestr, SZ_TIME)
	call fprintf (DT(db), "# %s\n")
	    call pargstr (timestr)
end


# DDB_SCAN -- Scan a line from the database.

int procedure ddb_scan (db)

pointer	db			# DDB pointer
int	fscan()

begin
	return (fscan (DT(db)))
end
