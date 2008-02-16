# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<time.h>
include	<ctype.h>
include	<ctotok.h>
include	<error.h>
include	<fset.h>
include	<pkg/dttext.h>

.help dttext May85 "Simple Text Database Tools"
.ih
DESCRIPTION

The database created, accessed, and modified by these procedures is
a simple text file.  The purpose of these tools is to act as
an interum database facility until the sophisticated IRAF database
package is available.  The database model consists of
comment lines which begin with a #, records, and fields within records.
Records and fields (except array fields) have the same format, a keyword
followed by data and terminated by a newline.  Records have the keyword
'begin' and the data is any following text.  Thus a record can be identified
by anything from a single character to an entire string.  A record contains
all the following lines until the next record or the end of the file.
Whitespace before and after the keyword is ignored.  The user is responsible
for adding indentation to clarify the structure
of the database.  However, the user can create a database with any
style of whitespace that satisfies the keyword/value syntax.

The array fields have a slightly different format.  The field begins just
like an integer valued field; keyword followed by an integer.  The integer
value is the number of array elements.  The following lines
contain the array values, one per line.  Because the field name
line has the same structure as an integer valued field the array
length can be determined before reading the array values by reading
the field as integer valued.

For arrays with more than one column per line the dtscan procedure is
used to scan a line and then the FMTIO garg procedures are used to
decode the columns.  The user read the array field as an integer to get
the number of lines and to position FIO to start reading the lines.

There are four types of fields currently supported.  These are integer
valued fields, real valued fields, string valued fields, and real arrays.
It is up to the user to know the type of value for each field.  Note
that the integer and real fields may be accessed as string valued.

Records are referenced by a record number.  When a database is mapped
each record which is unique is given a sequential record number.
When more than one record has the same record identifier then only
the last record is mapped.

There are limitations imposed by the text file format.  A database
may only be read or appended.  To update a record a new record must
be written.  A later record with the same name takes precedence.

Errors are handled through the standard error handling system of IRAF.
Thus, uncaught errors will terminate the task with a message.  If it
is possible that a field will not be present then the task can catch
the error and take appropriate action.
.ih
DATABASE MAPPING

When a database is mapped READ_ONLY then the records in the database
are found and a structure created.  The structure is given in the file
"dttext.h".  The important elements of the structure are:

.nf

	    DT(dt)		# Database FIO channel
	    DT_NRECS(dt)	# Number of records
	    DT_NAME(dt, rec)	# Record name
	    DT_OFFSET(dt, rec)	# FIO offset
.fi
.ih
PROCEDURES

The procedures separate into three types, procedures to map and unmap
the database, procedures to access the database, and procedures to make
entries in the database.  The access routines reference a particular
record.  To access a record by name the procedure dtlocate returns
the record number or EOF.  The put routines write to the end of
the database.  It is important to enter a record because otherwise
added fields will be associated with the preceding record.  The put
time command puts a comment line with the time.

.nf
	   dt = dtmap (database, mode)		# NEW_FILE, READ_ONLY or APPEND
	   dt = dtmap1 (database, name, mode)	# Use a directory as a database
		dtremap (dt, database, name,mode) # Remap a database
		dtunmap (dt)

       record = dtlocate (dt, recname)

		dtgstr (dt, record, field, str, maxchar)
	value = dtget[ird] (dt, record, field)
		dtgar (dt, record, field, array, len_array, npts)

		dtptime (dt)
		dtput (dt, format)
.fi
.ih
EXAMPLES

The following is an example record from a database.

.nf
# Fri 15:13:13 05-Apr-85 Example
begin	NGC1952 B
	title	NGC1952 B filter centered
	ra	12:40:20
	dec	+5:20:15
	flags	4
		3.1
		9.2
		1
		4
	exp	3600
.fi


The following example reads the example record and writes a new record.

.nf
	iferr {
	    dt = dtmap (database, READ_ONLY)
	    record = dtlocate (dt, "NGC1952 B")
	    call dtgstr (dt, record, "title", title, SZ_TITLE)
	    ra = dtgetr (dt, record, "RA")
	    dec = dtgetr (dt, record, "DEC")

	    # Get length of array for dynamic allocation.
	    nflags = dtgeti (dt, record, "flags")
	    call salloc (flags, nflags, TY_REAL)
	    call dtgar (dt, record, "flags", Memr[flags], nflags, nflags)
	}

	dt = dtmap (database, APPEND)
	call dtptime (dt)
	call dtput (dt, "begin\tNGC1952 Objects\n")
	call dtput (dt, "\tobjects\t10\n)
	do i = 1, 10 {
	    call dtput (dt, "\t\t%g\n")
		call pargr (objects[i])
	}
	call dtclose (dt)
.fi

The following is a database entry for a list which is read by the code below.

.nf
# Fri 15:13:13 05-Apr-85 Example
begin	Table 1
	1	apples		10	macintosh
	2	oranges		8	valencia
	3	potatoes	3	idaho


	# Code to read database table.

	record = dtlocate (dt, "Table 1")
	call seek (DT(dt), DT_OFFSET(dt, record))
	while (scan (DT(dt)) != EOF) {
	    call gargi (n)
	    call gargwrd (fruit[1, n])
	    call gargi (number[n])
	    call gargstr (comment[1, n])
	}

To read sequentially through a database:

	# Code to read sequentially through a database.

	do i = 1, DB_NRECS(db) {
	    call printf ("%s\n")
		call pargstr (DB_NAME(db, i))
	}
.fi
.ih
SEE ALSO
Source code
.endhelp


# DTMAP -- Map a database.

pointer procedure dtmap (database, mode)

char	database[ARB]			# Database file
int	mode				# FIO mode

int	i, nrec
int	dt_alloc1, dt_alloc2
pointer	dt, str

int	open(), fscan(), strlen()
bool	streq()
long	note()
errchk	delete, open

begin
	if (mode == NEW_FILE)
	    iferr (call delete (database))
		;

	i = open (database, mode, TEXT_FILE)

	call calloc (dt, DT_LEN, TY_STRUCT)
	DT(dt) = i

	if (mode != READ_ONLY)
	    return (dt)

	dt_alloc1 = DT_ALLOC
	dt_alloc2 = DT_ALLOC * SZ_LINE
	call malloc (DT_OFFSETS(dt), dt_alloc1, TY_LONG)
	call malloc (DT_NAMES(dt), dt_alloc1, TY_INT)
	call malloc (DT_MAP(dt), dt_alloc2, TY_CHAR)
	call malloc (str, SZ_LINE, TY_CHAR)

	nrec = 1
	DT_NRECS(dt) = 0
	DT_NAMEI(dt, nrec) = 0

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (DT_NAME(dt, nrec), SZ_LINE)

	    if (streq (DT_NAME(dt, nrec), "begin")) {
	        call gargstr (Memc[str], SZ_LINE)
		for (i=str; IS_WHITE(Memc[i]); i=i+1)
		    ;
		call strcpy (Memc[i], DT_NAME(dt,nrec), SZ_LINE)

		for (i = 1; i < nrec; i = i + 1)
		    if (streq (DT_NAME(dt, i), DT_NAME(dt, nrec)))
			break

		if (i < nrec)
		    DT_OFFSET(dt, i) = note (DT(dt))
		else {
		    DT_NRECS(dt) = nrec
		    DT_OFFSET(dt, nrec) = note (DT(dt))
		    DT_NAMEI(dt, nrec+1) = DT_NAMEI(dt, nrec) +
			strlen (DT_NAME(dt, nrec)) + 1
		    nrec = nrec + 1
		}

		if (nrec == dt_alloc1) {
		    dt_alloc1 = dt_alloc1 + DT_ALLOC
		    call realloc (DT_OFFSETS(dt), dt_alloc1, TY_LONG)
		    call realloc (DT_NAMES(dt), dt_alloc1, TY_INT)
		}
		if (DT_NAMEI(dt, nrec) + SZ_LINE >= dt_alloc2) {
		    dt_alloc2 = dt_alloc2 + DT_ALLOC * SZ_LINE
		    call realloc (DT_MAP(dt), dt_alloc2, TY_CHAR)
		}
	    }
	}

	call realloc (DT_MAP(dt), DT_NAMEI(dt, nrec), TY_CHAR)
	call realloc (DT_OFFSETS(dt), DT_NRECS(dt), TY_LONG)
	call realloc (DT_NAMES(dt), DT_NRECS(dt), TY_INT)
	call mfree (str, TY_CHAR)

	return (dt)
end


# DTCLOSE -- Close database.

procedure dtunmap (dt)

pointer	dt				# Database file descriptor

begin
	if (dt == NULL)
	    return
	call close (DT(dt))
	call mfree (DT_MAP(dt), TY_CHAR)
	call mfree (DT_OFFSETS(dt), TY_LONG)
	call mfree (DT_NAMES(dt), TY_INT)
	call mfree (dt, TY_STRUCT)
end


# DTLOCATE -- Locate a database record.

int procedure dtlocate (dt, name)

pointer	dt				# DTTEXT pointer
char	name[ARB]			# Record name

int	i

bool	streq()

begin
	do i = 1, DT_NRECS(dt) {
	    if (streq (name, DT_NAME(dt, i)))
		return (i)
	}

#	call printf ("Record = %s\n")
#	    call pargstr (name)
#	call flush (STDOUT)
	call error (0, "Database record not found")
end


# DTGSTR -- Get a string field

procedure dtgstr (dt, record, field, str, maxchar)

pointer	dt				# Database file descriptor
int	record				# Database index
char	field[ARB]			# Database field
char	str[maxchar]			# String value
int	maxchar				# Maximum characters for string

char	name[SZ_LINE]
int	i, fscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(dt)))
	    call error (0, "Database record request out of bounds")

	call seek (DT(dt), DT_OFFSET(dt, record))

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargstr (str, maxchar)
		for (i=1; IS_WHITE(str[i]); i=i+1)
		    ;
		if (i > 1)
		    call strcpy (str[i], str, maxchar)
		return
	    }
	}

	call error (0, "Database field not found")
end


# DTGETI -- Get an integer field

int procedure dtgeti (dt, record, field)

pointer	dt				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field

int	ival				# Field value
char	name[SZ_LINE]

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(dt)))
	    call error (0, "Database record request out of bounds")

	call seek (DT(dt), DT_OFFSET(dt, record))

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (ival)
		if (nscan() == 2)
		   return (ival)
		else
		   call error (0, "Error in database field value")
	    }
	}

	call error (0, "Database field not found")
end


# DTGETR -- Get an real field

real procedure dtgetr (dt, record, field)

pointer	dt				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field

real	rval
char	name[SZ_LINE]

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(dt)))
	    call error (0, "Database record request out of bounds")

	call seek (DT(dt), DT_OFFSET(dt, record))

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargr (rval)
		if (nscan() == 2)
		   return (rval)
		else
		   call error (0, "Error in database field value")
	    }
	}

	call error (0, "Database field not found")
end


# DTGETD -- Get a doubel precision field.

double procedure dtgetd (dt, record, field)

pointer dt                              # DTTEXT pointer
int     record                          # Database index
char    field[ARB]                      # Database field

double  dval
char    name[SZ_LINE]

int     fscan(), nscan()
bool    streq()

begin
        if ((record < 1) || (record > DT_NRECS(dt)))
            call error (0, "Database record request out of bounds")

        call seek (DT(dt), DT_OFFSET(dt, record))

        while (fscan (DT(dt)) != EOF) {
            call gargwrd (name, SZ_LINE)

            if (streq (name, "begin"))
                break
            else if (streq (name, field)) {
                call gargd (dval)
                if (nscan() == 2)
                   return (dval)
                else
                   call error (0, "Error in database field value")
            }
        }

        call error (0, "Database field not found")
end


# DTGAR -- Get a real array field

procedure dtgar (dt, record, field, array, len_array, npts)

pointer	dt				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field
real	array[len_array]		# Array values
int	len_array			# Length of array
int	npts				# Number of points in the array

char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(dt)))
	    call error (0, "Database record request out of bounds")

	call seek (DT(dt), DT_OFFSET(dt, record))

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (0, "Error in database field value")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (DT(dt)) == EOF)
		        call error (0, "Error in database field value")

		    call gargr (array[i])
		    if (nscan() != 1)
		        call error (0, "Error in database field value")
		}
		return
	    }
	}

	call error (0, "Database field not found")
end


# DTGAD -- Get a double array field

procedure dtgad (dt, record, field, array, len_array, npts)

pointer	dt				# DTTEXT pointer
int	record				# Database index
char	field[ARB]			# Database field
double	array[len_array]		# Array values
int	len_array			# Length of array
int	npts				# Number of points in the array

char	name[SZ_LINE]
int	i

int	fscan(), nscan()
bool	streq()

begin
	if ((record < 1) || (record > DT_NRECS(dt)))
	    call error (0, "Database record request out of bounds")

	call seek (DT(dt), DT_OFFSET(dt, record))

	while (fscan (DT(dt)) != EOF) {
	    call gargwrd (name, SZ_LINE)

	    if (streq (name, "begin"))
		break
	    else if (streq (name, field)) {
		call gargi (npts)
		if (nscan() != 2)
		    call error (0, "Error in database field value")

		npts = min (npts, len_array)
		for (i = 1; i <= npts; i = i + 1) {
		    if (fscan (DT(dt)) == EOF)
		        call error (0, "Error in database field value")

		    call gargd (array[i])
		    if (nscan() != 1)
		        call error (0, "Error in database field value")
		}
		return
	    }
	}

	call error (0, "Database field not found")
end


# DTPTIME -- Put a time string with a comment

procedure dtptime (dt)

pointer	dt				# DTTEXT pointer

char	timestr[SZ_TIME]
long	time, clktime()

begin
	time = clktime (0)
	call cnvtime (time, timestr, SZ_TIME)
	call fprintf (DT(dt), "# %s\n")
	    call pargstr (timestr)
end


# DTPUT -- Print to database.

procedure dtput (dt, format)

pointer	dt				# DTTEXT pointer
char	format[ARB]			# String format

begin
	call fprintf (DT(dt), format)
end

# DTSCAN -- Scan database.

int procedure dtscan (dt)

pointer	dt				# DTTEXT pointer

int	fscan()

begin
	return (fscan (DT(dt)))
end


include	<finfo.h>

# DTMAP1 -- Map database.
#
# The database name may be a regular file or a directory.  If it is a
# directory a database file with the name given by key is read or appended.

pointer procedure dtmap1 (database, key, mode)

char	database[ARB]		# Database
char	key[ARB]		# Key
int	mode			# Mode

pointer	sp, dbfile, dt

int	isdirectory(), access(), stridxs()
pointer	dtmap()

errchk	dtmap()

begin
	call smark (sp)
	call salloc (dbfile, SZ_PATHNAME + SZ_FNAME, TY_CHAR)

	# Check if the database does not exist create it as a directory.

	if (access (database, READ_ONLY, DIRECTORY_FILE) == NO)
	    if ((mode == APPEND) || (mode == NEW_FILE)) {
		if (stridxs (".", database) != 0)
		    call error (0,
			"Periods not allowed in database directory name")
		iferr (call fmkdir (database))
		    call error (0, "Can't make database directory")
	    }

	if (isdirectory (database, Memc[dbfile], SZ_PATHNAME + SZ_FNAME) > 0)
	    call strcat (key, Memc[dbfile], SZ_PATHNAME + SZ_FNAME)
	else
	    call strcpy (database, Memc[dbfile], SZ_PATHNAME + SZ_FNAME)

	dt = dtmap (Memc[dbfile], mode)
	call strcpy (database, DT_DNAME(dt), DT_SZFNAME)
	call strcpy (key, DT_FNAME(dt), DT_SZFNAME)
	DT_MODE(dt) = mode

	call sfree (sp)
	return (dt)
end


# DTREMAP -- Check if database needs to be remapped.
#
# If the pointer is null simply map the database.
# If the pointer is not null check if the requested database is the same
# as the current one and if not close the current database and map the
# new one.  Note that remapping between read and append will not update
# the entry data structure to include any information written.

procedure dtremap (dt, dname, fname, mode)

pointer	dt			# Database pointer
char	dname[ARB]		# Directory name
char	fname[ARB]		# File name
int	mode			# Mode

int	i, open()
bool	strne()
pointer	dbfile, dtmap1()
errchk	dtmap1, dtunmap

begin
	if (dt != NULL) {
	    if (strne (dname, DT_DNAME(dt)) || strne (fname, DT_FNAME(dt))) {
		call dtunmap (dt)
	    } else if (mode != DT_MODE(dt)) {
		i = SZ_PATHNAME + SZ_FNAME
		call malloc (dbfile, i, TY_CHAR)
		call fstats (DT(dt), F_FILENAME, Memc[dbfile], i)
		call close (DT(dt))
		iferr (i = open (Memc[dbfile], mode, TEXT_FILE)) {
		    DT(dt) = NULL
		    call dtunmap (dt)
		    call mfree (dbfile, TY_CHAR)
		    call erract (EA_ERROR)
		}
		DT(dt) = i
		DT_MODE(dt) = mode
		call mfree (dbfile, TY_CHAR)
	    }
	}

	if (dt == NULL) {
	    i = dtmap1 (dname, fname, mode)
	    dt = i
	}
end
