# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<mach.h>
include	<fset.h>

task	alloc		= t_allocate,
	dealloc		= t_deallocate,
	owner		= t_owner,
	mtpos		= t_mtposition,
	wtestfile	= t_wtestfile,
	mtexamine	= t_mtexamine,
	mtcopy		= t_mtcopy


.help testmtio
.nf __________________________________________________________________________
MTIO test routines.  Assorted routines for verification of MTIO.

	alloc		Allocate a drive.

	dealloc		Deallocate a drive.

	owner		Print drive status

	mtpos		Position to the indicated file and record.

	wtestfile	Writes a test file.  The number of records and the
			range of record sizes may be specified.  The contents
			of a record are determined by its size.

	mtexamine	Examines the structure of a tape.  Tells the number of
			files on the tape, the number of records in each file,
			the sizes of the records, and optionally dumps the
			contents of an indicated range of records from each
			file.

	mtcopy		Fast binary copy.  Copies a binary disk or tape file
			to a binary disk or tape file using all the FIO
			defaults.
.endhelp _____________________________________________________________________


# ALLOCATE -- Allocate a drive.

procedure t_allocate()

int	junk, status
char	drive[SZ_FNAME]
char	owner[SZ_FNAME]
int	xallocate(), xdevowner()

begin
	call clgstr ("drive", drive, SZ_FNAME)
	status = xallocate (drive)

	switch (status) {
	case OK:
	    call printf ("device allocated successfully\n")
	case ERR:
	    call printf ("cannot allocate device\n")
	case DV_DEVFREE:
	    call printf ("device is free and may be allocated\n")
	case DV_DEVALLOC:
	    call printf ("device is already allocated\n")
	case DV_DEVINUSE:
	    junk = xdevowner ("drive", owner, SZ_FNAME)
	    call printf ("device is already allocated to `%s'\n")
		call pargstr (owner)
	case DV_DEVNOTFOUND:
	    call printf ("device not found\n")
	default:
	    call printf ("unknown status %d\n")
		call pargi (status)
	}
end


# DEALLOCATE -- Deallocate a drive.

procedure t_deallocate()

int	junk, status
char	drive[SZ_FNAME]
char	owner[SZ_FNAME]

bool	clgetb()
int	xdeallocate(), xdevowner()

begin
	call clgstr ("drive", drive, SZ_FNAME)
	status = xdeallocate (drive, clgetb ("rewind"))

	switch (status) {
	case OK:
	    call printf ("device deallocated successfully\n")
	case ERR:
	    call printf ("cannot deallocate device\n")
	case DV_DEVFREE:
	    call printf ("device is free and may be allocated\n")
	case DV_DEVALLOC:
	    call printf ("device is already allocated\n")
	case DV_DEVINUSE:
	    junk = xdevowner ("drive", owner, SZ_FNAME)
	    call printf ("device is already allocated to `%s'\n")
		call pargstr (owner)
	case DV_DEVNOTFOUND:
	    call printf ("device not found\n")
	default:
	    call printf ("unknown status %d\n")
		call pargi (status)
	}
end


# OWNER -- Print drive status.

procedure t_owner()

int	status
char	drive[SZ_FNAME]
char	owner[SZ_FNAME]
int	xdevowner()

begin
	call clgstr ("drive", drive, SZ_FNAME)
	status = xdevowner (drive, owner, SZ_FNAME)

	switch (status) {
	case OK:
	    call printf ("device deallocated successfully\n")
	case ERR:
	    call printf ("cannot deallocate device\n")
	case DV_DEVFREE:
	    call printf ("device is free and may be allocated\n")
	case DV_DEVALLOC:
	    call printf ("device is already allocated\n")
	case DV_DEVINUSE:
	    call printf ("device is allocated to `%s'\n")
		call pargstr (owner)
	case DV_DEVNOTFOUND:
	    call printf ("device not found\n")
	default:
	    call printf ("unknown status %d\n")
		call pargi (status)
	}
end


# MTPOS -- Position to the indicated file and record.

procedure t_mtposition()

char	drive[SZ_FNAME]
int	clgeti()

begin
	call clgstr ("drive", drive, SZ_FNAME)
	call mtposition (drive, clgeti("file"), clgeti("record"))
end


# WTESTFILE -- Write a test file to the tape.  Specify file [1] to write to
# a new tape.  If no file number is given, the file is appended to the tape.
# Specify the number of records to be written and the range of sizes in bytes
# of the records.  Each byte of a record will contain the size of the record
# modulus 256.

procedure t_wtestfile()

char	tapefile[SZ_FNAME]
int	nrecords
int	min_recsize, max_recsize

int	fd, i, recsize, oschan, status
long	seed
pointer	buf
int	clgeti(), mtopen(), fstati()
real	urand()
data	seed /123/

begin
	# Get tapefile name and open file for writing.
	call clgstr ("tapefile", tapefile, SZ_FNAME)
	fd = mtopen (tapefile, WRITE_ONLY, 1)
	oschan = fstati (fd, F_CHANNEL)

	nrecords = max (0, clgeti ("nrecords"))
	min_recsize = max (1, clgeti ("min_recsize"))
	max_recsize = max (min_recsize, clgeti ("max_recsize"))

	call malloc (buf, max_recsize, TY_CHAR)

	# Records are written by directly calling ZAWRMT, so that we can
	# write odd size records.
	do i = 1, nrecords {
	    recsize = int ((max_recsize - min_recsize) * urand (seed)) +
		min_recsize
	    call amovkc (char(recsize), Memc[buf], recsize)
	    call chrpak (Memc[buf], 1, Memc[buf], 1, recsize)
	    call zawrmt (oschan, Memc[buf], recsize, 0)
	    call zawtmt (oschan, status)
	    if (status == ERR)
		call error (1, "write error")
	}

	call mfree (buf, TY_CHAR)
	call close (fd)
end


# MTEXAMINE -- Examine the structure of a tape filesystem or a file.  If no file
# number is given, all files are examined.

procedure t_mtexamine()

char	infile[SZ_FNAME], tapefile[SZ_FNAME]
int	fileno, nrecords
int	strlen(), mt_examine()

begin
	call clgstr ("infile", infile, SZ_FNAME)
	call fseti (STDOUT, F_FLUSHNL, YES)

	if (infile[strlen(infile)] == ']') {
	    call strcpy (infile, tapefile, SZ_FNAME)
	    nrecords = mt_examine (STDOUT, tapefile)

	} else {
	    fileno = 1
	    repeat {
		call sprintf (tapefile, SZ_FNAME, "%s[%d]")
		    call pargstr (infile)
		    call pargi (fileno)
		fileno = fileno + 1
	    } until (mt_examine (STDOUT, tapefile) == 0)
	}
end


# MT_EXAMINE -- Examine a magtape file.  Print file number, then count
# successive records.  When the record size changes, print the number of
# records encountered with the old size.  When all done, print the total
# number of records and bytes.  Return the number of records in the file.

int procedure mt_examine (out, mtfile)

int	out				# output stream
char	mtfile[ARB]			# magtape file to be examined

int	in, nrecords, totrecords, totbytes, bufsize, recsize, last_recsize
pointer	buf
int	mtopen(), read(), fstati()
errchk	mtopen, read, fstati, printf, pargi

begin
	in = mtopen (mtfile, READ_ONLY, 0)
	bufsize = fstati (in, F_BUFSIZE)
	call malloc (buf, bufsize, TY_CHAR)

	call fprintf (out, "    File %s:\n")
	    call pargstr (mtfile)

	totrecords = 0
	nrecords = 0
	totbytes = 0
	last_recsize = 0

	# Describe record composition of file.
	while (read (in, Memc[buf], bufsize) != EOF) {
	    recsize = fstati (in, F_SZBBLK)
	    if (nrecords == 0) {			# first record
		nrecords = 1
		last_recsize = recsize
	    } else if (recsize == last_recsize) {
		nrecords = nrecords + 1
	    } else {
		call fprintf (out, "\t%d %d-byte records\n")
		    call pargi (nrecords)
		    call pargi (last_recsize)
		nrecords = 1
		last_recsize = recsize
	    }
	    totrecords = totrecords + 1
	    totbytes = totbytes + recsize
	}

	if (nrecords > 0) {
	    call fprintf (out, "\t%d %d-byte records\n")
		call pargi (nrecords)
		call pargi (last_recsize)
	}

	# Print total count of records, bytes.
	call fprintf (out, "\tTotal %d records, %d bytes\n")
	    call pargi (totrecords)
	    call pargi (totbytes)

	call mfree (buf, TY_CHAR)
	call close (in)

	return (totrecords)
end


# MTCOPY -- Copy a binary file from magtape or disk to magtape or disk,
# using all the default FIO and MTIO pararameters.  If the output file is
# a magtape, all records (except possibly the last record in the file) will
# be the same size.  If input tape records are not commensurate with the size
# of a CHAR they will be zero-padded to an integral number of chars upon
# input.

procedure t_mtcopy()

char	infile[ARB], outfile[ARB]
int	in, out, bufsize, acmode
pointer	buf
int	mtopen(), fstati(), read(), strncmp()

begin
	call clgstr ("infile", infile, SZ_FNAME)
	call clgstr ("outfile", outfile, SZ_FNAME)

	in = mtopen (infile, READ_ONLY, 0)

	# If output file is a disk file, create a new file, but do not
	# create a new tape if writing to tape.
	acmode = NEW_FILE
	if (strncmp (outfile, "mt", 2) == 0)
	    acmode = WRITE_ONLY
	out = mtopen (outfile, acmode, 0)

	bufsize = fstati (in, F_BUFSIZE)
	call malloc (buf, bufsize, TY_CHAR)

	while (read (in, Memc[buf], bufsize) != EOF)
	    call write (out, Memc[buf], fstati (in, F_NCHARS))

	call mfree (buf, TY_CHAR)
	call close (in)
	call close (out)
end
