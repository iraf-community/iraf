# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<fset.h>

define	MIN_BUFSIZE	512


# FCOPY -- Copy a file.  Works for either text or binary files.  The new file
# will not be created unless the input file can be opened successfully.  All
# buffer space is dynamically allocated, and buffer sizes are automatically
# adjusted by the system for efficient sequential access (the actual buffer
# size is dependent on the machine, device, and file type).

procedure fcopy (oldfile, newfile)

char	oldfile[ARB]
char	newfile[ARB]

int	in, out, file_type, fd
int	open(), access(), fsfopen(), fstdfile()
errchk	open, fcopyo, access

begin
	if (access (oldfile, 0, TEXT_FILE) == YES)
	    file_type = TEXT_FILE
	else
	    file_type = BINARY_FILE

	in = open (oldfile, READ_ONLY, file_type)
	if (fstdfile (newfile, out) == NO) {
	    iferr (call fmkcopy (oldfile, newfile)) {
		call close (in)
		call erract (EA_ERROR)
	    }
	    out = open (newfile, APPEND, file_type)
	}

	# Warn user if the file being copied has subfiles.
	ifnoerr (fd = fsfopen (oldfile, READ_ONLY)) {
	    call close (fd)
	    call eprintf ("Warning from fcopy: file `%s' has subfiles\n")
		call pargstr (oldfile)
	}

	# Copy the file.
	call fcopyo (in, out)

	call close (in)
	call close (out)
end


# FCOPYO -- Copy a file, where both the input and output files have
# already been open.  Works regardless of the datatype of the files.

procedure fcopyo (in, out)

int	in			# input file descriptor
int	out			# output file descriptor

pointer	sp, buf
int	buf_size
int	fstati(), read()
errchk	read, write

begin
	call smark (sp)

	# Set up file buffers, intermediate buffer for efficient
	# sequential i/o (advice is ignored if text file).  Local buffer
	# is made same size as FIO buffer.

	call fseti (in, F_ADVICE, SEQUENTIAL)
	call fseti (out, F_ADVICE, SEQUENTIAL)
	buf_size = max (MIN_BUFSIZE, fstati (in, F_BUFSIZE))
	call salloc (buf, buf_size, TY_CHAR)

	while (read (in, Memc[buf], buf_size) != EOF)
	    call write (out, Memc[buf], fstati (in, F_NCHARS))

	call sfree (sp)
end
