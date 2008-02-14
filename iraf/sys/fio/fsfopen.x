# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FSFOPEN -- Open the subfile list file.  A subfile is a physical file which is
# logically subordinate to another file (e.g., the pixel storage file is a
# subfile of an imagefile).  The subfile list file is a hidden textfile,
# containing a list of filenames.

int procedure fsfopen (fname, mode)

char	fname[ARB]		# name of logical file
int	mode			# access mode for subfile list

int	fd
pointer	sp, listfile
int	open()
errchk	open, fsf_getfname

begin
	call smark (sp)
	call salloc (listfile, SZ_FNAME, TY_CHAR)

	call fsf_getfname (fname, Memc[listfile], SZ_FNAME)
	fd = open (Memc[listfile], mode, TEXT_FILE)

	call sfree (sp)
	return (fd)
end


# FSFDELETE -- Delete all of the subfiles of a file, then the subfile list
# file itself.

procedure fsfdelete (fname)

char	fname[ARB]		# file whose subfiles are to be deleted
int	fd
pointer	sp, subfile
int	fsfopen(), getline()
errchk	getline, delete, fsf_getfname

begin
	call smark (sp)
	call salloc (subfile, SZ_FNAME, TY_CHAR)

	# Open the list and delete each subfile.  To avoid recursion this must
	# be done by calling delete, hence subfiles may not have subfiles.
	# It is not an error if the listfile cannot be opened, i.e., if there
	# are no subfiles.

	iferr (fd = fsfopen (fname, READ_ONLY)) {
	    call sfree (sp)
	    return
	}

	while (getline (fd, Memc[subfile]) != EOF)
	    call delete (Memc[subfile])
	call close (fd)

	# Delete the listfile itself.

	call fsf_getfname (fname, Memc[subfile], SZ_FNAME)
	call delete (Memc[subfile])

	call sfree (sp)
end


# FSF_GETFNAME -- Get the name of the subfile list file for a file.

procedure fsf_getfname (fname, fsf_file, maxch)

char	fname[ARB]		# main file
char	fsf_file[maxch]		# file containing names of subfiles
int	maxch

begin
	call strcpy (fname, fsf_file, maxch)
	call strcat (SUBFILE_EXTN, fsf_file, maxch)
end
