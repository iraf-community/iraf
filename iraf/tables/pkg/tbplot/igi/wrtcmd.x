include <fset.h>

procedure wrtcmd (cmdf, out_file, mode)

int	cmdf			# Command buffer (spool) file descriptor
char	out_file[ARB]		# Output file name
int	mode			# Output file mode

pointer	sp, in_file

int	open()

begin
	call smark (sp)
	call salloc (in_file, SZ_FNAME, TY_CHAR)

	# Find input (command buffer) file name
	call fstats (cmdf, F_FILENAME, Memc[in_file], SZ_FNAME)

	call close (cmdf)

	# Copy the file
	call fcopy (Memc[in_file], out_file)

	# Reopen the input (command buffer) file
	cmdf = open (Memc[in_file], READ_WRITE, TEXT_FILE)

	call sfree (sp)
end
