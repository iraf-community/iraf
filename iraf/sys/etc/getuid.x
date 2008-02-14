# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETUID -- Get user id, i.e., return the name of the user.  We do this by
# creating a temporary file and calling fowner to get the name of the file
# owner.

procedure getuid (user_name, maxch)

char	user_name[ARB]
int	maxch
pointer	sp, tempfile
size_t	sz_val
int	open()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (tempfile, sz_val, TY_CHAR)

	call mktemp ("tmp$uid", Memc[tempfile], SZ_FNAME)
	call close (open (Memc[tempfile], NEW_FILE, BINARY_FILE))
	call fowner (Memc[tempfile], user_name, maxch)
	call delete (Memc[tempfile])

	call sfree (sp)
end
