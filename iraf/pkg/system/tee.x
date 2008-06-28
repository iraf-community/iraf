# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

define	SZ_OTSTR	10

# TEE -- Tee the standard output.  The type of the output file defaults to
# text file unless the caller specifies otherwise.  Note that this tee
# writes the entire output all at once.

procedure t_tee()

char	tee_file[SZ_FNAME], out_type_string[SZ_OTSTR]
size_t	sz_val
pointer	sp, buf
int	out, out_type
size_t	sz_buf, nchars

bool	clgetb()
int	open(), strmatch(), getline()
long	read(), fstatl()

begin
	# Get params and open the output file.
	call clgstr ("tee_file", tee_file, SZ_FNAME)
	call clgstr ("out_type", out_type_string, SZ_OTSTR)

	if (strmatch (out_type_string, "^#{b}") > 0)
	    out_type = BINARY_FILE
	else
	    out_type = TEXT_FILE

	if (clgetb ("append"))
	    out = open (tee_file, APPEND, out_type)
	else
	    out = open (tee_file, NEW_FILE, out_type)


	# Make a buffer and perform the copy operation.
	call smark (sp)

	if (out_type == TEXT_FILE) {
	    sz_val = SZ_LINE
	    call salloc (buf, sz_val, TY_CHAR)
	    while (getline (STDIN, Memc[buf]) != EOF) {
		call putline (STDOUT, Memc[buf])
		call putline (out, Memc[buf])
	    }

	} else {
	    sz_buf = fstatl (STDIN, F_BUFSIZE)
	    call salloc (buf, sz_buf, TY_CHAR)

	    while (read (STDIN, Memc[buf], sz_buf) != EOF) {
		nchars = fstatl (STDIN, F_NCHARS)
		call write (STDOUT, Memc[buf], nchars)
		call write (out, Memc[buf], nchars)
	    }
	}

	call sfree (sp)
	call close (out)
end
