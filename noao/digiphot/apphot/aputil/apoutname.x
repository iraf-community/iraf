# APOUTNAME -- Procedure to construct an apphot output file name.
# If output is null or a directory a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.

procedure apoutname (image, output, ext, name, maxch)

char	image[ARB]		# image name
char	output[ARB]		# output directory or name
char	ext[ARB]		# extension
char	name[ARB]		# output name
int	maxch			# maximum size of name

int	ndir
pointer	sp, root
int	fnldir(), strlen(), apimroot()

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call strcpy (image, Memc[root], maxch)

	ndir = fnldir (output, name, maxch)
	if (strlen (output) == ndir) {
	    ndir = ndir + apimroot (Memc[root], name[ndir+1], maxch)
	    call sprintf (name[ndir+1], maxch, ".%s.*")
		call pargstr (ext)
	    call apoversion (name, name, maxch)
	} else
	    call strcpy (output, name, maxch)

	call sfree (sp)
end
