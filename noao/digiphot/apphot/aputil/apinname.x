# APINNAME -- Procedure to construct an apphot input file name.
# If input is null or a directory a name is constructed from the root
# of the image name and the extension. The disk is searched to avoid
# name collisions.

procedure apinname (image, input, ext, name, maxch)

char	image[ARB]		# image name
char	input[ARB]		# input directory or name
char	ext[ARB]		# extension
char	name[ARB]		# input name
int	maxch			# maximum size of name

int	ndir
pointer	sp, root
int	fnldir(), strlen(), apimroot()

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call strcpy (image, Memc[root], maxch)

	ndir = fnldir (input, name, maxch)
	if (strlen (input) == ndir) {
	    ndir = ndir + apimroot (Memc[root], name[ndir+1], maxch)
	    call sprintf (name[ndir+1], maxch, ".%s.*")
		call pargstr (ext)
	    call apiversion (name, name, maxch)
	} else
	    call strcpy (input, name, maxch)

	call sfree (sp)
end
