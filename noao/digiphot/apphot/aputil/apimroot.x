
# APIMROOT -- Procedure to fetch the root image name minus the directory
# specification and the section notation. The length of the root name is
# returned.

int procedure apimroot (image, root, maxch)

char	image[ARB]		# image specification
char	root[ARB]		# rootname
int	maxch			# maximum number of characters

int	nchars
pointer	sp, str
int	fnldir(), strlen()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call imgimage (image, root, maxch)
	nchars = fnldir (root, Memc[str], maxch)
	call strcpy (root[nchars+1], root, maxch)

	call sfree (sp)
	return (strlen (root))
end
