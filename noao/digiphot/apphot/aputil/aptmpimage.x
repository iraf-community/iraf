# APTMPIMAGE -- Procedure to generate either a temporary image name using
# a given prefix or concatenating an image onto a specified prefix.

int procedure aptmpimage (image, prefix, tmp, name, maxch)

char	image[ARB]		# image name
char	prefix[ARB]		# user supplied prefix
char	tmp[ARB]		# user supplied temporary root
char	name[ARB]		# output name
int	maxch			# max number of chars

int	npref, ndir, junk
int	fnldir(), apimroot(), strlen()

begin
	npref = strlen (prefix)
	ndir = fnldir (prefix, name, maxch)
	if (npref == ndir) {
	    call mktemp (tmp, name[ndir+1], maxch)
	    return (NO)
	} else {
	    call strcpy (prefix, name, npref)
	    junk = apimroot (image, name[npref+1], maxch)
	    return (YES)
	}
end
