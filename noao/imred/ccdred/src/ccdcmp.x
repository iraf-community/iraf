# CCD_CMP -- Compare two image names with extensions ignored.

bool procedure ccd_cmp (image1, image2)

char	image1[ARB]		# First image
char	image2[ARB]		# Second image

int	i, j, strmatch(), strlen(), strncmp()
bool	streq()

begin
	if (streq (image1, image2))
	    return (true)

	i = max (strmatch (image1, ".imh"), strmatch (image1, ".hhh"))
	if (i == 0)
	    i = strlen (image1)
	j = max (strmatch (image2, ".imh"), strmatch (image2, ".hhh"))
	if (j == 0)
	    j = strlen (image2)

	return (strncmp (image1, image2, max (i, j)) == 0)
end
