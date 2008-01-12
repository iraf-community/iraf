# FC_GETIM -- Get next image name with standard image extensions removed.
# This is necessary to avoid having two legal image names refering to the
# same image.

int procedure fc_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Maximum number of chars in image name

int	i, stat, imtgetim(), strmatch()

begin
	stat = imtgetim (list, image, maxchar)

	if (stat == EOF)
	    return (stat)

	i = strmatch (image, ".imh")
	if (i > 0) {
	    call strcpy (image[i], image[i-4], maxchar)
	    return (stat)
	}

	i = strmatch (image, ".hhh")
	if (i > 0) {
	    call strcpy (image[i], image[i-4], maxchar)
	    return (stat)
	}

  	return (stat)
end
