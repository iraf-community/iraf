# EC_GETIM -- Get next image name with standard image extensions removed.

int procedure ec_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Maximum number of chars in image name

int	i, stat, imtgetim(), strldx()

begin
	stat = imtgetim (list, image, maxchar)

	if (stat == EOF)
	    return (stat)

        # .??h
        i = strldx ("h", image)
        if (i > 4)
           if (image[i-3] == '.')
               call strcpy (image[i+1], image[i-3], ARB)

  	return (stat)
end
