# AP_GETIM -- Get next image name with standard image extensions removed.
# This is necessary to avoid having two legal image names refering to the
# same image in the database.

int procedure ap_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Maximum number of chars in image name

int	i, stat, imtgetim(), strmatch()

begin
	stat = imtgetim (list, image, maxchar)
	if (stat != EOF) {
	    i = strmatch (image, ".??h$")
	    if (i > 0)
	        call strcpy (image[i], image[i-4], maxchar)
	    i = strmatch (image, ".??h[")
	    if (i > 0)
	        call strcpy (image[i], image[i-4], maxchar)
	}
  	return (stat)
end
