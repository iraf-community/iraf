# AP_GETIM -- Get next image name with standard image extensions removed.
# This is necessary to avoid having two legal image names refering to the
# same image in the database.

int procedure ap_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Maximum number of chars in image name

int	i, j, k, stat, imtgetim(), strmatch()

begin
	stat = imtgetim (list, image, maxchar)
	if (stat != EOF) {
	    i = strmatch (image, ".??h$")
	    j = strmatch (image, ".qp$")
	    k = strmatch (image, ".pl$")
	    if (i > 0)
	        call strcpy (image[i], image[i-4], maxchar)
	    else if (j > 0)
	        call strcpy (image[j], image[j-3], maxchar)
	    else if (k > 0)
	        call strcpy (image[k], image[k-3], maxchar)
	    i = strmatch (image, ".??h[") - 1
	    j = strmatch (image, ".qp[") - 1
	    k = strmatch (image, ".pl[") - 1
	    if (i > 0)
	        call strcpy (image[i], image[i-4], maxchar)
	    else if (j > 0)
	        call strcpy (image[j], image[j-3], maxchar)
	    else if (k > 0)
	        call strcpy (image[k], image[k-3], maxchar)
	}
  	return (stat)
end
