# EC_GETIM -- Get next image name with standard image extensions removed.

int procedure ec_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Maximum number of chars in image name

int	stat, imtgetim()

begin
	stat = imtgetim (list, image, maxchar)
	if (stat != EOF)
	    call xt_imroot (image, image, maxchar)

  	return (stat)
end
