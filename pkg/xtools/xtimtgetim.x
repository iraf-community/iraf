# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XT_IMTGETIM -- Given two input lists and an output list return image elements
# from each of the lists.  The shorter input list is repeated as necessary.
# EOF is returned when the longer input list is finished or when the output
# list is finished.  The image name strings are assumed to all be at least
# of length sz_image.  It is assumed that if the image lists were
# successfully opened then no errors will occur with imtlen, imtgetim, and
# imtrew.

int procedure xt_imtgetim (list1, list2, list3, image1, image2, image3,
    sz_image)

int	list1			# First input image list
int	list2			# Second input image list
int	list3			# Output image list
char	image1[sz_image]	# Returned image from first list
char	image2[sz_image]	# Returned image from second list
char	image3[sz_image]	# Returned image from third list
int	sz_image		# Maximum size of image strings

int	imtlen(), imtgetim()

begin
	# If list1 is longer than list2 then get next element of list1
	# and repeat list2 if necessary.

	if (imtlen (list1) > imtlen (list2)) {
	    if (imtgetim (list1, image1, sz_image) == EOF)
		return (EOF)
	    if (imtgetim (list2, image2, sz_image) == EOF) {
		call imtrew (list2)
	        if (imtgetim (list2, image2, sz_image) == EOF)
		    return (EOF)	# Two EOFs are a null list.
	    }

	# If list2 is longer or equal to list1 then get next element of list2
	# and repeat list1 if necessary.

	} else {
	    if (imtgetim (list2, image2, sz_image) == EOF)
		return (EOF)
	    if (imtgetim (list1, image1, sz_image) == EOF) {
		call imtrew (list1)
	        if (imtgetim (list1, image1, sz_image) == EOF)
		    return (EOF)	# Two EOFs are a null list.
	    }
	}

	# Return the output image and the status of the output list.
	return (imtgetim (list3, image3, sz_image))
end
