include <imhdr.h>
include <imset.h>

# AP_MEMSTAT -- Figure out if there is  enough memory to cache the image
# pixels. If it is necessary to request more memory and the memory is
# avalilable return YES otherwise return NO.

int procedure ap_memstat (cache, req_size, old_size)

int	cache			#I cache memory ?
size_t	req_size		#I the requested working set size in chars 
size_t	old_size		#O the original working set size in chars 

size_t	cur_size, max_size, sz_0
size_t	begmem()

begin
        # Find the default working set size.
	sz_0 = 0
        cur_size = begmem (sz_0, old_size, max_size)

	# If cacheing is disabled return NO regardless of the working set size.
	if (cache == NO)
	    return (NO)

	# If the requested working set size is less than the current working
	# set size return YES.
	if (req_size <= cur_size)
	    return (YES)

	# Reset the current working set size.
	cur_size = begmem (req_size, old_size, max_size)
	if (req_size <= cur_size) {
	    return (YES)
	} else {
	    return (NO)
	}
end


# AP_PCACHE -- Cache the image pixels im memory by resetting the  default image
# buffer size. If req_size is INDEF the size of the image is used to determine
# the size of the image i/o buffers.

procedure ap_pcache (im, req_size, buf_size)

pointer im                      #I the input image point
long	req_size                #I the requested working set size in chars
size_t	buf_size		#O the new image buffer size

long	def_size, new_imbufsize
int     sizeof()
long	imstatl()

begin
	# Find the default buffer size.
	def_size = imstatl (im, IM_BUFSIZE)

        # Return if the image is not 2-dimensional.
        if (IM_NDIM(im) != 2) {
	    buf_size = def_size
            return
	}

        # Compute the new required image i/o buffer size in chars.
        if (IS_INDEFL(req_size)) {
            new_imbufsize = IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
        } else {
            new_imbufsize = req_size
        }

	# If the default image i/o buffer size is already bigger than
	# the requested size do nothing.
	if (def_size >= new_imbufsize) {
	    buf_size = def_size
	    return
	}

        # Reset the image i/o buffer.
        call imsetl (im, IM_BUFSIZE, new_imbufsize)
        call imseti (im, IM_BUFFRAC, 0)
	buf_size = new_imbufsize
        return 
end
