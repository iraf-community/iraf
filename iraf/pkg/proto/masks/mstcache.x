include <imhdr.h>
include <imset.h>

define	MEMFUDGE	1.05

# MST_CACHE1 -- Cache 1 image in memory using the image i/o buffer sizes.

procedure mst_cache1 (cache, im, old_size)

int	cache			#I cache the image pixels in the imio buffer
pointer	im			#I the image descriptor
size_t	old_size		#O the old working set size

int	i
long	buf_size, l_val
size_t	req_size
int	sizeof(), mst_memstat()

begin
	req_size = MEMFUDGE * IM_LEN(im,1) * sizeof (IM_PIXTYPE(im))
	do i = 2, IM_NDIM(im)
	    req_size = req_size * IM_LEN(im,i)
	if (mst_memstat (cache, req_size, old_size) == YES) {
	    l_val = INDEFL
	    call mst_pcache (im, l_val, buf_size)
	}
end


# MST_MEMSTAT -- Figure out if there is  enough memory to cache the image
# pixels. If it is necessary to request more memory and the memory is
# avalilable return YES otherwise return NO.

int procedure mst_memstat (cache, req_size, old_size)

int	cache			#I cache memory ?
size_t	req_size		#I the requested working set size in chars 
size_t	old_size		#O the original working set size in chars 

size_t	cur_size, max_size, sz_0
size_t	begmem()

begin
	sz_0 = 0
        # Find the default working set size.
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


# MST_PCACHE -- Cache the image pixels im memory by resetting the  default image
# buffer size. If req_size is INDEF the size of the image is used to determine
# the size of the image i/o buffers.

procedure mst_pcache (im, req_size, buf_size)

pointer im                      #I the input image point
long	req_size                #I the requested working set size in chars
long	buf_size		#O the new image buffer size

int	i
long	def_size, new_imbufsize, l_val
int	sizeof()
long	imstatl()

begin
	# Find the default buffer size.
	def_size = imstatl (im, IM_BUFSIZE)

        # Compute the new required image i/o buffer size in chars.
        if (IS_INDEFL(req_size)) {
            new_imbufsize = IM_LEN(im,1) * sizeof (IM_PIXTYPE(im))
	    do i = 2, IM_NDIM(im)
		new_imbufsize = new_imbufsize * IM_LEN(im,i)
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
	l_val = 0
        call imsetl (im, IM_BUFFRAC, l_val)
	buf_size = new_imbufsize
end

