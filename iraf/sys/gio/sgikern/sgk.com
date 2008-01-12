# SGK.COM -- The common for the SGK kernel.  A common is used here for maximum
# efficiency (minimum indirection) when rasterizing vectors and encoding
# metacode.  The maximum bitmap size is set at compile time in sgk.h.

# Booleans put here to avoid possible alignment problems.

bool	mf_bitmap			# metafile type, metacode or bitmap
bool	mf_rotate			# rotate (swap x and y)
bool	mf_yflip			# flip y axis end for end
bool	mf_update			# update bitmap
bool	mf_delete			# delete metacode file after dispose
bool	mf_debug			# print kernel debugging messages
bool	mf_swap2			# swap every 2 bytes on output
bool	mf_swap4			# swap every 4 bytes on output
bool	mf_oneperfile			# store each frame in a new file

common	/sgkboo/ mf_bitmap, mf_rotate, mf_yflip, mf_update, mf_delete, mf_debug,
	mf_swap2, mf_swap4, mf_oneperfile

# Everything else goes here.

int	mf_fd				# file descriptor of output file
int	mf_frame			# frame counter
char	mf_fname[SZ_PATHNAME]		# metafile filename
char	mf_dispose[SZ_OSCMD]		# host dispose command

int	mf_op				# [MCODE] index into obuf
short	mf_obuf[LEN_OBUF]		# metacode buffer

int	mf_cx, mf_cy			# [BITMAPS] current pen position
int	mf_nbpb				# packing factor, bits per byte
int	mf_pxsize, mf_pysize		# physical x, y size of bitmap, bits
int	mf_wxsize, mf_wysize		# x, y size of bitmap window, bits
int	mf_xorigin, mf_yorigin		# origin of bitmap window
real	mf_xscale, mf_yscale		# to convert from NDC to device coords
int	mf_xmin, mf_xmax		# x clipping limits
int	mf_ymin, mf_ymax		# y clipping limits
int	mf_lenframe			# frame size, words
int	mf_linewidth			# relative line width
int	mf_lworigin			# device width of line size 1.0
real	mf_lwslope			# device pixels per line size increment
int	mf_fbuf[LEN_FBUF]		# frame buffer (BIG)
int	mf_bitmask[BPW]			# bit mask table

common	/sgkcom/ mf_fd, mf_frame, mf_op, mf_cx, mf_cy, mf_nbpb, mf_pxsize,
	mf_pysize, mf_wxsize, mf_wysize, mf_xorigin, mf_yorigin, mf_xscale,
	mf_yscale, mf_xmin, mf_xmax, mf_ymin, mf_ymax, mf_lenframe,
	mf_linewidth, mf_lworigin, mf_lwslope, mf_fbuf, mf_bitmask, mf_obuf,
	mf_fname, mf_dispose
