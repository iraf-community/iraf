# snap common block
int	sn_fd					# device file descriptor
int	sn_frame, sn_bitpl			# save current iframe, iplane
int	zbufsize				# fio buffer size--save here
pointer	lutp[3,LEN_IISFRAMES]			# look up table storage
pointer	ofmp[3]					# rgb ofm tables
pointer	grp[3]					# graphics tables
pointer	result[3]				# rgb results
pointer	answer					# final answer
pointer	input					# input data
pointer	zs					# zoom/scrolled data; scratch
pointer	grbit_on				# graphics bit on
bool	gr_in_use				# graphics RAM not all zeroes
bool	on[LEN_IISFRAMES]			# if frames on at all
bool	multi_frame				# snap using >1 frame
short	range[3]				# range and offset for rgb
short	offset[3]
short	left[3,2,LEN_IISFRAMES]			# left boundary of line
short	right[3,2,LEN_IISFRAMES]		# right boundary of line
short	ysplit					# split point for y
short	prev_y					# previous line read
short	sn_start, sn_end			# color range to snap

common	/ zsnap / sn_fd, sn_frame, sn_bitpl, zbufsize, lutp, ofmp, grp,
	result, answer, input, zs, grbit_on, gr_in_use, on, multi_frame,
	range, offset, left, right, ysplit, prev_y, sn_start, sn_end
