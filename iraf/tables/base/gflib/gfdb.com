# GFDB.COM -- Database of extra information for images

common / gfdb / imcache, dbcache, gncache, excache, incache, upcache, hscache, high, hnum

pointer	imcache[MAXDB]		# image descriptors, used as key
pointer	dbcache[MAXDB]		# database descriptor
int	gncache[MAXDB]		# group number of image
int	excache[MAXDB]		# is this a fits file with extensions?
int	incache[MAXDB]		# is the primary header inherited?
int	upcache[MAXDB]		# has primary header been updated?
int     hscache[MAXDB]          # history spool
int	high			# last slot allocated to db
int     hnum                    # hist file id

