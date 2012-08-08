# FXFCACHE.COM -- Named common block used to cache filenames and image
# extension information.
#
# ##### This should be reimplemented to use a small package (i.e. functions)
# ##### rather than global common.  rf_fname below is using a lot of memory.
# ##### Dynamic memory allocation or a packed string buffer should be used
# ##### instead.  Not worth fixing though until the cache code is redone.

int	rf_cachesize
pointer rf_fit[MAX_CACHE]		# FITS descriptor
pointer rf_hdrp[MAX_CACHE]		# Fits headers pointer
pointer rf_pixp[MAX_CACHE]		# Fits pixels pointer
pointer rf_pextn[MAX_CACHE]		# EXTNAME pointer
pointer rf_pextv[MAX_CACHE]		# EXTVER pointer
int	rf_lru[MAX_CACHE]		# Lowest value is oldest slot
long	rf_time[MAX_CACHE]		# Time when entry was cached
long	rf_mtime[MAX_CACHE]		# Modify time of file in cache
int	rf_hdr[MAX_CACHE]		# FITS Primary header data
int	rf_fitslen[MAX_CACHE]		# Size Primary header data
char	rf_fname[SZ_PATHNAME,MAX_CACHE] # Header file pathname

common /fxflcachec/ rf_time, rf_mtime
common /fxfcachec/ rf_cachesize, rf_fit, rf_hdrp, rf_pixp, rf_pextn,
       rf_pextv, rf_lru, rf_hdr, rf_fitslen, rf_fname
