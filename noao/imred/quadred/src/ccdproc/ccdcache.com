# Common data defining the cached images and data.

int	ccd_ncache		# Number of images cached
int	ccd_maxcache		# Maximum size of cache
int	ccd_szcache		# Current size of cache
int	ccd_oldsize		# Original memory size
int	ccd_pcache		# Pointer to image cache structures

common	/ccdcache_com/ ccd_ncache, ccd_maxcache, ccd_szcache, ccd_oldsize,
	ccd_pcache
