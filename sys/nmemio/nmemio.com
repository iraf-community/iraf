
int	mclear				# clear newly allocated memory?
int	mwatch				# check buffer sentinals on FREE?
int	mcollect			# garbage collect on exit?
int	mreport				# report memio usage stats?

int	lsentinal			# lower sentinal value
int	usentinal			# upper sentinal value

long	mem_used			# total mem usage
long	max_alloc			# largest allocated pointer size
long	leaked				# total leaked bytes
int 	nleaked				# number leaked pointers
int 	nalloc				# total number of allocations
int 	nfree				# total number of frees

int	mdebug				# debugging memory use in tasks?
int	in_task				# in task or iraf main?

int	bmax                            # current maximum number of pointers
pointer	mgc				# garbage collection buffer

#  Debug common
common	/nmemio/ mclear, mwatch, mcollect, mreport, lsentinal, usentinal,
		 mem_used, max_alloc, nleaked, leaked, nalloc, nfree, 
		 mdebug, in_task, bmax, mgc

