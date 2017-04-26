# PMIO.COM -- A few scratch vectors used in many of the PMIO routines,
# defined in common to save some space.

long	v1[PM_MAXDIM], v2[PM_MAXDIM], v3[PM_MAXDIM], v4[PM_MAXDIM]
common	/pmiocom/ v1, v2, v3, v4
