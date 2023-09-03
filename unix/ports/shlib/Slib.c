/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/*
 * SLIB.C -- Support routines for the shared library.
 */

extern	unsigned vshlib_[];
extern	char *((*environ)[]);
extern	char *Malloc(), *Realloc(), *Free();

main()
{
	/* Malloc etc. are produced by the MEDIT task - see mkshlib.csh. */
	vlibinit_ (environ, Malloc, Realloc, Free);

	/* Do something useful when S.e is called as a task. */
	printf ("Sun/IRAF Shared Library, version %d, %d symbols\n",
	    vshlib_[0], vshlib_[5]);
	printf ("base=%x, etext=%x, edata=%x, end=%x, size=%dKb\n",
	    vshlib_[1], vshlib_[2], vshlib_[3], vshlib_[4],
	    (vshlib_[4] - vshlib_[1]) / 1000);
}


/* Back link to selected procedures and global variables in the client so
 * that the shared library code can call routines executing in the runtime
 * context of the client image.  (Not intended to be portable).
 */
#define	I_malloc	0
#define	I_realloc	1
#define	I_free		2
#define	I_len		3

#ifdef SOLARIS
#define	I_dlopen	4
#define	I_dlclose	5
#define	I_dlsym		6
#define	I_dlerror	7
#endif

typedef int (*PFI)();
static	PFI fcn[I_len];

malloc	(nb)	{ return (fcn[I_malloc](nb)); }
realloc	(bp,nb)	{ return (fcn[I_realloc](bp,nb)); }
free	(bp)	{ return (fcn[I_free](bp)); }

#ifdef SOLARIS
void *dlopen (const char *pathname, int mode)
	{ return ((void *)fcn[I_dlopen] (pathname, mode)); }
int dlclose (void *handle)
	{ return ((int)fcn[I_dlclose] (handle)); }
void *dlsym (void *handle, const char *name)
	{ return ((void *)fcn[I_dlsym] (handle, name)); }
char *dlerror (void)
	{ return ((char *)fcn[I_dlerror]()); }

vlibinit_ (u_environ, u_malloc, u_realloc, u_free,
	    u_dlopen, u_dlclose, u_dlsym, u_dlerror)
char	*((*u_environ)[]);
PFI	u_malloc, u_realloc, u_free;
PFI	u_dlopen, u_dlclose, u_dlsym, u_dlerror;
{
	environ = u_environ;
	fcn[I_malloc] = u_malloc;
	fcn[I_realloc] = u_realloc;
	fcn[I_free] = u_free;
	fcn[I_dlopen] = u_dlopen;
	fcn[I_dlclose] = u_dlclose;
	fcn[I_dlsym] = u_dlsym;
	fcn[I_dlerror] = u_dlerror;
}
#else

vlibinit_ (u_environ, u_malloc, u_realloc, u_free)
char	*((*u_environ)[]);
PFI	u_malloc, u_realloc, u_free;
{
	environ = u_environ;
	fcn[I_malloc] = u_malloc;
	fcn[I_realloc] = u_realloc;
	fcn[I_free] = u_free;
}
#endif
