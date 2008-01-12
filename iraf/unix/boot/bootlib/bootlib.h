#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	NOKNET
#define	import_knames
#include <iraf.h>

#define	SZ_FBUF		512		/* File i/o buffer size		*/

#ifdef VMS
#define	rindex	strrchr 
struct	timeval {
	long	tv_sec;
	long	tv_usec;
};
#else
#include <sys/time.h>
#endif

#define safe_strcpy(dest,size,src) (0 < (size) ? strcpy((dest)+(size)-1,"") : NULL, strncpy(dest,src,0 < (size) ? (size)-1 : 0))
#define safe_strcat(dest,size,src) strncat(dest,src,(strlen(dest) < (size)) ? (size)-1-strlen(dest) : 0)

/* osglobal.c */
extern	int bdebug;
extern	int osfiletype;
extern	XCHAR text[];
extern	XCHAR *txop;

/* vfn2osfn.c */
extern const char *vfn2osfn ( const char *, int );
/* osfn2vfn.c */
extern const char *osfn2vfn ( const char * );
/* osstrpak.c */
extern char *os_strpak ( const XCHAR *, char *, long );
/* osstrupk.c */
extern XCHAR *os_strupk ( const char *, XCHAR *, long );
/* osgetenv.c */
extern char *_os_getenv ( const char *, char *, size_t );
extern const char *os_getenv ( const char * );
/* osputenv.c */
extern void os_putenv ( const char *, const char * );
/* osfpathname.c */
extern int os_fpathname ( const char *, char *, size_t );
/* osaccess.c */
extern int os_access ( const char *, int, int );
/* envinit.c */
extern void _envinit( void );
extern void loadpkgenv ( const char * );
/* ossubdir.c */
extern const char *os_subdir ( const char *, const char * );
/* oscrfile.c */
extern int os_createfile ( const char *, int, int );
/* osfdate.c */
extern long os_fdate ( const char * );
/* ossysfile.c */
extern int os_sysfile ( const char *, const char *, char *, size_t );
/* oscmd.c */
extern int os_cmd ( const char * );
/* ossymlink.c */
extern int os_symlink ( const char *, char *, size_t );
/* osdelete.c */
extern int os_delete ( const char * );
/* oschdir.c */
extern int os_chdir ( const char * );
/* osdir.c */
extern int os_diropen ( const char * );
extern int os_dirclose ( int );
extern int os_gfdir ( int, char *, size_t );
/* osfiletype.c */
extern int os_filetype ( const char * );
/* oswrite.c */
extern int os_write ( int, const char *, int );
/* osamovb.c */
extern void os_amovb ( const char *, char *, int );
/* osclose.c */
extern int os_close ( int );
/* osfcopy.c */
extern int os_fcopy ( const char *, const char * );
/* ossetfmode.c */
extern int os_setfmode ( const char *, int );
/* ossetowner.c */
extern int os_setowner ( const char *, int, int );
/* ossettime.c */
extern int os_setmtime ( const char *, long );
/* oscreatedir.c */
extern int os_createdir ( const char *, int );
/* osgetowner.c */
extern void os_getowner ( const char *, int *, int * );
/* ostime.c */
extern long os_utime ( long );
/* osopen.c */
extern int os_open ( const char *, int, int );
/* osread.c */
extern int os_read ( int, char *, int );
/* tape.c */
extern int tape_open ( const char *, int );
extern int tape_read ( int, char *, int );
extern int tape_close ( int );
extern int tape_write ( int, const char *, int );
