/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>


void 	BYTMOV (XCHAR *a, XINT *aoff, XCHAR *b, XINT *boff, XINT *nbytes);

void 	loadpkgenv (char *pkg);
void 	_envinit (void);
void 	loadenv (char *osfn);

#ifdef NO_OS_INDEX
char   *index (char *str, int ch);
char   *rindex (char *str, int ch);
#endif

int 	os_access (char *fname, int mode, int type);
void 	os_amovb (char *a, char *b, int nbytes);
int 	os_chdir (char *dir);
void 	os_close (int fd);
int 	os_cmd (char *cmd);
int 	os_createdir (char *dirname, int mode);
int 	os_createfile (char *fname, int mode, int type);
int 	os_delete (char *fname);
int 	os_diropen (char *dirname);
int 	os_dirclose (int chan);
int 	os_gfdir (int chan, char *fname, int maxch);
int 	os_fcopy (char *oldfile, char *newfile);
long 	os_fdate (char *fname);
int 	os_filetype (char *fname);
char   *osfn2vfn (char *osfn);
int 	os_fpathname (char *vfn, char *osfn, int maxch);
char   *os_getenv (char *envvar);
void 	os_getowner (char *fname, int *uid, int *gid);
int 	os_open (char *vfn, int mode, int type);
void 	os_putenv (char *name, char *value);
int 	os_read (int fd, char *buf, int nbytes);
int 	os_setfmode (char *fname, int mode);
int 	os_setowner (char *fname, int uid, int gid);
int 	os_setmtime (char *fname, long mtime);
char   *os_strpak (XCHAR *sppstr, char *cstr, int maxch);
XCHAR  *os_strupk (char *str, XCHAR *outstr, int maxch);
char   *os_subdir (char *dir, char *subdir);
int 	os_symlink (char *fname, char *valbuf, int maxch);
int 	os_sysfile (char *sysfile, char *fname, int maxch);
char   *os_irafpath (char *sysfile);
long 	os_utime (long iraf_time);
long 	os_itime (long unix_time);
int 	os_write (int fd, char *buf, int nbytes);
char   *vfn2osfn (char *vfn, int new);
