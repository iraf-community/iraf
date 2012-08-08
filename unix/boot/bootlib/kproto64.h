/* _bytmov.c */
extern void bytmov_(short *a, long *aoff, short *b, long *boff, long *nbytes);
/* envinit.c */
extern void loadpkgenv(char *pkg);
extern void _envinit(void);
extern void loadenv(char *osfn);
/* index.c */
/* osaccess.c */
extern int os_access(char *fname, int mode, int type);
/* osamovb.c */
extern void os_amovb(char *a, char *b, int nbytes);
/* oschdir.c */
extern int os_chdir(char *dir);
/* osclose.c */
extern void os_close(int fd);
/* oscmd.c */
extern int os_cmd(char *cmd);
/* oscreatedir.c */
extern int os_createdir(char *dirname, int mode);
/* oscrfile.c */
extern int os_createfile(char *fname, int mode, int type);
/* osdelete.c */
extern int os_delete(char *fname);
/* osdir.c */
extern int os_diropen(char *dirname);
extern int os_dirclose(int chan);
extern int os_gfdir(int chan, char *fname, int maxch);
/* osfcopy.c */
extern int os_fcopy(char *oldfile, char *newfile);
/* osfdate.c */
extern long os_fdate(char *fname);
/* osfiletype.c */
extern int os_filetype(char *fname);
/* osfn2vfn.c */
extern char *osfn2vfn(char *osfn);
/* osfpathname.c */
extern int os_fpathname(char *vfn, char *osfn, int maxch);
/* osgetenv.c */
extern char *os_getenv(char *envvar);
extern char *_os_getenv(char *envvar, char *outstr, int maxch);
/* osgetowner.c */
extern void os_getowner(char *fname, int *uid, int *gid);
/* osopen.c */
extern int os_open(char *vfn, int mode, int type);
/* osputenv.c */
extern void os_putenv(char *name, char *value);
/* osread.c */
extern int os_read(int fd, char *buf, int nbytes);
/* ossetfmode.c */
extern int os_setfmode(char *fname, int mode);
/* ossetowner.c */
extern int os_setowner(char *fname, int uid, int gid);
/* ossettime.c */
extern int os_setmtime(char *fname, long mtime);
/* osstrpak.c */
extern char *os_strpak(short *sppstr, char *cstr, int maxch);
/* osstrupk.c */
extern short *os_strupk(char *str, short *outstr, int maxch);
/* ossubdir.c */
extern char *os_subdir(char *dir, char *subdir);
/* ossymlink.c */
extern int os_symlink(char *fname, char *valbuf, int maxch);
/* ossysfile.c */
extern int os_sysfile(char *sysfile, char *fname, int maxch);
/* ostime.c */
extern long os_utime(long iraf_time);
extern long os_itime(long unix_time);
/* oswrite.c */
extern int os_write(int fd, char *buf, int nbytes);
/* rindex.c */
/* tape.c */
extern int tape_open(char *fname, int mode);
extern int tape_close(int fd);
extern int tape_read(int fd, char *buf, int maxbytes);
extern int tape_write(int fd, char *buf, int nbytes);
/* vfn2osfn.c */
extern char *vfn2osfn(char *vfn, int new);
extern int kigets_(void);
extern void kisend_(void);
extern void kirece_(void);
