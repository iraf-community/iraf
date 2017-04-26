#ifndef lint
static char *rid="$XConsortium: main.c,v 1.222 94/04/17 20:23:28 gildea Exp $";
#endif /* lint */

/*
 * 				 W A R N I N G
 * 
 * If you think you know what all of this code is doing, you are
 * probably very mistaken.  There be serious and nasty dragons here.
 *
 * This client is *not* to be taken as an example of how to write X
 * Toolkit applications.  It is in need of a substantial rewrite,
 * ideally to create a generic tty widget with several different parsing
 * widgets so that you can plug 'em together any way you want.  Don't
 * hold your breath, though....
 */

/***********************************************************


Copyright (c) 1987, 1988  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.


Copyright 1987, 1988 by Digital Equipment Corporation, Maynard.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Digital not be used in 
advertising or publicity pertaining to distribution of the software 
without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/


/* main.c */

#include "ptyx.h"
#include "data.h"
#include "error.h"
#include "menu.h"
#include <ObmW/Gterm.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#ifdef I18N
#include <X11/Xlocale.h>
#endif

#define TERMCAP_SIZE		1500


/* choose a nice default value for speed - if we make it too low, users who
 * mistakenly use $TERM set to vt100 will get padding delays
 */
#define B38400                   /* everyone should define this */
#ifdef  B38400                   /* everyone should define this */
#define VAL_LINE_SPEED B38400
#else /* ...but xterm's used this for a long time */
#define VAL_LINE_SPEED B9600
#endif


#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/SimpleMenu.h>
#include <pwd.h>
#include <ctype.h>

#ifdef USE_TTY_GROUP
#include <grp.h>
#endif

#if defined(SYSV) && !defined(SVR4) && !defined(ISC22) && !defined(ISC30)
/* older SYSV systems cannot ignore SIGHUP.
   Shell hangs, or you get extra shells, or something like that */
#define USE_SYSV_SIGHUP
#endif

#ifdef __DARWIN__
#define NEW_GET_PTY
#define NEW_SPAWN
#define USE_HANDSHAKE
#define USE_USG_PTYS
#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value
#define USE_STRUCT_WINSIZE 1
#define TTYSIZE_STRUCT struct winsize

#define TTYSIZE_STRUCT struct winsize
#define SET_TTYSIZE(fd, data) ioctl(fd, TIOCSWINSZ, (char *) &data)
#define TTYSIZE_COLS(data) data.ws_col
#define TTYSIZE_ROWS(data) data.ws_row
/*
*/
#endif

/*
#ifdef linux
*/
#if defined(linux)
#define USE_ANY_SYSV_TERMIO
#define USE_SYSV_TERMIO
#define USE_SYSV_PGRP
#define USE_SYSV_UTMP
#define USE_SYSV_SIGNALS

#define FEDORA
#define NEW_GET_PTY
#define NEW_SPAWN
#define USE_USG_PTYS
/*
#define USE_STRUCT_WINSIZE
*/
#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value

#define USE_STRUCT_WINSIZE 1
#define TTYSIZE_STRUCT struct winsize

#define TTYSIZE_STRUCT struct winsize
#define SET_TTYSIZE(fd, data) ioctl(fd, TIOCSWINSZ, (char *) &data)
#define TTYSIZE_COLS(data) data.ws_col
#define TTYSIZE_ROWS(data) data.ws_row

/*
#ifndef SYSV
#define SYSV
#endif
*/
#endif   /* linux */


#ifdef att
#define ATT
#endif

#ifdef linux
#define USE_SYSV_UTMP
#define USE_SYSV_PGRP
#define USE_TERMIOS
#define USE_HANDSHAKE
#define HAS_UTMP_UT_HOST
/*
#define HAS_BSD_GROUPS
*/
#undef TIOCCONS
#define CONSLOG "/proc/kmsg"
#endif

#ifdef SVR4
#ifndef SYSV
#define SYSV			/* SVR4 is (approx) superset of SVR3 */
#endif
#define ATT
#define USE_SYSV_UTMP
#define USE_TERMIOS
#define HAS_UTMP_UT_HOST
#endif

#if defined(sgi) && OSMAJORVERSION >= 5
#ifndef SVR4
#define SVR4			/* close enough for xterm */
#endif
#define USE_SYSV_UTMP
#define USE_TERMIOS
#define HAS_UTMP_UT_HOST
#endif

#ifdef SYSV386
#define USE_SYSV_UTMP
#define ATT
#define USE_HANDSHAKE
static Bool IsPts = False;
#endif

#ifdef ATT
#define USE_USG_PTYS
#else
#define USE_HANDSHAKE
#endif

#if defined(SYSV) && !defined(SVR4) && !defined(AUX) && !defined(ISC22) && !defined(ISC30)
/* older SYSV systems cannot ignore SIGHUP.
   Shell hangs, or you get extra shells, or something like that */
#define USE_SYSV_SIGHUP
#endif

/* Some local additions to get things to build.  (DCT) */
#ifdef AUX
#include <utmp.h>
struct utmp *getutid();
extern char *ttyname();
void *memmove(a,b,n) void *a; const void *b; size_t n; { bcopy(b,a,n); }
#endif
#if defined(sun) && OSMAJORVERSION < 5
#undef memmove
void *memmove(a,b,n) void *a, *b; int n; { bcopy(b,a,n); }
#endif

#if defined(sony) && defined(bsd43) && !defined(KANJI)
#define KANJI
#endif

#include <sys/ioctl.h>
#include <sys/stat.h>


#ifdef USE_TERMIOS
#include <termios.h>
/* this hacked termios support only works on SYSV */
#define USE_SYSV_TERMIO
#define termio termios
#undef TCGETA
#define TCGETA TCGETS
#undef TCSETA
#define TCSETA TCSETS
#else /* USE_TERMIOS */
#ifdef SYSV
#include <sys/termio.h>
#ifdef SCO /* broken TIOCSWINSZ ioctl so disable it */
#undef TIOCSWINSZ
#endif
#endif /* SYSV */
#endif /* USE_TERMIOS else */

#if defined(SVR4) || defined(__linux__)
#undef TIOCSLTC				/* defined, but not useable */
#endif
#define USE_TERMCAP_ENVVARS     /* every one uses this except SYSV maybe */

#if defined(sgi) && OSMAJORVERSION >= 5
#undef TIOCLSET				/* defined, but not useable */
#endif

#ifdef SYSV /* { */
#ifdef USE_USG_PTYS			/* AT&T SYSV has no ptyio.h */
#include <sys/stream.h>			/* get typedef used in ptem.h */
#include <sys/stropts.h>		/* for I_PUSH */
#ifndef SVR4
#include <sys/ptem.h>			/* get struct winsize */
#endif
#include <poll.h>			/* for POLLIN */
#endif /* USE_USG_PTYS */
#ifdef FEDORA
#define USE_USG_PTYS
#endif
#define USE_SYSV_TERMIO
#define USE_SYSV_SIGNALS
#define	USE_SYSV_PGRP

#if !defined(TIOCSWINSZ)
#define USE_SYSV_ENVVARS		/* COLUMNS/LINES vs. TERMCAP */
#endif

#ifndef SCO
#undef USE_TERMCAP_ENVVARS      /* SCO wants both TERMCAP and TERMINFO env */
#endif
/*
 * now get system-specific includes
 */
#ifdef CRAY
#define USE_SYSV_UTMP
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#endif
#ifdef _IBMR2
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#define USE_SYSV_UTMP
#endif
#ifdef macII
#define USE_SYSV_UTMP
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#include <sys/ttychars.h>
#undef USE_SYSV_ENVVARS
#undef FIOCLEX
#undef FIONCLEX
#define setpgrp2 setpgrp
#include <sgtty.h>
#include <sys/resource.h>
#endif
#if defined(hpux) || defined (__hpux)
#define HAS_BSD_GROUPS
#define USE_SYSV_UTMP
#define HAS_UTMP_UT_HOST
#include <sys/ptyio.h>
#endif /* __hpux */
#ifdef sgi
#include <sys/sysmacros.h>
#endif /* sgi */
#ifdef sun
#include <sys/strredir.h>
#endif
#ifdef AIXV3
#define USE_SYSV_UTMP
#define HAS_UTMP_UT_HOST
#endif
#else /* } !SYSV { */			/* BSD systems 	*/
#ifndef linux
#include <sgtty.h>
#include <sys/resource.h>
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#ifdef __osf__
#define USE_SYSV_UTMP
#define setpgrp setpgid
#endif
#endif	/* !linux */
#endif	/* } !SYSV */

#ifdef _POSIX_SOURCE
#define USE_POSIXSYSV_WAIT
#endif
#ifdef SVR4
#define USE_POSIX_WAIT
#endif

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#ifdef X_NOT_STDC_ENV
extern int errno;
#define Time_t long
extern Time_t time ();
#else
#include <time.h>
#define Time_t time_t
#endif

#if defined(hpux) || defined(__hpux) 
#include <sys/utsname.h>
#endif /* __hpux */

#if defined(apollo) && OSMAJORVERSION == 10 && OSMINORVERSION < 4
#define ttyslot() 1
#endif /* apollo */

#ifdef SVR4
#include <utmpx.h>
#define setutent setutxent
#define getutent getutxent
#define getutid getutxid
#define endutent endutxent
#define pututline pututxline
#else
#include <utmp.h>
#if defined(_CRAY) && OSMAJORVERSION < 8
extern struct utmp *getutid __((struct utmp *_Id));
#endif
#endif

#ifdef LASTLOG
#include <lastlog.h>
#endif
#include <sys/param.h>	/* for NOFILE */

#ifdef  PUCC_PTYD
#include <local/openpty.h>
int	Ptyfd;
#endif /* PUCC_PTYD */

#ifdef sequent
#define USE_GET_PSEUDOTTY
#endif

#ifndef UTMP_FILENAME
#ifdef UTMP_FILE
#define UTMP_FILENAME UTMP_FILE
#else
#define UTMP_FILENAME "/etc/utmp"
#endif
#endif

#ifndef LASTLOG_FILENAME
#ifdef _PATH_LASTLOG
#define LASTLOG_FILENAME _PATH_LASTLOG
#else
#define LASTLOG_FILENAME "/usr/adm/lastlog"  /* only on BSD systems */
#endif
#endif

#ifndef WTMP_FILENAME
#ifdef WTMP_FILE
#define WTMP_FILENAME WTMP_FILE
#else
#if defined(_PATH_WTMP)		/* R6 update	*/
#define WTMP_FILENAME _PATH_WTMP
#else
#if defined(SYSV)
#define WTMP_FILENAME "/etc/wtmp"
#else
#define WTMP_FILENAME "/usr/adm/wtmp"
#endif
#endif
#endif
#endif

#include <signal.h>

#if defined(SCO) || defined(ISC)
#undef SIGTSTP			/* defined, but not the BSD way */
#endif

#ifdef SIGTSTP
#include <sys/wait.h>
#if defined(hpux) || defined(__hpux)
#include <sys/bsdtty.h>
#endif
#endif

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#define SIGNAL_RETURN return 0
#else
#define SIGNAL_T void
#define SIGNAL_RETURN return
#endif

SIGNAL_T Exit();

#ifndef X_NOT_POSIX
#include <unistd.h>
#else
extern long lseek();
#if defined(USG) || defined(SCO324)
extern unsigned sleep();
#else
extern void sleep();
#endif
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *malloc();
extern char *calloc();
extern char *realloc();
extern char *getenv();
extern void exit();
#endif
#ifdef X_NOT_POSIX
extern char *ttyname();
#endif

#ifdef SYSV
extern char *ptsname();
#endif

extern char *strindex ();
extern void HandlePopupMenu();
extern void gtermio_connect();

int switchfb[] = {0, 2, 1, 3};

static SIGNAL_T reapchild ();

static Bool added_utmp_entry = False;

static char **command_to_exec;

#ifdef USE_SYSV_TERMIO
/* The following structures are initialized in main() in order
** to eliminate any assumptions about the internal order of their
** contents.
*/
static struct termio d_tio;
#ifdef TIOCSLTC
static struct ltchars d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
static unsigned int d_lmode;
#endif	/* TIOCLSET */
#else /* not USE_SYSV_TERMIO */
static struct  sgttyb d_sg = {
        0, 0, 0177, CKILL, EVENP|ODDP|ECHO|XTABS|CRMOD
};
static struct  tchars d_tc = {
        CINTR, CQUIT, CSTART,
        CSTOP, CEOF, CBRK,
};
static struct  ltchars d_ltc = {
        CSUSP, CDSUSP, CRPRNT,
        CFLUSH, CWERASE, CLNEXT
};
static int d_disipline = NTTYDISC;
static long int d_lmode = LCRTBS|LCRTERA|LCRTKIL|LCTLECH;
#ifdef sony
static long int d_jmode = KM_SYSSJIS|KM_ASCII;
static struct jtchars d_jtc = {
	'J', 'B'
};
#endif /* sony */
#endif /* USE_SYSV_TERMIO */

static int parse_tty_modes ();
/*
 * SYSV has the termio.c_cc[V] and ltchars; BSD has tchars and ltchars;
 * SVR4 has only termio.c_cc, but it includes everything from ltchars.
 */
#define TTYMODE(name) { name, sizeof(name)-1, 0, 0 }

static int override_tty_modes = 0;
struct _xttymodes {
    char *name;
    int len;
    int set;
    char value;
} ttymodelist[] = {
{ "intr", 4, 0, '\0' },			/* tchars.t_intrc ; VINTR */
#define XTTYMODE_intr 0
{ "quit", 4, 0, '\0' },			/* tchars.t_quitc ; VQUIT */
#define XTTYMODE_quit 1
{ "erase", 5, 0, '\0' },		/* sgttyb.sg_erase ; VERASE */
#define XTTYMODE_erase 2
{ "kill", 4, 0, '\0' },			/* sgttyb.sg_kill ; VKILL */
#define XTTYMODE_kill 3
{ "eof", 3, 0, '\0' },			/* tchars.t_eofc ; VEOF */
#define XTTYMODE_eof 4
{ "eol", 3, 0, '\0' },			/* VEOL */
#define XTTYMODE_eol 5
{ "swtch", 5, 0, '\0' },		/* VSWTCH */
#define XTTYMODE_swtch 6
{ "start", 5, 0, '\0' },		/* tchars.t_startc */
#define XTTYMODE_start 7
{ "stop", 4, 0, '\0' },			/* tchars.t_stopc */
#define XTTYMODE_stop 8
{ "brk", 3, 0, '\0' },			/* tchars.t_brkc */
#define XTTYMODE_brk 9
{ "susp", 4, 0, '\0' },			/* ltchars.t_suspc ; VSUSP */
#define XTTYMODE_susp 10
{ "dsusp", 5, 0, '\0' },		/* ltchars.t_dsuspc ; VDSUSP */
#define XTTYMODE_dsusp 11
{ "rprnt", 5, 0, '\0' },		/* ltchars.t_rprntc ; VREPRINT */
#define XTTYMODE_rprnt 12
{ "flush", 5, 0, '\0' },		/* ltchars.t_flushc ; VDISCARD */
#define XTTYMODE_flush 13
{ "weras", 5, 0, '\0' },		/* ltchars.t_werasc ; VWERASE */
#define XTTYMODE_weras 14
{ "lnext", 5, 0, '\0' },		/* ltchars.t_lnextc ; VLNEXT */
#define XTTYMODE_lnext 15
    TTYMODE("status"),          	/* VSTATUS */
#define XTTYMODE_status 16
    TTYMODE("erase2"),          	/* VERASE2 */
#define XTTYMODE_erase2 17
    TTYMODE("eol2"),            	/* VEOL2 */
#define XTTYMODE_eol2   18
{ NULL, 0, 0, '\0' },			/* end of data */
};

#ifdef USE_SYSV_UTMP
#if defined(X_NOT_STDC_ENV) || defined(AIXV3)
extern struct utmp *getutent();
extern struct utmp *getutid();
extern struct utmp *getutline();
extern void setutent();
extern void endutent();
#ifdef AIXV3
extern struct utmp* pututline();
extern int utmpname();
#else
extern void pututline();
extern void utmpname();
#endif
#endif /* !SVR4 */

/*#ifndef SYSV386		/* could remove paragraph unconditionally? */
#ifdef X_NOT_STDC_ENV		/* could remove paragraph unconditionally? */
extern struct passwd *getpwent();
extern struct passwd *getpwuid();
extern struct passwd *getpwnam();
extern void setpwent();
extern void endpwent();
#endif

extern struct passwd *fgetpwent();
#else	/* not USE_SYSV_UTMP */
static char etc_utmp[] = UTMP_FILENAME;
#ifdef LASTLOG
static char etc_lastlog[] = LASTLOG_FILENAME;
#endif 
#endif	/* USE_SYSV_UTMP */

#ifdef WTMP
static char etc_wtmp[] = WTMP_FILENAME;
#endif

/*
 * Some people with 4.3bsd /bin/login seem to like to use login -p -f user
 * to implement xgterm -ls.  They can turn on USE_LOGIN_DASH_P and turn off
 * WTMP and LASTLOG.
 */
#ifdef USE_LOGIN_DASH_P
#ifndef LOGIN_FILENAME
#define LOGIN_FILENAME "/bin/login"
#endif
static char bin_login[] = LOGIN_FILENAME;
#endif

static int inhibit;
static char passedPty[2];	/* name if pty if slave */

#if defined(TIOCCONS) || defined(SRIOCSREDIR)
static int Console;
#include <X11/Xmu/SysUtil.h>	/* XmuGetHostname */
#define MIT_CONSOLE_LEN	12
#define MIT_CONSOLE "MIT_CONSOLE_"
static char mit_console_name[255 + MIT_CONSOLE_LEN + 1] = MIT_CONSOLE;
static Atom mit_console;
#endif	/* TIOCCONS */

#ifndef USE_SYSV_UTMP
static int tslot;
#endif	/* USE_SYSV_UTMP */
static jmp_buf env;

char *ProgramName;
Boolean sunFunctionKeys;

static struct _resource {
    char *xgterm_name;
    char *icon_geometry;
    char *title;
    char *icon_name;
    char *term_name;
    char *tty_modes;
    Boolean utmpInhibit;
    Boolean sunFunctionKeys;	/* %%% should be widget resource? */
    Boolean wait_for_map;
    Boolean useInsertMode;
} resource;

/* used by VT (charproc.c) */

#define offset(field)	XtOffsetOf(struct _resource, field)

static XtResource application_resources[] = {
    {"name", "Name", XtRString, sizeof(char *),
	offset(xgterm_name), XtRString, "xgterm"},
    {"iconGeometry", "IconGeometry", XtRString, sizeof(char *),
	offset(icon_geometry), XtRString, (caddr_t) NULL},
    {XtNtitle, XtCTitle, XtRString, sizeof(char *),
	offset(title), XtRString, (caddr_t) NULL},
    {XtNiconName, XtCIconName, XtRString, sizeof(char *),
	offset(icon_name), XtRString, (caddr_t) NULL},
    {"termName", "TermName", XtRString, sizeof(char *),
	offset(term_name), XtRString, (caddr_t) NULL},
    {"ttyModes", "TtyModes", XtRString, sizeof(char *),
	offset(tty_modes), XtRString, (caddr_t) NULL},
    {"utmpInhibit", "UtmpInhibit", XtRBoolean, sizeof (Boolean),
	offset(utmpInhibit), XtRString, "false"},
    {"sunFunctionKeys", "SunFunctionKeys", XtRBoolean, sizeof (Boolean),
	offset(sunFunctionKeys), XtRString, "false"},
    {"waitForMap", "WaitForMap", XtRBoolean, sizeof (Boolean),
        offset(wait_for_map), XtRString, "false"},
    {"useInsertMode", "UseInsertMode", XtRBoolean, sizeof (Boolean),
        offset(useInsertMode), XtRString, "false"},
};
#undef offset

static char *fallback_resources[] = {
    "XGterm*SimpleMenu*menuLabel.vertSpace: 100",
    "XGterm*SimpleMenu*HorizontalMargins: 16",
    "XGterm*SimpleMenu*Sme.height: 16",
    "XGterm*SimpleMenu*Cursor: left_ptr",
    "XGterm*mainMenu.Label:  Main Options (no app-defaults)",
    "XGterm*vtMenu.Label:  VT Options (no app-defaults)",
    "XGterm*fontMenu.Label:  VT Fonts (no app-defaults)",
    NULL
};

/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XrmParseCommand is let loose. */

static XrmOptionDescRec optionDescList[] = {
{"-geometry", "*vt100.geometry",       XrmoptionSepArg,	    (caddr_t) NULL},
{"-132",      "*c132",	               XrmoptionNoArg,	    (caddr_t) "on"},
{"+132",      "*c132",	               XrmoptionNoArg,	    (caddr_t) "off"},
{"-ah",	      "*alwaysHighlight",      XrmoptionNoArg,	    (caddr_t) "on"},
{"+ah",	      "*alwaysHighlight",      XrmoptionNoArg,	    (caddr_t) "off"},
{"-aw",	      "*autoWrap",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+aw",	      "*autoWrap",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-b",	      "*internalBorder",       XrmoptionSepArg,	    (caddr_t) NULL},
{"-cb",	      "*cutToBeginningOfLine", XrmoptionNoArg,      (caddr_t) "off"},
{"+cb",	      "*cutToBeginningOfLine", XrmoptionNoArg,      (caddr_t) "on"},
{"-cc",	      "*charClass",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-cn",	      "*cutNewline",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"+cn",	      "*cutNewline",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"-cr",	      "*cursorColor",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-cu",	      "*curses",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+cu",	      "*curses",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-dc",       "*dynamicColors",        XrmoptionNoArg,      (caddr_t) "on"},
{"+dc",       "*dynamicColors",        XrmoptionNoArg,      (caddr_t) "off"},
{"-e",	      NULL,	  	       XrmoptionSkipLine,   (caddr_t) NULL},
{"-fb",	      "*boldFont",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-im",	      "*useInsertMode",        XrmoptionNoArg,	    (caddr_t) "on"},
{"+im",	      "*useInsertMode",        XrmoptionNoArg,	    (caddr_t) "off"},
{"-j",	      "*jumpScroll",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+j",	      "*jumpScroll",	       XrmoptionNoArg,	    (caddr_t) "off"},
/* parse logging options anyway for compatibility */
{"-l",	      "*logging",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+l",	      "*logging",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-lf",	      "*logFile",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-ls",	      "*loginShell",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+ls",	      "*loginShell",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-mb",	      "*marginBell",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+mb",	      "*marginBell",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-mc",	      "*multiClickTime",       XrmoptionSepArg,	    (caddr_t) NULL},
{"-ms",	      "*pointerColor",         XrmoptionSepArg,	    (caddr_t) NULL},
{"-nb",	      "*nMarginBell",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-rw",	      "*reverseWrap",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+rw",	      "*reverseWrap",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-s",	      "*multiScroll",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+s",	      "*multiScroll",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-sb",	      "*scrollBar",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+sb",	      "*scrollBar",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-sbr",      "*scrollBarRight",       XrmoptionNoArg,      (caddr_t) "on"},
{"+sbr",      "*scrollBarRight",       XrmoptionNoArg,      (caddr_t) "off"},
{"-sf",	      "*sunFunctionKeys",      XrmoptionNoArg,	    (caddr_t) "on"},
{"+sf",	      "*sunFunctionKeys",      XrmoptionNoArg,	    (caddr_t) "off"},
{"-si",	      "*scrollTtyOutput",      XrmoptionNoArg,	    (caddr_t) "off"},
{"+si",	      "*scrollTtyOutput",      XrmoptionNoArg,	    (caddr_t) "on"},
{"-sk",	      "*scrollKey",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+sk",	      "*scrollKey",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-sl",	      "*saveLines",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-t",        "*tekStartup",           XrmoptionNoArg,      (caddr_t) "on"},
{"+t",        "*tekStartup",           XrmoptionNoArg,      (caddr_t) "off"},
{"-tm",	      "*ttyModes",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-tn",	      "*termName",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-ut",	      "*utmpInhibit",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+ut",	      "*utmpInhibit",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-vb",	      "*visualBell",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+vb",	      "*visualBell",	       XrmoptionNoArg,	    (caddr_t) "off"},
{"-wf",	      "*waitForMap",	       XrmoptionNoArg,	    (caddr_t) "on"},
{"+wf",	      "*waitForMap",	       XrmoptionNoArg,	    (caddr_t) "off"},
/* bogus old compatibility stuff for which there are
   standard XtAppInitialize options now */
{"-G",        "*tekGeometry",          XrmoptionSepArg,     (caddr_t) NULL},
{"%",         "*tekGeometry",          XrmoptionStickyArg,  (caddr_t) NULL},
{"#",	      ".iconGeometry",         XrmoptionStickyArg,  (caddr_t) NULL},
{"-T",	      "*title",	               XrmoptionSepArg,	    (caddr_t) NULL},
{"-n",	      "*iconName",	       XrmoptionSepArg,	    (caddr_t) NULL},
{"-r",	      "*reverseVideo",         XrmoptionNoArg,	    (caddr_t) "on"},
{"+r",	      "*reverseVideo",         XrmoptionNoArg,	    (caddr_t) "off"},
{"-rv",	      "*reverseVideo",         XrmoptionNoArg,	    (caddr_t) "on"},
{"+rv",	      "*reverseVideo",         XrmoptionNoArg,	    (caddr_t) "off"},
{"-w",	      ".borderWidth",          XrmoptionSepArg,	    (caddr_t) NULL},
};

static struct _options {
  char *opt;
  char *desc;
} options[] = {
{ "-help",                 "print out this message" },
{ "-display displayname",  "X server to contact" },
{ "-geometry geom",        "size (in characters) and position" },
{ "-G geom",  		   "geometry of toplevel graphics window" },
{ "-/+rv",                 "turn on/off reverse video" },
{ "-bg color",             "background color" },
{ "-fg color",             "foreground color" },
{ "-bd color",             "border color" },
{ "-bw number",            "border width in pixels" },
{ "-fn fontname",          "normal text font" },
{ "-iconic",               "start iconic" },
{ "-name string",          "client instance, icon, and title strings" },
{ "-title string",         "title string" },
{ "-xrm resourcestring",   "additional resource specifications" },
{ "-/+132",                "turn on/off column switch inhibiting" },
{ "-/+ah",                 "turn on/off always highlight" },
{ "-b number",             "internal border in pixels" },
{ "-/+cb",                 "turn on/off cut-to-beginning-of-line inhibit" },
{ "-cc classrange",        "specify additional character classes" },
{ "-/+cn",                 "turn on/off cut newline inhibit" },
{ "-cr color",             "text cursor color" },
{ "-/+cu",                 "turn on/off curses emulation" },
{ "-/+dc",                 "turn off/on dynamic color selection" },
{ "-/+sbr",                 "turn off/on rightside scrollbar option" },
{ "-fb fontname",          "bold text font" },
{ "-/+im",		   "use insert mode for TERMCAP" },
{ "-/+j",                  "turn on/off jump scroll" },
#ifdef ALLOWLOGGING
{ "-/+l",                  "turn on/off logging" },
{ "-lf filename",          "logging filename" },
#else
{ "-/+l",                  "turn on/off logging (not supported)" },
{ "-lf filename",          "logging filename (not supported)" },
#endif
{ "-/+ls",                 "turn on/off login shell" },
{ "-/+mb",                 "turn on/off margin bell" },
{ "-mc milliseconds",      "multiclick time in milliseconds" },
{ "-ms color",             "pointer color" },
{ "-nb number",            "margin bell in characters from right end" },
{ "-/+aw",                 "turn on/off auto wraparound" },
{ "-/+rw",                 "turn on/off reverse wraparound" },
{ "-/+s",                  "turn on/off multiscroll" },
{ "-/+sb",                 "turn on/off scrollbar" },
{ "-/+sf",                 "turn on/off Sun Function Key escape codes" },
{ "-/+si",                 "turn on/off scroll-on-tty-output inhibit" },
{ "-/+sk",                 "turn on/off scroll-on-keypress" },
{ "-sl number",            "number of scrolled lines to save" },
{ "-tm string",            "terminal mode keywords and characters" },
{ "-tn name",              "TERM environment variable name" },
#ifdef UTMP
{ "-/+ut",                 "turn on/off utmp inhibit" },
#else
{ "-/+ut",                 "turn on/off utmp inhibit (not supported)" },
#endif
{ "-/+vb",                 "turn on/off visual bell" },
{ "-/+wf",                 "turn on/off wait for map before command exec" },
{ "-e command args ...",   "command to execute" },
{ "%geom",                 "Tek window geometry" },
{ "#geom",                 "icon window geometry" },
{ "-T string",             "title name for window" },
{ "-n string",             "icon name for window" },
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
{ "-C",                    "intercept console messages" },
#else
{ "-C",                    "intercept console messages (not supported)" },
#endif
{ "-Sxxd",                 "slave mode on \"ttyxx\", file descriptor \"d\"" },
{ NULL, NULL }};

static char *message[] = {
"Fonts must be fixed width and, if both normal and bold are specified, must",
"have the same size.  If only a normal font is specified, it will be used for",
"both normal and bold text (by doing overstriking).  The -e option, if given,",
"must be appear at the end of the command line, otherwise the user's default",
"shell will be started.  Options that start with a plus sign (+) restore the",
"default.",
NULL};


/* The X11IRAF version. */
char *xgterm_version[] = {
#   include "../version.h"
    NULL
};


/*
 * If we're linked to terminfo, tgetent() will return an empty buffer.  We
 * cannot use that to adjust the $TERMCAP variable.
 */
static Boolean
get_termcap(char *name, char *buffer, char *resized)
{
    register TScreen *screen = &term->screen;

    *buffer = 0;        /* initialize, in case we're using terminfo's tgetent */

    if (name != 0) {
        if (tgetent(buffer, name) == 1) {
            if (*buffer) {
                if (!screen->TekEmu) {
                    resize(screen, buffer, resized);
                }
            }
            return True;
        } else {
            *buffer = 0;        /* just in case */
        }
    }
    return False;
}


static void Syntax (badOption)
    char *badOption;
{
    struct _options *opt;
    int col;

    fprintf (stderr, "%s:  bad command line option \"%s\"\r\n\n",
	     ProgramName, badOption);

    fprintf (stderr, "usage:  %s", ProgramName);
    col = 8 + strlen(ProgramName);
    for (opt = options; opt->opt; opt++) {
	int len = 3 + strlen(opt->opt);	 /* space [ string ] */
	if (col + len > 79) {
	    fprintf (stderr, "\r\n   ");  /* 3 spaces */
	    col = 3;
	}
	fprintf (stderr, " [%s]", opt->opt);
	col += len;
    }

    fprintf (stderr, "\r\n\nType %s -help for a full description.\r\n\n",
	     ProgramName);
    exit (1);
}

static void Help ()
{
    struct _options *opt;
    char **cpp;

    fprintf (stderr, "usage:\n        %s [-options ...] [-e command args]\n\n",
	     ProgramName);
    fprintf (stderr, "where options include:\n");
    for (opt = options; opt->opt; opt++) {
	fprintf (stderr, "    %-28s %s\n", opt->opt, opt->desc);
    }

    putc ('\n', stderr);
    for (cpp = message; *cpp; cpp++) {
	fputs (*cpp, stderr);
	putc ('\n', stderr);
    }
    putc ('\n', stderr);

    exit (0);
}

#if defined(TIOCCONS) || defined(SRIOCSREDIR)
/* ARGSUSED */
static Boolean
ConvertConsoleSelection(w, selection, target, type, value, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *value;
    unsigned long *length;
    int *format;
{
    /* we don't save console output, so can't offer it */
    return False;
}
#endif /* TIOCCONS */


extern WidgetClass xgtermWidgetClass;

Arg ourTopLevelShellArgs[] = {
	{ XtNallowShellResize, (XtArgVal) TRUE },	
	{ XtNinput, (XtArgVal) TRUE },
};
int number_ourTopLevelShellArgs = 2;
	
XtAppContext app_con;
Widget toplevel;
Bool waiting_for_initial_map;

extern void do_hangup();
extern void xt_error();

/*
 * DeleteWindow(): Action proc to implement ICCCM delete_window.
 */
/* ARGSUSED */
void
DeleteWindow(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
  if (w == toplevel)
    if (term->screen.Tshow)
      hide_vt_window();
    else
      do_hangup(w);
  else
    if (term->screen.Vshow)
      hide_tek_window();
    else
      do_hangup(w);
}

/* ARGSUSED */
void
KeyboardMapping(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    switch (event->type) {
       case MappingNotify:
	  XRefreshKeyboardMapping(&event->xmapping);
	  break;
    }
}

XtActionsRec actionProcs[] = {
    "DeleteWindow", DeleteWindow,
    "KeyboardMapping", KeyboardMapping,
};

Atom wm_delete_window;


main (argc, argv)
int argc;
char **argv;
{
	register TScreen *screen;
	register int pty;
	int Xsocket, mode;
	char *base_name();
	int xerror(), xioerror();

#ifdef I18N
        setlocale(LC_ALL, NULL);
#endif

	ProgramName = argv[0];

	ttydev = (char *) malloc (strlen (TTYDEV) + 1);
	ptydev = (char *) malloc (strlen (PTYDEV) + 1);
	if (!ttydev || !ptydev) {
	    fprintf (stderr, 
	    	     "%s:  unable to allocate memory for ttydev or ptydev\n",
		     ProgramName);
	    exit (1);
	}
	strcpy (ttydev, TTYDEV);
	strcpy (ptydev, PTYDEV);

#ifdef USE_SYSV_TERMIO
	/* Initialization is done here rather than above in order
	** to prevent any assumptions about the order of the contents
	** of the various terminal structures (which may change from
	** implementation to implementation).
	*/
#if defined(macII) || defined(ATT) || defined(CRAY)
	d_tio.c_iflag = ICRNL|IXON;
	d_tio.c_oflag = OPOST|ONLCR|TAB3;
    	d_tio.c_cflag = B9600|CS8|CREAD|PARENB|HUPCL;
    	d_tio.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK;

#ifndef USE_TERMIOS
	d_tio.c_line = 0;
#endif

	d_tio.c_cc[VINTR] = CINTR;
	d_tio.c_cc[VQUIT] = CQUIT;
	d_tio.c_cc[VERASE] = CERASE;
	d_tio.c_cc[VKILL] = CKILL;
    	d_tio.c_cc[VEOF] = CEOF;
	d_tio.c_cc[VEOL] = CNUL;
	d_tio.c_cc[VEOL2] = CNUL;
	d_tio.c_cc[VSWTCH] = CNUL;

#ifdef USE_TERMIOS
	d_tio.c_cc[VSUSP] = CSUSP;
#ifdef VDSUSP				/* R6 update	*/
	d_tio.c_cc[VDSUSP] = CDSUSP;
#endif
	d_tio.c_cc[VREPRINT] = CNUL;
	d_tio.c_cc[VDISCARD] = CNUL;
	d_tio.c_cc[VWERASE] = CNUL;
	d_tio.c_cc[VLNEXT] = CNUL;
#endif
#ifdef TIOCSLTC
        d_ltc.t_suspc = CSUSP;		/* t_suspc */
        d_ltc.t_dsuspc = CDSUSP;	/* t_dsuspc */
        d_ltc.t_rprntc = 0;		/* reserved...*/
        d_ltc.t_flushc = 0;
        d_ltc.t_werasc = 0;
        d_ltc.t_lnextc = 0;
#endif /* TIOCSLTC */
#ifdef TIOCLSET
	d_lmode = 0;
#endif /* TIOCLSET */
#else  /* else !macII */
	d_tio.c_iflag = ICRNL|IXON;
	d_tio.c_oflag = OPOST|ONLCR|TAB3;
#ifdef BAUD_0
    	d_tio.c_cflag = CS8|CREAD|PARENB|HUPCL;
#else	/* !BAUD_0 */
    	d_tio.c_cflag = B9600|CS8|CREAD|PARENB|HUPCL;
#endif	/* !BAUD_0 */
    	d_tio.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK;
#ifndef sgi
	d_tio.c_line = 0;
#endif
#ifdef linux
	d_tio.c_cc[VINTR] = 'C' & 0x3f;
	d_tio.c_cc[VQUIT] = '\\' & 0x3f;
	d_tio.c_cc[VERASE] = 0x7f;
	d_tio.c_cc[VKILL] = 'U' & 0x3f;
	d_tio.c_cc[VEOF] = 'D' & 0x3f;
	d_tio.c_cc[VEOL] = '@' & 0x3f;
#else        
	d_tio.c_cc[VINTR] = 0x7f;		/* DEL  */
	d_tio.c_cc[VQUIT] = '\\' & 0x3f;	/* '^\'	*/
	d_tio.c_cc[VERASE] = '#';		/* '#'	*/
	d_tio.c_cc[VKILL] = '@';		/* '@'	*/
    	d_tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/
	d_tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
#endif
#ifdef VSWTCH
	d_tio.c_cc[VSWTCH] = '@' & 0x3f;	/* '^@'	*/
#endif	/* VSWTCH */
	/* now, try to inherit tty settings */
	{
	    int i;

	    for (i = 0; i <= 2; i++) {
		struct termio deftio;
		if (ioctl (i, TCGETA, &deftio) == 0) {
		    d_tio.c_cc[VINTR] = deftio.c_cc[VINTR];
		    d_tio.c_cc[VQUIT] = deftio.c_cc[VQUIT];
		    d_tio.c_cc[VERASE] = deftio.c_cc[VERASE];
		    d_tio.c_cc[VKILL] = deftio.c_cc[VKILL];
		    d_tio.c_cc[VEOF] = deftio.c_cc[VEOF];
		    d_tio.c_cc[VEOL] = deftio.c_cc[VEOL];
#ifdef VSWTCH
		    d_tio.c_cc[VSWTCH] = deftio.c_cc[VSWTCH];
#endif /* VSWTCH */
		    break;
		}
	    }
	}
#ifdef TIOCSLTC
        d_ltc.t_suspc = '\000';		/* t_suspc */
        d_ltc.t_dsuspc = '\000';	/* t_dsuspc */
        d_ltc.t_rprntc = '\377';	/* reserved...*/
        d_ltc.t_flushc = '\377';
        d_ltc.t_werasc = '\377';
        d_ltc.t_lnextc = '\377';
#endif	/* TIOCSLTC */
#ifdef USE_TERMIOS
#ifdef linux
	d_tio.c_cc[VSUSP] = 'Z' & 0x3f;
#else
	d_tio.c_cc[VSUSP] = '\000';
#endif
#ifdef VDSUSP
	d_tio.c_cc[VDSUSP] = '\000';
#endif
	d_tio.c_cc[VREPRINT] = '\377';
	d_tio.c_cc[VDISCARD] = '\377';
	d_tio.c_cc[VWERASE] = '\377';
	d_tio.c_cc[VLNEXT] = '\377';
#endif
#ifdef TIOCLSET
	d_lmode = 0;
#endif	/* TIOCLSET */
#endif  /* macII */
#endif	/* USE_SYSV_TERMIO */

	/* Init the Toolkit. */
        {
#ifdef HAS_POSIX_SAVED_IDS
            uid_t euid = geteuid();
            gid_t egid = getegid();
            uid_t ruid = getuid();
            gid_t rgid = getgid();

            if (setegid(ruid) == -1)
                (void) fprintf(stderr, "setegid(%d): %s\n",
                               rgid, strerror(errno));

            if (seteuid(ruid) == -1)
                (void) fprintf(stderr, "seteuid(%d): %s\n",
                               ruid, strerror(errno));
#endif

	XtSetErrorHandler(xt_error);
	toplevel = XtAppInitialize (&app_con, "XGterm", 
				    optionDescList, XtNumber(optionDescList), 
				    &argc, argv, fallback_resources, NULL, 0);

	XtGetApplicationResources(toplevel, (XtPointer) &resource,
				  application_resources,
				  XtNumber(application_resources), NULL, 0);

#ifdef HAS_POSIX_SAVED_IDS
            if (seteuid(euid) == -1)
                (void) fprintf(stderr, "seteuid(%d): %s\n",
                               euid, strerror(errno));

            if (setegid(egid) == -1)
                (void) fprintf(stderr, "setegid(%d): %s\n",
                               egid, strerror(errno));
#endif
	}


	waiting_for_initial_map = resource.wait_for_map;

	/*
	 * ICCCM delete_window.
	 */
	XtAppAddActions(app_con, actionProcs, XtNumber(actionProcs));

	/*
	 * fill in terminal modes
	 */
	if (resource.tty_modes) {
	    int n = parse_tty_modes (resource.tty_modes, ttymodelist);
	    if (n < 0) {
		fprintf (stderr, "%s:  bad tty modes \"%s\"\n",
			 ProgramName, resource.tty_modes);
	    } else if (n > 0) {
		override_tty_modes = 1;
	    }
	}

	xgterm_name = resource.xgterm_name;
	sunFunctionKeys = resource.sunFunctionKeys;
	if (strcmp(xgterm_name, "-") == 0) xgterm_name = "xgterm";
	if (resource.icon_geometry != NULL) {
	    int scr, junk;
	    int ix, iy;
	    Arg args[2];

	    for(scr = 0;	/* yyuucchh */
		XtScreen(toplevel) != ScreenOfDisplay(XtDisplay(toplevel),scr);
		scr++);

	    args[0].name = XtNiconX;
	    args[1].name = XtNiconY;
	    XGeometry(XtDisplay(toplevel), scr, resource.icon_geometry, "",
		      0, 0, 0, 0, 0, &ix, &iy, &junk, &junk);
	    args[0].value = (XtArgVal) ix;
	    args[1].value = (XtArgVal) iy;
	    XtSetValues( toplevel, args, 2);
	}

	XtSetValues (toplevel, ourTopLevelShellArgs, 
		     number_ourTopLevelShellArgs);


	/* Parse the rest of the command line */
	for (argc--, argv++ ; argc > 0 ; argc--, argv++) {
	    if(**argv != '-') Syntax (*argv);

	    switch(argv[0][1]) {
	     case 'h':
		Help ();
		/* NOTREACHED */
	     case 'C':
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
		{
		    struct stat sbuf;

		    /* Must be owner and have read/write permission.
		       xdm cooperates to give the console the right user. */
		    if ( !stat("/dev/console", &sbuf) &&
			 (sbuf.st_uid == getuid()) &&
			 !access("/dev/console", R_OK|W_OK))
		    {
			Console = TRUE;
		    } else
			Console = FALSE;
		}
#endif	/* TIOCCONS */
		continue;
	     case 'S':
		if (sscanf(*argv + 2, "%c%c%d", passedPty, passedPty+1,
			   &am_slave) != 3)
		    Syntax(*argv);
		continue;
#ifdef DEBUG
	     case 'D':
		debug = TRUE;
		continue;
#endif	/* DEBUG */
	     case 'e':
		if (argc <= 1) Syntax (*argv);
		command_to_exec = ++argv;
		argc = 0;
		break;
	     case 'v':
		if (strcmp (*argv, "-version") == 0) {
                    printf ("Version:  %s\n", xgterm_version[0]);
                    exit (1);
		}
		break;
	     default:
		Syntax (*argv);
	    }
	    break;
	}

	XawSimpleMenuAddGlobalActions (app_con);
	XtRegisterGrabAction (HandlePopupMenu, True,
			      (ButtonPressMask|ButtonReleaseMask),
			      GrabModeAsync, GrabModeAsync);

	/* Create the vt100 terminal emulator widget. */
        term = (XgtermWidget) XtCreateManagedWidget ("vt100",
	    xgtermWidgetClass, toplevel, NULL, 0);
        screen = &term->screen;

	if (screen->savelines < 0) screen->savelines = 0;

	term->flags = 0;
	if (!screen->jumpscroll) {
	    term->flags |= SMOOTHSCROLL;
	    update_jumpscroll();
	}
	if (term->misc.reverseWrap) {
	    term->flags |= REVERSEWRAP;
	    update_reversewrap();
	}
	if (term->misc.autoWrap) {
	    term->flags |= WRAPAROUND;
	    update_autowrap();
	}
	if (term->misc.re_verse) {
	    term->flags |= REVERSE_VIDEO;
	    update_reversevideo();
	}

	inhibit = 0;
#ifdef ALLOWLOGGING
	if (term->misc.logInhibit) 	    inhibit |= I_LOG;
#endif
	if (term->misc.signalInhibit)		inhibit |= I_SIGNAL;
	if (term->misc.tekInhibit)		inhibit |= I_TEK;

	term->initflags = term->flags;

	if (term->misc.appcursorDefault) {
	    term->keyboard.flags |= CURSOR_APL;
	    update_appcursor();
	}

	if (term->misc.appkeypadDefault) {
	    term->keyboard.flags |= KYPD_APL;
	    update_appkeypad();
	}

/*
 * Set title and icon name if not specified
 */

	if (command_to_exec) {
	    Arg args[2];

	    if (!resource.title) {
		if (command_to_exec) {
		    resource.title = base_name (command_to_exec[0]);
		} /* else not reached */
	    }

	    if (!resource.icon_name) 
	      resource.icon_name = resource.title;
	    XtSetArg (args[0], XtNtitle, resource.title);
	    XtSetArg (args[1], XtNiconName, resource.icon_name);		

	    XtSetValues (toplevel, args, 2);
	}

#ifdef DEBUG
    {
	/* Set up stderr properly.  Opening this log file cannot be
	 done securely by a privileged xgterm process (although we try),
	 so the debug feature is disabled by default. */
	int i = -1;
	if(debug) {
	        creat_as (getuid(), getgid(), "xgterm.debug.log", 0666);
		i = open ("xgterm.debug.log", O_WRONLY | O_TRUNC, 0666);
	}
	if(i >= 0) {
#if defined(USE_SYSV_TERMIO) && !defined(SVR4) && !defined(linux)
		/* SYSV has another pointer which should be part of the
		** FILE structure but is actually a seperate array.
		*/
		unsigned char *old_bufend;

		old_bufend = (unsigned char *) _bufend(stderr);
#if defined(hpux) || defined(__hpux)
		stderr->__fileH = (i >> 8);
		stderr->__fileL = i;
#else
		stderr->_file = i;
#endif
		_bufend(stderr) = old_bufend;
#else	/* USE_SYSV_TERMIO */
#ifdef linux
		setfileno(stderr, i);
#else
		stderr->_file = i;
#endif
#endif	/* USE_SYSV_TERMIO */

		/* mark this file as close on exec */
		(void) fcntl(i, F_SETFD, 1);
	}
    }
#endif	/* DEBUG */

	/* open a terminal for client */
	get_terminal ();
	spawn ();


	/* Child process is out there, let's catch its termination */
#ifdef USE_POSIX_SIGNALS
    (void) posix_signal(SIGCHLD, reapchild);
#else
    (void) signal(SIGCHLD, reapchild);
#endif


	/* Realize procs have now been executed */

	Xsocket = ConnectionNumber(screen->display);
	pty = screen->respond;
	init_ttyio (pty);

	/* Connect gtermio to the pty. */
	if (!term->misc.tekInhibit) {
	    gio_setup (app_con, argc, argv, pty);
	    gio_postconnectcallback (gtermio_connect, NULL);
	}

	/* Start up graphics window? */
        if (inhibit & I_TEK)
                screen->TekEmu = FALSE;
        if (screen->TekEmu)
	    hide_vt_window();

	/* Write window id so master end can read and use */
	if (am_slave >= 0) { 
	    char buf[80];

	    buf[0] = '\0';
	    sprintf (buf, "%lx\n", XtWindow (XtParent (term)));
	    write (pty, buf, strlen (buf));
	}

#ifdef ALLOWLOGGING
	if (term->misc.log_on) {
		StartLog(screen);
	}
#endif
	screen->inhibit = inhibit;

#ifdef AIXV3
	/* In AIXV3, xterms started from /dev/console have CLOCAL set.
	 * This means we need to clear CLOCAL so that SIGHUP gets sent
	 * to the slave-pty process when xterm exits. 
	 */

	{
	    struct termio tio;

	    if(ioctl(pty, TCGETA, &tio) == -1)
		SysError(ERROR_TIOCGETP);

	    tio.c_cflag &= ~(CLOCAL);

	    if (ioctl (pty, TCSETA, &tio) == -1)
		SysError(ERROR_TIOCSETP);
	}
#endif
#ifdef USE_SYSV_TERMIO
	if (0 > (mode = fcntl(pty, F_GETFL, 0)))
		Error();
#ifdef O_NDELAY
	mode |= O_NDELAY;
#else
	mode |= O_NONBLOCK;
#endif /* O_NDELAY */
	if (fcntl(pty, F_SETFL, mode))
		Error();
#else	/* USE_SYSV_TERMIO */
	mode = 1;
	if (ioctl (pty, FIONBIO, (char *)&mode) == -1) SysError (ERROR_FIONBIO);
#endif	/* USE_SYSV_TERMIO */
	
	pty_mask = 1 << pty;
	X_mask = 1 << Xsocket;
	Select_mask = pty_mask | X_mask;
	max_plus1 = (pty < Xsocket) ? (1 + Xsocket) : (1 + pty);

#ifdef DEBUG
	if (debug) printf ("debugging on\n");
#endif	/* DEBUG */
	XSetErrorHandler(xerror);
	XSetIOErrorHandler(xioerror);
	for( ; ; )
	    VTRun();
}

char *base_name(name)
char *name;
{
	register char *cp;

	cp = strrchr(name, '/');
	return(cp ? cp + 1 : name);
}




#ifdef NEW_GET_PTY


#ifdef __DARWIN__
#define USE_OPENPTY 1
static int opened_tty = -1;
#endif

/*
 * This function opens up a pty master and stuffs its value into pty.
 *
 * If it finds one, it returns a value of 0.  If it does not find one,
 * it returns a value of !0.  This routine is designed to be re-entrant,
 * so that if a pty master is found and later, we find that the slave
 * has problems, we can re-enter this function and get another one.
 */
static int
get_pty(int *pty, char *from)
{
    int result = 1;


#if defined(USE_OPENPTY)
    result = openpty(pty, &opened_tty, ttydev, NULL, NULL);

#elif defined(__OpenBSD__)
    static int m_tty = -1;
    static int m_pty = -1;
    struct group *ttygrp;


    if (pty == NULL) {
	result = openpty(&m_pty, &m_tty, ttydev, NULL, NULL);

	seteuid(0);
	if ((ttygrp = getgrnam(TTY_GROUP_NAME)) != 0) {
	    set_owner(ttydev, getuid(), ttygrp->gr_gid, 0600);
	} else {
	    set_owner(ttydev, getuid(), getgid(), 0600);
	}
	seteuid(getuid());
    } else if (m_pty != -1) {
	*pty = m_pty;
	result = 0;
    } else {
	result = -1;
    }
#elif defined(PUCC_PTYD)

    result = ((*pty = openrpty(ttydev, ptydev,
			       (resource.utmpInhibit ? OPTY_NOP : OPTY_LOGIN),
			       getuid(), from)) < 0);

/*
#elif defined(__osf__) || (defined(__GLIBC__) && !defined(USE_USG_PTYS)) || defined(__NetBSD__)

    int tty;
    result = openpty(pty, &tty, ttydev, NULL, NULL);
*/
#elif defined(__QNXNTO__)

    result = pty_search(pty);

#else
#if defined(USE_ISPTS_FLAG)

    /*
       The order of this code is *important*.  On SYSV/386 we want to open
       a /dev/ttyp? first if at all possible.  If none are available, then
       we'll try to open a /dev/pts??? device.

       The reason for this is because /dev/ttyp? works correctly, where
       as /dev/pts??? devices have a number of bugs, (won't update
       screen correcly, will hang -- it more or less works, but you
       really don't want to use it).

       Most importantly, for boxes of this nature, one of the major
       "features" is that you can emulate a 8086 by spawning off a UNIX
       program on 80386/80486 in v86 mode.  In other words, you can spawn
       off multiple MS-DOS environments.  On ISC the program that does
       this is named "vpix."  The catcher is that "vpix" will *not* work
       with a /dev/pts??? device, will only work with a /dev/ttyp? device.

       Since we can open either a /dev/ttyp? or a /dev/pts??? device,
       the flag "IsPts" is set here so that we know which type of
       device we're dealing with in routine spawn().  That's the reason
       for the "if (IsPts)" statement in spawn(); we have two different
       device types which need to be handled differently.
     */
    result = pty_search(pty);
    if (!result)
	IsPts = 0;

#endif
#if defined(USE_USG_PTYS) || defined(__CYGWIN__) || defined(FEDORA)
#ifdef __GLIBC__		/* if __GLIBC__ and USE_USG_PTYS, we know glibc >= 2.1 */
    /* GNU libc 2 allows us to abstract away from having to know the
       master pty device name. */
    if ((*pty = getpt()) >= 0) {
	char *name = ptsname(*pty);
	if (name != 0) {	/* if filesystem is trashed, this may be null */
	    strcpy(ttydev, name);
	    result = 0;
	}
    }
#elif defined(__MVS__)
    result = pty_search(pty);
#else
#if defined(USE_ISPTS_FLAG)
    if (result) {
#endif
	result = ((*pty = open("/dev/ptmx", O_RDWR)) < 0);
#endif
#if defined(SVR4) || defined(__SCO__) || defined(USE_ISPTS_FLAG)
	if (!result)
	    strcpy(ttydev, ptsname(*pty));
#ifdef USE_ISPTS_FLAG
	IsPts = !result;	/* true if we're successful */
    }
#endif
#endif

#elif defined(AIXV3)

    if ((*pty = open("/dev/ptc", O_RDWR)) >= 0) {
	strcpy(ttydev, ttyname(*pty));
	result = 0;
    }
#elif defined(__convex__)

    char *pty_name;
    extern char *getpty(void);

    while ((pty_name = getpty()) != NULL) {
	if ((*pty = open(pty_name, O_RDWR)) >= 0) {
	    strcpy(ptydev, pty_name);
	    strcpy(ttydev, pty_name);
	    *x_basename(ttydev) = 't';
	    result = 0;
	    break;
	}
    }

#elif defined(sequent)

    result = ((*pty = getpseudotty(&ttydev, &ptydev)) < 0);

#elif defined(__sgi) && (OSMAJORVERSION >= 4)

    char *tty_name;

    tty_name = _getpty(pty, O_RDWR, 0622, 0);
    if (tty_name != 0) {
	strcpy(ttydev, tty_name);
	result = 0;
    }
#elif (defined(__sgi) && (OSMAJORVERSION < 4)) || (defined(umips) && defined (SYSTYPE_SYSV))

    struct stat fstat_buf;

    *pty = open("/dev/ptc", O_RDWR);
    if (*pty >= 0 && (fstat(*pty, &fstat_buf)) >= 0) {
	result = 0;
	sprintf(ttydev, "/dev/ttyq%d", minor(fstat_buf.st_rdev));
    }
#elif defined(__hpux)

    /*
     * Use the clone device if it works, otherwise use pty_search logic.
     */
    if ((*pty = open("/dev/ptym/clone", O_RDWR)) >= 0) {
	char *name = ptsname(*pty);
	if (name != 0) {
	    strcpy(ttydev, name);
	    result = 0;
	} else {		/* permissions, or other unexpected problem */
	    close(*pty);
	    *pty = -1;
	    result = pty_search(pty);
	}
    } else {
	result = pty_search(pty);
    }

#else

    result = pty_search(pty);

#endif
#endif

    return result;
}
#else



/* This function opens up a pty master and stuffs its value into pty.
 * If it finds one, it returns a value of 0.  If it does not find one,
 * it returns a value of !0.  This routine is designed to be re-entrant,
 * so that if a pty master is found and later, we find that the slave
 * has problems, we can re-enter this function and get another one.
 */

get_pty (pty)
    int *pty;
{
#if defined(SYSV) && defined(SYSV386)
        /*
	  The order of this code is *important*.  On SYSV/386 we want to open
	  a /dev/ttyp? first if at all possible.  If none are available, then
	  we'll try to open a /dev/pts??? device.
	  
	  The reason for this is because /dev/ttyp? works correctly, where
	  as /dev/pts??? devices have a number of bugs, (won't update
	  screen correcly, will hang -- it more or less works, but you
	  really don't want to use it).
	  
	  Most importantly, for boxes of this nature, one of the major
	  "features" is that you can emulate a 8086 by spawning off a UNIX
	  program on 80386/80486 in v86 mode.  In other words, you can spawn
	  off multiple MS-DOS environments.  On ISC the program that does
	  this is named "vpix."  The catcher is that "vpix" will *not* work
	  with a /dev/pts??? device, will only work with a /dev/ttyp? device.
	  
	  Since we can open either a /dev/ttyp? or a /dev/pts??? device,
	  the flag "IsPts" is set here so that we know which type of
	  device we're dealing with in routine spawn().  That's the reason
	  for the "if (IsPts)" statement in spawn(); we have two different
	  device types which need to be handled differently.
	  */
        if (pty_search(pty) == 0)
	    return 0;
#endif /* SYSV && SYSV386 */

/* Need to move this block of code up a bit on IRIX 6.5 systems to avoid
 * a segvio when running out of ptys.
 */
#if defined(sgi) && OSMAJORVERSION >= 6 && OSMINORVERSION >= 5
	{
	    char    *tty_name;

	    tty_name = _getpty (pty, O_RDWR, 0622, 0);
	    if (tty_name == 0)
		return 1;
	    strcpy (ttydev, tty_name);
	    return 0;
	}
#endif
#ifdef ATT
	if ((*pty = open ("/dev/ptmx", O_RDWR)) < 0) {
	    return 1;
	}
#if defined(SVR4) || defined(SYSV386)
	strcpy(ttydev, ptsname(*pty));
#if defined (SYSV) && defined(SYSV386)
	IsPts = True;
#endif
#endif
	return 0;
#else /* ATT else */
#ifdef AIXV3
	if ((*pty = open ("/dev/ptc", O_RDWR)) < 0) {
	    return 1;
	}
	strcpy(ttydev, ttyname(*pty));
	return 0;
#endif

/* Original code for systems below IRIX 6.5 */
#if defined(sgi) && OSMAJORVERSION >= 4 && (OSMAJORVERSION <= 6 && OSMINORVERSION < 5)
	{
	    char    *tty_name;

	    tty_name = _getpty (pty, O_RDWR, 0622, 0);
	    if (tty_name == 0)
		return 1;
	    strcpy (ttydev, tty_name);
	    return 0;
	}
#endif
#ifdef __convex__
        {
	    char *pty_name, *getpty();

	    while ((pty_name = getpty()) != NULL) {
		if ((*pty = open (pty_name, O_RDWR)) >= 0) {
		    strcpy(ptydev, pty_name);
		    strcpy(ttydev, pty_name);
		    ttydev[5] = 't';
		    return 0;
		}
	    }
	    return 1;
	}
#endif /* __convex__ */
#ifdef USE_GET_PSEUDOTTY
	return ((*pty = getpseudotty (&ttydev, &ptydev)) >= 0 ? 0 : 1);
#else
#if (defined(sgi) && OSMAJORVERSION < 4) || (defined(umips) && defined (SYSTYPE_SYSV))
	struct stat fstat_buf;

	*pty = open ("/dev/ptc", O_RDWR);
	if (*pty < 0 || (fstat (*pty, &fstat_buf)) < 0) {
	  return(1);
	}
	sprintf (ttydev, "/dev/ttyq%d", minor(fstat_buf.st_rdev));
#ifndef sgi
	sprintf (ptydev, "/dev/ptyq%d", minor(fstat_buf.st_rdev));
	if ((*tty = open (ttydev, O_RDWR)) < 0) {
	  close (*pty);
	  return(1);
	}
#endif /* !sgi */
	/* got one! */
	return(0);
#else /* sgi or umips */

	return pty_search(pty);

#endif /* sgi or umips else */
#endif /* USE_GET_PSEUDOTTY else */
#endif /* ATT else */
}
#endif


/*
 * Called from get_pty to iterate over likely pseudo terminals
 * we might allocate.  Used on those systems that do not have
 * a functional interface for allocating a pty.
 * Returns 0 if found a pty, 1 if fails.
 */
int pty_search(pty)
    int *pty;
{
    static int devindex, letter = 0;

#ifdef CRAY
    for (; devindex < 256; devindex++) {
	sprintf (ttydev, "/dev/ttyp%03d", devindex);
	sprintf (ptydev, "/dev/pty/%03d", devindex);

	if ((*pty = open (ptydev, O_RDWR)) >= 0) {
	    /* We need to set things up for our next entry
	     * into this function!
	     */
	    (void) devindex++;
	    return 0;
	}
    }
#else /* CRAY */
    while (PTYCHAR1[letter]) {
	ttydev [strlen(ttydev) - 2]  = ptydev [strlen(ptydev) - 2] =
	    PTYCHAR1 [letter];

	while (PTYCHAR2[devindex]) {
	    ttydev [strlen(ttydev) - 1] = ptydev [strlen(ptydev) - 1] =
		PTYCHAR2 [devindex];
	    /* for next time around loop or next entry to this function */
	    devindex++;
	    if ((*pty = open (ptydev, O_RDWR)) >= 0) {
#ifdef sun
		/* Need to check the process group of the pty.
		 * If it exists, then the slave pty is in use,
		 * and we need to get another one.
		 */
		int pgrp_rtn;
		if (ioctl(*pty, TIOCGPGRP, &pgrp_rtn) == 0 || errno != EIO) {
		    close(*pty);
		    continue;
		}
#endif /* sun */
		return 0;
	    }
	}
	devindex = 0;
	(void) letter++;
    }
#endif /* CRAY else */
    /*
     * We were unable to allocate a pty master!  Return an error
     * condition and let our caller terminate cleanly.
     */
    return 1;
}

get_terminal ()
/* 
 * sets up X and initializes the terminal structure except for term.buf.fildes.
 */
{
	register TScreen *screen = &term->screen;
	
	screen->arrow = make_colored_cursor (XC_left_ptr, 
					     screen->mousecolor,
					     screen->mousecolorback);
}

/*
 * The only difference in /etc/termcap between 4014 and 4015 is that
 * the latter has support for switching character sets.  We support the
 * 4015 protocol, but ignore the character switches.  Therefore, we
 * choose 4014 over 4015.
 *
 * Features of the 4014 over the 4012: larger (19") screen, 12-bit
 * graphics addressing (compatible with 4012 10-bit addressing),
 * special point plot mode, incremental plot mode (not implemented in
 * later Tektronix terminals), and 4 character sizes.
 * All of these are supported by xterm.
 */

static char *tekterm[] = {
        "tek4014",
        "tek4015",              /* 4014 with APL character set support */
        "tek4012",              /* 4010 with lower case */
        "tek4013",              /* 4012 with APL character set support */
        "tek4010",              /* small screen, upper-case only */
        "dumb",
        0
};

/* The VT102 is a VT100 with the Advanced Video Option included standard.
 * It also adds Escape sequences for insert/delete character/line.
 * The VT220 adds 8-bit character sets, selective erase.
 * The VT320 adds a 25th status line, terminal state interrogation.
 * The VT420 has up to 48 lines on the screen.
 */

static char *vtterm[] = {
#ifdef USE_X11TERM
	"x11term",		/* for people who want special term name */
#endif
	"xgterm",		/* technically correct name */
	"xterm",		/* more likely to be found */
	"vt102",
	"vt100",
	"ansi",
	"dumb",
	0
};

/* ARGSUSED */
SIGNAL_T hungtty(i)
	int i;
{
	longjmp(env, 1);
	SIGNAL_RETURN;
}

#ifdef USE_HANDSHAKE
typedef enum {		/* c == child, p == parent                        */
	PTY_BAD,	/* c->p: can't open pty slave for some reason     */
	PTY_FATALERROR,	/* c->p: we had a fatal error with the pty        */
	PTY_GOOD,	/* c->p: we have a good pty, let's go on          */
	PTY_NEW,	/* p->c: here is a new pty slave, try this        */
	PTY_NOMORE,	/* p->c; no more pty's, terminate                 */
	UTMP_ADDED,	/* c->p: utmp entry has been added                */
	UTMP_TTYSLOT,	/* c->p: here is my ttyslot                       */
	PTY_EXEC	/* p->c: window has been mapped the first time    */
} status_t;

typedef struct {
	status_t status;
	int error;
	int fatal_error;
	int tty_slot;
	int rows;
	int cols;
	char buffer[1024];
} handshake_t;

/* HsSysError()
 *
 * This routine does the equivalent of a SysError but it handshakes
 * over the errno and error exit to the master process so that it can
 * display our error message and exit with our exit code so that the
 * user can see it.
 */

void
HsSysError(pf, error)
int pf;
int error;
{
	handshake_t handshake;

	handshake.status = PTY_FATALERROR;
	handshake.error = errno;
	handshake.fatal_error = error;
	strcpy(handshake.buffer, ttydev);
	write(pf, (char *) &handshake, sizeof(handshake));
	exit(error);
}

static int pc_pipe[2];	/* this pipe is used for parent to child transfer */
static int cp_pipe[2];	/* this pipe is used for child to parent transfer */

void first_map_occurred ()
{
    handshake_t handshake;
    register TScreen *screen = &term->screen;

    handshake.status = PTY_EXEC;
    handshake.rows = screen->max_row;
    handshake.cols = screen->max_col;
    write (pc_pipe[1], (char *) &handshake, sizeof(handshake));
    close (cp_pipe[0]);
    close (pc_pipe[1]);
    waiting_for_initial_map = False;
}
#else
/*
 * temporary hack to get xgterm working on att ptys
 */
void first_map_occurred ()
{
    return;
}
#define HsSysError(a,b)
#endif /* USE_HANDSHAKE else !USE_HANDSHAKE */


#ifdef NEW_SPAWN

extern char **environ;

char *
x_basename(char *name)
{
    char *cp;

    cp = strrchr(name, '/');
#ifdef __UNIXOS2__
    if (cp == 0)
        cp = strrchr(name, '\\');
#endif
    return (cp ? cp + 1 : name);
}


static void
set_owner(char *device, int uid, int gid, int mode)
{
    if (chown(device, uid, gid) < 0) {
        if (errno != ENOENT
            && getuid() == 0) {
            fprintf(stderr, "Cannot chown %s to %d,%d: %s\n",
                    device, uid, gid, strerror(errno));
        }
    }
    chmod(device, mode);
}


/*
 * sets the value of var to be arg in the Unix 4.2 BSD environment env.
 * Var should end with '=' (bindings are of the form "var=value").
 * This procedure assumes the memory for the first level of environ
 * was allocated using calloc, with enough extra room at the end so not
 * to have to do a realloc().
 */
void
xtermSetenv(register char *var, register char *value)
{
    register int envindex = 0;
    register size_t len = strlen(var);

    while (environ[envindex] != NULL) {
        if (strncmp(environ[envindex], var, len) == 0) {
            /* found it */
            environ[envindex] = (char *) malloc((unsigned) len
                                                + strlen(value) + 1);
            strcpy(environ[envindex], var);
            strcat(environ[envindex], value);
            return;
        }
        envindex++;
    }

    environ[envindex] = (char *) malloc((unsigned) len + strlen(value) + 1);
    (void) strcpy(environ[envindex], var);
    strcat(environ[envindex], value);
    environ[++envindex] = NULL;
}


/*
 * Some platforms use names such as /dev/tty01, others /dev/pts/1.  Parse off
 * the "tty01" or "pts/1" portion, and return that for use as an identifier for
 * utmp.
 */
static char *
my_pty_name(char *device)
{
    size_t len = strlen(device);
    Boolean name = False;

    while (len != 0) {
        int ch = device[len - 1];
        if (isdigit(ch)) {
            len--;
        } else if (ch == '/') {
            if (name)
                break;
            len--;
        } else if (isalpha(ch)) {
            name = True;
            len--;
        } else {
            break;
        }
    }
    return device + len;
}

/*
 * If the name contains a '/', it is a "pts/1" case.  Otherwise, return the
 * last few characters for a utmp identifier.
 */
static char *
my_pty_id(char *device)
{
    char *name = my_pty_name(device);
    char *leaf = x_basename(name);

    if (name == leaf) {         /* no '/' in the name */
        int len = strlen(leaf);
        if (PTYCHARLEN < len)
            leaf = leaf + (len - PTYCHARLEN);
    }
    return leaf;
}


/*
 * Set the tty/pty identifier
 */
static void
set_pty_id(char *device, char *id)
{
    char *name = my_pty_name(device);
    char *leaf = x_basename(name);

    if (name == leaf) {
        strcpy(my_pty_id(device), id);
    } else {
        strcpy(leaf, id);
    }
}




spawn(void)
/*
 *  Inits pty and tty and forks a login process.
 *  Does not close fd Xsocket.
 *  If slave, the pty named in passedPty is already open for use
 */
{
    register TScreen *screen = &term->screen;
#if OPT_PTY_HANDSHAKE
    handshake_t handshake;
    int done;
#endif
#if OPT_INITIAL_ERASE
    int initial_erase = VAL_INITIAL_ERASE;
#endif
    int rc = 0;
    int tty = -1;
#ifdef USE_ANY_SYSV_TERMIO
    struct termio tio;
#ifdef TIOCLSET
    unsigned lmode;
#endif /* TIOCLSET */
#ifdef HAS_LTCHARS
    struct ltchars ltc;
#endif /* HAS_LTCHARS */
#elif defined(USE_POSIX_TERMIOS)
    struct termios tio;
#else /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */
    int ldisc = 0;
    int discipline;
    unsigned lmode;
    struct tchars tc;
    struct ltchars ltc;
    struct sgttyb sg;
#ifdef sony
    int jmode;
    struct jtchars jtc;
#endif /* sony */
#endif /* USE_ANY_SYSV_TERMIO */

    char termcap[TERMCAP_SIZE];
    char newtc[TERMCAP_SIZE];
    char *ptr, *shname, *shname_minus;
    int i, no_dev_tty = FALSE;
    char **envnew;		/* new environment */
    int envsize;		/* elements in new environment */
    char buf[64];
    char *TermName = NULL;
#ifdef TTYSIZE_STRUCT
    TTYSIZE_STRUCT ts;
#endif
    struct passwd *pw = NULL;
    char *login_name = NULL;
#ifndef USE_UTEMPTER
#ifdef HAVE_UTMP
    struct UTMP_STR utmp;
#ifdef USE_SYSV_UTMP
    struct UTMP_STR *utret = NULL;
#endif
#ifdef USE_LASTLOG
    struct lastlog lastlog;
#endif
#ifdef USE_LASTLOGX
    struct lastlogx lastlog;
#endif /* USE_LASTLOG */
#endif /* HAVE_UTMP */
#endif /* !USE_UTEMPTER */
    char *x_basename();

    /* Noisy compilers (suppress some unused-variable warnings) */
    (void) rc;
#if defined(HAVE_UTMP) && defined(USE_SYSV_UTMP) && !defined(USE_UTEMPTER)
    (void) utret;
#endif

    screen->uid = getuid();
    screen->gid = getgid();

    termcap[0] = '\0';
    newtc[0] = '\0';

#ifdef SIGTTOU
    /* so that TIOCSWINSZ || TIOCSIZE doesn't block */
    signal(SIGTTOU, SIG_IGN);
#endif

    if (am_slave >= 0) {
	screen->respond = am_slave;
	set_pty_id(ttydev, passedPty);
#ifdef USE_PTY_DEVICE
	set_pty_id(ptydev, passedPty);
#endif
	setgid(screen->gid);
	setuid(screen->uid);
    } else {
	Bool tty_got_hung;

	/*
	 * Sometimes /dev/tty hangs on open (as in the case of a pty
	 * that has gone away).  Simply make up some reasonable
	 * defaults.
	 */

	signal(SIGALRM, hungtty);
	alarm(2);		/* alarm(1) might return too soon */
	if (!sigsetjmp(env, 1)) {
	    tty = open("/dev/tty", O_RDWR);
	    alarm(0);
	    tty_got_hung = False;
	} else {
	    tty_got_hung = True;
	    tty = -1;
	    errno = ENXIO;
	}
#if OPT_INITIAL_ERASE
	initial_erase = VAL_INITIAL_ERASE;
#endif
	signal(SIGALRM, SIG_DFL);

	/*
	 * Check results and ignore current control terminal if
	 * necessary.  ENXIO is what is normally returned if there is
	 * no controlling terminal, but some systems (e.g. SunOS 4.0)
	 * seem to return EIO.  Solaris 2.3 is said to return EINVAL.
	 * Cygwin returns ENOENT.
	 */
	no_dev_tty = FALSE;
	if (tty < 0) {
	    if (tty_got_hung || errno == ENXIO || errno == EIO ||
#ifdef ENODEV
		errno == ENODEV ||
#endif
#ifdef __CYGWIN__
		errno == ENOENT ||
#endif
		errno == EINVAL || errno == ENOTTY || errno == EACCES) {
		no_dev_tty = TRUE;
#ifdef HAS_LTCHARS
		ltc = d_ltc;
#endif /* HAS_LTCHARS */
#ifdef TIOCLSET
		lmode = d_lmode;
#endif /* TIOCLSET */
#if defined(USE_ANY_SYSV_TERMIO) || defined(USE_POSIX_TERMIOS)
		tio = d_tio;
#else /* not USE_ANY_SYSV_TERMIO and not USE_POSIX_TERMIOS */
		sg = d_sg;
		tc = d_tc;
		discipline = d_disipline;
#ifdef sony
		jmode = d_jmode;
		jtc = d_jtc;
#endif /* sony */
#endif /* USE_ANY_SYSV_TERMIO or USE_POSIX_TERMIOS */
	    } else {
		SysError(ERROR_OPDEVTTY);
	    }
	} else {

	    /* Get a copy of the current terminal's state,
	     * if we can.  Some systems (e.g., SVR4 and MacII)
	     * may not have a controlling terminal at this point
	     * if started directly from xdm or xinit,
	     * in which case we just use the defaults as above.
	     */
#ifdef HAS_LTCHARS
	    if (ioctl(tty, TIOCGLTC, &ltc) == -1)
		ltc = d_ltc;
#endif /* HAS_LTCHARS */
#ifdef TIOCLSET
	    if (ioctl(tty, TIOCLGET, &lmode) == -1)
		lmode = d_lmode;
#endif /* TIOCLSET */
#ifdef USE_ANY_SYSV_TERMIO
	    if ((rc = ioctl(tty, TCGETA, &tio)) == -1)
		tio = d_tio;
#elif defined(USE_POSIX_TERMIOS)
	    if ((rc = tcgetattr(tty, &tio)) == -1)
		tio = d_tio;
#else /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */
	    if ((rc = ioctl(tty, TIOCGETP, (char *) &sg)) == -1)
		sg = d_sg;
	    if (ioctl(tty, TIOCGETC, (char *) &tc) == -1)
		tc = d_tc;
	    if (ioctl(tty, TIOCGETD, (char *) &discipline) == -1)
		discipline = d_disipline;
#ifdef sony
	    if (ioctl(tty, TIOCKGET, (char *) &jmode) == -1)
		jmode = d_jmode;
	    if (ioctl(tty, TIOCKGETC, (char *) &jtc) == -1)
		jtc = d_jtc;
#endif /* sony */
#endif /* USE_ANY_SYSV_TERMIO */

	    /*
	     * If ptyInitialErase is set, we want to get the pty's
	     * erase value.  Just in case that will fail, first get
	     * the value from /dev/tty, so we will have something
	     * at least.
	     */
#if OPT_INITIAL_ERASE
	    if (resource.ptyInitialErase) {
#ifdef USE_ANY_SYSV_TERMIO
		initial_erase = tio.c_cc[VERASE];
#elif defined(USE_POSIX_TERMIOS)
		initial_erase = tio.c_cc[VERASE];
#else /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */
		initial_erase = sg.sg_erase;
#endif /* USE_ANY_SYSV_TERMIO */
	    }
#endif

	    close(tty);
	    /* tty is no longer an open fd! */
	    tty = -1;
	}

#ifdef NEW_GET_PTY
	if (get_pty(&screen->respond, XDisplayString(screen->display))) {
	    SysError(ERROR_PTYS);
	}
#else
	if (get_pty(&screen->respond))) {
	    SysError(ERROR_PTYS);
	}
#endif
#if OPT_INITIAL_ERASE
	if (resource.ptyInitialErase) {
#ifdef USE_ANY_SYSV_TERMIO
	    struct termio my_tio;
	    if ((rc = ioctl(screen->respond, TCGETA, &my_tio)) == 0)
		initial_erase = my_tio.c_cc[VERASE];
#elif defined(USE_POSIX_TERMIOS)
	    struct termios my_tio;
	    if ((rc = tcgetattr(screen->respond, &my_tio)) == 0)
		initial_erase = my_tio.c_cc[VERASE];
#else /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */
	    struct sgttyb my_sg;
	    if ((rc = ioctl(screen->respond, TIOCGETP, (char *) &my_sg)) == 0)
		initial_erase = my_sg.sg_erase;
#endif /* USE_ANY_SYSV_TERMIO */
	}
#endif /* OPT_INITIAL_ERASE */
    }

    /* avoid double MapWindow requests
     */
    XtSetMappedWhenManaged( XtParent(term), False );

    wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				   False);

    VTInit();		/* realize now so know window size for tty driver */
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
    if (Console) {
	/*
	 * Inform any running xconsole program
	 * that we are going to steal the console.
	 */
	XmuGetHostname(mit_console_name + MIT_CONSOLE_LEN, 255);
	mit_console = XInternAtom(screen->display, mit_console_name, False);
	/* the user told us to be the console, so we can use CurrentTime */
	XtOwnSelection(XtParent(term),
		       mit_console, CurrentTime,
		       ConvertConsoleSelection, NULL, NULL);

    }
#endif

    if (screen->TekEmu) {
	envnew = tekterm;
	ptr = newtc;
    } else {
	envnew = vtterm;
	ptr = termcap;
    }

    /*
     * This used to exit if no termcap entry was found for the specified
     * terminal name.  That's a little unfriendly, so instead we'll allow
     * the program to proceed (but not to set $TERMCAP) if the termcap
     * entry is not found.
     */
    if (!get_termcap(TermName = resource.term_name, ptr, newtc)) {
	char *last = NULL;
	TermName = *envnew;
	while (*envnew != NULL) {
	    if ((last == NULL || strcmp(last, *envnew))
		&& get_termcap(*envnew, ptr, newtc)) {
		TermName = *envnew;
		break;
	    }
	    last = *envnew;
	    envnew++;
	}
    }

    /*
     * Check if ptyInitialErase is not set.  If so, we rely on the termcap
     * (or terminfo) to tell us what the erase mode should be set to.
     */
#if OPT_INITIAL_ERASE
    if (!resource.ptyInitialErase) {
	char temp[1024], *p = temp;
	char *s = tgetstr(TERMCAP_ERASE, &p);
	if (s != 0) {
	    initial_erase = decode_keyvalue(&s, True);
	}
    }

    if (resource.backarrow_is_erase) {	/* see input.c */
	if (initial_erase == 127) {
	    term->keyboard.flags &= ~MODE_DECBKM;
	} else {
	    term->keyboard.flags |= MODE_DECBKM;
	    term->keyboard.reset_DECBKM = 1;
	}
    } else {
	term->keyboard.reset_DECBKM = 2;
    }
#endif /* OPT_INITIAL_ERASE */

#ifdef TTYSIZE_STRUCT
    /* tell tty how big window is */

/*
    if (screen->TekEmu) {
	TTYSIZE_ROWS(ts) = 38;
	TTYSIZE_COLS(ts) = 81;
#if defined(USE_STRUCT_WINSIZE)
	ts.ws_xpixel = TFullWidth(screen);
	ts.ws_ypixel = TFullHeight(screen);
#endif
    } else
*/

    {
	TTYSIZE_ROWS(ts) = screen->max_row + 1;
	TTYSIZE_COLS(ts) = screen->max_col + 1;
#if defined(USE_STRUCT_WINSIZE)
	ts.ws_xpixel = FullWidth(screen);
	ts.ws_ypixel = FullHeight(screen);
#endif
    }
    i = SET_TTYSIZE(screen->respond, ts);
#endif /* TTYSIZE_STRUCT */

    added_utmp_entry = False;
#if defined(USE_UTEMPTER)
#undef UTMP
    if (!resource.utmpInhibit) {
	struct UTMP_STR dummy;
	char host[sizeof(dummy.ut_host) + 1], *endptr;

	(void) strncpy(host, DisplayString(screen->display), sizeof host);
	host[sizeof(host) - 1] = '\0';
	endptr = strrchr(host, ':');
	if (endptr) {
	    *endptr = '\0';
	}
	addToUtmp(ttydev, host, screen->respond);
	added_utmp_entry = True;
    }
#endif

    if (am_slave < 0) {
#if OPT_PTY_HANDSHAKE
	if (resource.ptyHandshake && (pipe(pc_pipe) || pipe(cp_pipe)))
	    SysError(ERROR_FORK);
#endif
	if ((screen->pid = fork()) == -1)
	    SysError(ERROR_FORK);

	if (screen->pid == 0) {
	    /*
	     * now in child process
	     */
#if defined(_POSIX_SOURCE) || defined(SVR4) || defined(__convex__) || defined(__SCO__) || defined(__QNX__)
		int pgrp = setsid();	/* variable may not be used... */
#else
		int pgrp = getpid();
#endif

#ifdef USE_USG_PTYS
#ifdef USE_ISPTS_FLAG
	    if (IsPts) {	/* SYSV386 supports both, which did we open? */
#endif
		int ptyfd = 0;
		char *pty_name = 0;

#ifdef __DARWIN__
 ; 
#else
		setpgrp();
#endif
		grantpt(screen->respond);
		unlockpt(screen->respond);
		if ((pty_name = ptsname(screen->respond)) == 0) {
		    SysError(ERROR_PTSNAME);
		}
		if ((ptyfd = open(pty_name, O_RDWR)) < 0) {
		    SysError(ERROR_OPPTSNAME);
		}
#ifdef I_PUSH
		if (ioctl(ptyfd, I_PUSH, "ptem") < 0) {
		    SysError(ERROR_PTEM);
		}
#if !defined(SVR4) && !(defined(SYSV) && defined(i386))
		if (!getenv("CONSEM") && ioctl(ptyfd, I_PUSH, "consem") < 0) {
		    SysError(ERROR_CONSEM);
		}
#endif /* !SVR4 */
		if (ioctl(ptyfd, I_PUSH, "ldterm") < 0) {
		    SysError(ERROR_LDTERM);
		}
#ifdef SVR4			/* from Sony */
		if (ioctl(ptyfd, I_PUSH, "ttcompat") < 0) {
		    SysError(ERROR_TTCOMPAT);
		}
#endif /* SVR4 */
#endif /* I_PUSH */
		tty = ptyfd;
		close(screen->respond);

#ifdef TTYSIZE_STRUCT
		/* tell tty how big window is */

		if (screen->TekEmu) {
		    TTYSIZE_ROWS(ts) = 24;
		    TTYSIZE_COLS(ts) = 80;
/*
#ifdef USE_STRUCT_WINSIZE
		    ts.ws_xpixel = TFullWidth(screen);
		    ts.ws_ypixel = TFullHeight(screen);
#endif
*/
		} else

		{
		    TTYSIZE_ROWS(ts) = screen->max_row + 1;
		    TTYSIZE_COLS(ts) = screen->max_col + 1;
/*
#ifdef USE_STRUCT_WINSIZE
		    ts.ws_xpixel = FullWidth(screen);
		    ts.ws_ypixel = FullHeight(screen);
#endif
*/
		}
#endif /* TTYSIZE_STRUCT */

#ifdef USE_ISPTS_FLAG
	    } else {		/* else pty, not pts */
#endif
#endif /* USE_USG_PTYS */

		(void) pgrp;	/* not all branches use this variable */

#if OPT_PTY_HANDSHAKE		/* warning, goes for a long ways */
		if (resource.ptyHandshake) {
		    /* close parent's sides of the pipes */
		    close(cp_pipe[0]);
		    close(pc_pipe[1]);

		    /* Make sure that our sides of the pipes are not in the
		     * 0, 1, 2 range so that we don't fight with stdin, out
		     * or err.
		     */
		    if (cp_pipe[1] <= 2) {
			if ((i = fcntl(cp_pipe[1], F_DUPFD, 3)) >= 0) {
			    (void) close(cp_pipe[1]);
			    cp_pipe[1] = i;
			}
		    }
		    if (pc_pipe[0] <= 2) {
			if ((i = fcntl(pc_pipe[0], F_DUPFD, 3)) >= 0) {
			    (void) close(pc_pipe[0]);
			    pc_pipe[0] = i;
			}
		    }

		    /* we don't need the socket, or the pty master anymore */
		    close(ConnectionNumber(screen->display));
		    close(screen->respond);

		    /* Now is the time to set up our process group and
		     * open up the pty slave.
		     */
#ifdef USE_SYSV_PGRP
#if defined(CRAY) && (OSMAJORVERSION > 5)
		    (void) setsid();
#else
		    (void) setpgrp();
#endif
#endif /* USE_SYSV_PGRP */

#if defined(__QNX__) && !defined(__QNXNTO__)
		    qsetlogin(getlogin(), ttydev);
#endif
		    while (1) {
#if defined(TIOCNOTTY) && (!defined(__GLIBC__) || (__GLIBC__ < 2) || ((__GLIBC__ == 2) && (__GLIBC_MINOR__ < 1)))
			if (!no_dev_tty
			    && (tty = open("/dev/tty", O_RDWR)) >= 0) {
			    ioctl(tty, TIOCNOTTY, (char *) NULL);
			    close(tty);
			}
#endif /* TIOCNOTTY && !glibc >= 2.1 */
#ifdef CSRG_BASED
			(void) revoke(ttydev);
#endif
			if ((tty = open(ttydev, O_RDWR)) >= 0) {
#if defined(CRAY) && defined(TCSETCTTY)
			    /* make /dev/tty work */
			    ioctl(tty, TCSETCTTY, 0);
#endif
#ifdef USE_SYSV_PGRP
			    /* We need to make sure that we are actually
			     * the process group leader for the pty.  If
			     * we are, then we should now be able to open
			     * /dev/tty.
			     */
			    if ((i = open("/dev/tty", O_RDWR)) >= 0) {
				/* success! */
				close(i);
				break;
			    }
#else /* USE_SYSV_PGRP */
			    break;
#endif /* USE_SYSV_PGRP */
			}
			perror("open ttydev");
#ifdef TIOCSCTTY
			ioctl(tty, TIOCSCTTY, 0);
#endif
			/* let our master know that the open failed */
			handshake.status = PTY_BAD;
			handshake.error = errno;
			strcpy(handshake.buffer, ttydev);
			write(cp_pipe[1], (char *) &handshake,
			      sizeof(handshake));

			/* get reply from parent */
			i = read(pc_pipe[0], (char *) &handshake,
				 sizeof(handshake));
			if (i <= 0) {
			    /* parent terminated */
			    exit(1);
			}

			if (handshake.status == PTY_NOMORE) {
			    /* No more ptys, let's shutdown. */
			    exit(1);
			}

			/* We have a new pty to try */
			free(ttydev);
			ttydev = (char *) malloc((unsigned)
						 (strlen(handshake.buffer) + 1));
			if (ttydev == NULL) {
			    SysError(ERROR_SPREALLOC);
			}
			strcpy(ttydev, handshake.buffer);
		    }

		    /* use the same tty name that everyone else will use
		     * (from ttyname)
		     */
		    if ((ptr = ttyname(tty)) != 0) {
			/* it may be bigger */
			ttydev = (char *) realloc(ttydev,
						  (unsigned) (strlen(ptr) + 1));
			if (ttydev == NULL) {
			    SysError(ERROR_SPREALLOC);
			}
			(void) strcpy(ttydev, ptr);
		    }
		}
#endif /* OPT_PTY_HANDSHAKE -- from near fork */

#ifdef USE_ISPTS_FLAG
	    }			/* end of IsPts else clause */
#endif

#ifdef USE_TTY_GROUP
	    {
		struct group *ttygrp;
		if ((ttygrp = getgrnam("tty")) != 0) {
		    /* change ownership of tty to real uid, "tty" gid */
		    set_owner(ttydev, screen->uid, ttygrp->gr_gid, 0620);
		} else {
		    /* change ownership of tty to real group and user id */
		    set_owner(ttydev, screen->uid, screen->gid, 0622);
		}
		endgrent();
	    }
#else /* else !USE_TTY_GROUP */
	    /* change ownership of tty to real group and user id */
	    set_owner(ttydev, screen->uid, screen->gid, 0622);
#endif /* USE_TTY_GROUP */

	    /*
	     * set up the tty modes
	     */
	    {
#if defined(USE_ANY_SYSV_TERMIO) || defined(USE_POSIX_TERMIOS)
#if defined(umips) || defined(CRAY) || defined(linux)
		/* If the control tty had its modes screwed around with,
		   eg. by lineedit in the shell, or emacs, etc. then tio
		   will have bad values.  Let's just get termio from the
		   new tty and tailor it.  */
		if (ioctl(tty, TCGETA, &tio) == -1)
		    SysError(ERROR_TIOCGETP);
		tio.c_lflag |= ECHOE;
#endif /* umips */
		/* Now is also the time to change the modes of the
		 * child pty.
		 */
		/* input: nl->nl, don't ignore cr, cr->nl */
		tio.c_iflag &= ~(INLCR | IGNCR);
		tio.c_iflag |= ICRNL;
		/* ouput: cr->cr, nl is not return, no delays, ln->cr/nl */
#ifndef USE_POSIX_TERMIOS
		tio.c_oflag &=
		    ~(OCRNL
		      | ONLRET
		      | NLDLY
		      | CRDLY
		      | TABDLY
		      | BSDLY
		      | VTDLY
		      | FFDLY);
#endif /* USE_POSIX_TERMIOS */
#ifdef ONLCR
		tio.c_oflag |= ONLCR;
#endif /* ONLCR */
#ifdef OPOST
		tio.c_oflag |= OPOST;
#endif /* OPOST */
#ifndef USE_POSIX_TERMIOS
# if defined(Lynx) && !defined(CBAUD)
#  define CBAUD V_CBAUD
# endif
		tio.c_cflag &= ~(CBAUD);
#ifdef BAUD_0
		/* baud rate is 0 (don't care) */
#elif defined(HAVE_TERMIO_C_ISPEED)
		tio.c_ispeed = tio.c_ospeed = VAL_LINE_SPEED;
#else /* !BAUD_0 */
		tio.c_cflag |= VAL_LINE_SPEED;
#endif /* !BAUD_0 */
#else /* USE_POSIX_TERMIOS */
		cfsetispeed(&tio, VAL_LINE_SPEED);
		cfsetospeed(&tio, VAL_LINE_SPEED);
#ifdef __MVS__
		/* turn off bits that can't be set from the slave side */
		tio.c_cflag &= ~(PACKET | PKT3270 | PTU3270 | PKTXTND);
#endif /* __MVS__ */
		/* Clear CLOCAL so that SIGHUP is sent to us
		   when the xterm ends */
		tio.c_cflag &= ~CLOCAL;
#endif /* USE_POSIX_TERMIOS */
		tio.c_cflag &= ~CSIZE;
		if (screen->input_eight_bits)
		    tio.c_cflag |= CS8;
		else
		    tio.c_cflag |= CS7;
		/* enable signals, canonical processing (erase, kill, etc),
		 * echo
		 */
		tio.c_lflag |= ISIG | ICANON | ECHO | ECHOE | ECHOK;
#ifdef ECHOKE
		tio.c_lflag |= ECHOKE | IEXTEN;
#endif
#ifdef ECHOCTL
		tio.c_lflag |= ECHOCTL | IEXTEN;
#endif
#ifndef __MVS__
		/* reset EOL to default value */
		tio.c_cc[VEOL] = CEOL;	/* '^@' */
		/* certain shells (ksh & csh) change EOF as well */
		tio.c_cc[VEOF] = CEOF;	/* '^D' */
#else
		if (tio.c_cc[VEOL] == 0)
		    tio.c_cc[VEOL] = CEOL;	/* '^@' */
		if (tio.c_cc[VEOF] == 0)
		    tio.c_cc[VEOF] = CEOF;	/* '^D' */
#endif
#ifdef VLNEXT
		tio.c_cc[VLNEXT] = CLNEXT;
#endif
#ifdef VWERASE
		tio.c_cc[VWERASE] = CWERASE;
#endif
#ifdef VREPRINT
		tio.c_cc[VREPRINT] = CRPRNT;
#endif
#ifdef VRPRNT
		tio.c_cc[VRPRNT] = CRPRNT;
#endif
#ifdef VDISCARD
		tio.c_cc[VDISCARD] = CFLUSH;
#endif
#ifdef VFLUSHO
		tio.c_cc[VFLUSHO] = CFLUSH;
#endif
#ifdef VSTOP
		tio.c_cc[VSTOP] = CSTOP;
#endif
#ifdef VSTART
		tio.c_cc[VSTART] = CSTART;
#endif
#ifdef VSUSP
		tio.c_cc[VSUSP] = CSUSP;
#endif
#ifdef VDSUSP
		tio.c_cc[VDSUSP] = CDSUSP;
#endif
		if (override_tty_modes) {
		    /* sysv-specific */
		    TMODE(XTTYMODE_intr, tio.c_cc[VINTR]);
		    TMODE(XTTYMODE_quit, tio.c_cc[VQUIT]);
		    TMODE(XTTYMODE_erase, tio.c_cc[VERASE]);
		    TMODE(XTTYMODE_kill, tio.c_cc[VKILL]);
		    TMODE(XTTYMODE_eof, tio.c_cc[VEOF]);
		    TMODE(XTTYMODE_eol, tio.c_cc[VEOL]);
#ifdef VSWTCH
		    TMODE(XTTYMODE_swtch, tio.c_cc[VSWTCH]);
#endif
#ifdef VSUSP
		    TMODE(XTTYMODE_susp, tio.c_cc[VSUSP]);
#endif
#ifdef VDSUSP
		    TMODE(XTTYMODE_dsusp, tio.c_cc[VDSUSP]);
#endif
#ifdef VREPRINT
		    TMODE(XTTYMODE_rprnt, tio.c_cc[VREPRINT]);
#endif
#ifdef VRPRNT
		    TMODE(XTTYMODE_rprnt, tio.c_cc[VRPRNT]);
#endif
#ifdef VDISCARD
		    TMODE(XTTYMODE_flush, tio.c_cc[VDISCARD]);
#endif
#ifdef VFLUSHO
		    TMODE(XTTYMODE_flush, tio.c_cc[VFLUSHO]);
#endif
#ifdef VWERASE
		    TMODE(XTTYMODE_weras, tio.c_cc[VWERASE]);
#endif
#ifdef VLNEXT
		    TMODE(XTTYMODE_lnext, tio.c_cc[VLNEXT]);
#endif
#ifdef VSTART
		    TMODE(XTTYMODE_start, tio.c_cc[VSTART]);
#endif
#ifdef VSTOP
		    TMODE(XTTYMODE_stop, tio.c_cc[VSTOP]);
#endif
#ifdef VSTATUS
		    TMODE(XTTYMODE_status, tio.c_cc[VSTATUS]);
#endif
#ifdef VERASE2
		    TMODE(XTTYMODE_erase2, tio.c_cc[VERASE2]);
#endif
#ifdef VEOL2
		    TMODE(XTTYMODE_eol2, tio.c_cc[VEOL2]);
#endif
#ifdef HAS_LTCHARS
		    /* both SYSV and BSD have ltchars */
		    TMODE(XTTYMODE_susp, ltc.t_suspc);
		    TMODE(XTTYMODE_dsusp, ltc.t_dsuspc);
		    TMODE(XTTYMODE_rprnt, ltc.t_rprntc);
		    TMODE(XTTYMODE_flush, ltc.t_flushc);
		    TMODE(XTTYMODE_weras, ltc.t_werasc);
		    TMODE(XTTYMODE_lnext, ltc.t_lnextc);
#endif
		}
#ifdef HAS_LTCHARS
#ifdef __hpux
		/* ioctl chokes when the "reserved" process group controls
		 * are not set to _POSIX_VDISABLE */
		ltc.t_rprntc = ltc.t_rprntc = ltc.t_flushc =
		    ltc.t_werasc = ltc.t_lnextc = _POSIX_VDISABLE;
#endif /* __hpux */
		if (ioctl(tty, TIOCSLTC, &ltc) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETC);
#endif /* HAS_LTCHARS */
#ifdef TIOCLSET
		if (ioctl(tty, TIOCLSET, (char *) &lmode) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCLSET);
#endif /* TIOCLSET */
#ifndef USE_POSIX_TERMIOS
		if (ioctl(tty, TCSETA, &tio) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETP);
#else /* USE_POSIX_TERMIOS */
		if (tcsetattr(tty, TCSANOW, &tio) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETP);
#endif /* USE_POSIX_TERMIOS */
#else /* USE_ANY_SYSV_TERMIO or USE_POSIX_TERMIOS */
		sg.sg_flags &= ~(ALLDELAY | XTABS | CBREAK | RAW);
		sg.sg_flags |= (ECHO | CRMOD);
		/* make sure speed is set on pty so that editors work right */
/*
		sg.sg_ispeed = VAL_LINE_SPEED;
		sg.sg_ospeed = VAL_LINE_SPEED;
*/
		/* reset t_brkc to default value */
		tc.t_brkc = -1;
#ifdef LPASS8
		if (screen->input_eight_bits)
		    lmode |= LPASS8;
		else
		    lmode &= ~(LPASS8);
#endif
#ifdef sony
		jmode &= ~KM_KANJI;
#endif /* sony */

		ltc = d_ltc;

		if (override_tty_modes) {
		    TMODE(XTTYMODE_intr, tc.t_intrc);
		    TMODE(XTTYMODE_quit, tc.t_quitc);
		    TMODE(XTTYMODE_erase, sg.sg_erase);
		    TMODE(XTTYMODE_kill, sg.sg_kill);
		    TMODE(XTTYMODE_eof, tc.t_eofc);
		    TMODE(XTTYMODE_start, tc.t_startc);
		    TMODE(XTTYMODE_stop, tc.t_stopc);
		    TMODE(XTTYMODE_brk, tc.t_brkc);
		    /* both SYSV and BSD have ltchars */
		    TMODE(XTTYMODE_susp, ltc.t_suspc);
		    TMODE(XTTYMODE_dsusp, ltc.t_dsuspc);
		    TMODE(XTTYMODE_rprnt, ltc.t_rprntc);
		    TMODE(XTTYMODE_flush, ltc.t_flushc);
		    TMODE(XTTYMODE_weras, ltc.t_werasc);
		    TMODE(XTTYMODE_lnext, ltc.t_lnextc);
		}

		if (ioctl(tty, TIOCSETP, (char *) &sg) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETP);
		if (ioctl(tty, TIOCSETC, (char *) &tc) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETC);
		if (ioctl(tty, TIOCSETD, (char *) &discipline) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSETD);
		if (ioctl(tty, TIOCSLTC, (char *) &ltc) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCSLTC);
		if (ioctl(tty, TIOCLSET, (char *) &lmode) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCLSET);
#ifdef sony
		if (ioctl(tty, TIOCKSET, (char *) &jmode) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCKSET);
		if (ioctl(tty, TIOCKSETC, (char *) &jtc) == -1)
		    HsSysError(cp_pipe[1], ERROR_TIOCKSETC);
#endif /* sony */
#endif /* !USE_ANY_SYSV_TERMIO */
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
		if (Console) {
#ifdef TIOCCONS
		    int on = 1;
		    if (ioctl(tty, TIOCCONS, (char *) &on) == -1)
			fprintf(stderr, "%s: cannot open console: %s\n",
				"xgterm", strerror(errno));
#endif
#ifdef SRIOCSREDIR
		    int fd = open("/dev/console", O_RDWR);
		    if (fd == -1 || ioctl(fd, SRIOCSREDIR, tty) == -1)
			fprintf(stderr, "%s: cannot open console: %s\n",
				"xgterm", strerror(errno));
		    (void) close(fd);
#endif
		}
#endif /* TIOCCONS */
	    }

	    signal(SIGCHLD, SIG_DFL);
#ifdef USE_SYSV_SIGHUP
	    /* watch out for extra shells (I don't understand either) */
	    signal(SIGHUP, SIG_DFL);
#else
	    signal(SIGHUP, SIG_IGN);
#endif
	    /* restore various signals to their defaults */
	    signal(SIGINT, SIG_DFL);
	    signal(SIGQUIT, SIG_DFL);
	    signal(SIGTERM, SIG_DFL);

	    /*
	     * If we're not asked to make the parent process set the
	     * terminal's erase mode, and if we had no ttyModes resource,
	     * then set the terminal's erase mode from our best guess.
	     */
#if OPT_INITIAL_ERASE
	    if (!resource.ptyInitialErase
		&& !override_tty_modes
		&& !ttymodelist[XTTYMODE_erase].set) {
#ifdef USE_ANY_SYSV_TERMIO
		if (ioctl(tty, TCGETA, &tio) == -1)
		    tio = d_tio;
		tio.c_cc[VERASE] = initial_erase;
		rc = ioctl(tty, TCSETA, &tio);
#elif defined(USE_POSIX_TERMIOS)
		if (tcgetattr(tty, &tio) == -1)
		    tio = d_tio;
		tio.c_cc[VERASE] = initial_erase;
		rc = tcsetattr(tty, TCSANOW, &tio);
#else /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */
		if (ioctl(tty, TIOCGETP, (char *) &sg) == -1)
		    sg = d_sg;
		sg.sg_erase = initial_erase;
		rc = ioctl(tty, TIOCSETP, (char *) &sg);
#endif /* USE_ANY_SYSV_TERMIO */
	    }
#endif

	    /* copy the environment before Setenving */
	    for (i = 0; environ[i] != NULL; i++) ;
	    /* compute number of xtermSetenv() calls below */
	    envsize = 1;	/* (NULL terminating entry) */
	    envsize += 3;	/* TERM, WINDOWID, DISPLAY */
#ifdef HAVE_UTMP
	    envsize += 1;	/* LOGNAME */
#endif /* HAVE_UTMP */
#ifdef USE_SYSV_ENVVARS
	    envsize += 2;	/* COLUMNS, LINES */
#ifdef HAVE_UTMP
	    envsize += 2;	/* HOME, SHELL */
#endif /* HAVE_UTMP */
#ifdef OWN_TERMINFO_DIR
	    envsize += 1;	/* TERMINFO */
#endif
#else /* USE_SYSV_ENVVARS */
	    envsize += 1;	/* TERMCAP */
#endif /* USE_SYSV_ENVVARS */
	    envnew = (char **) calloc((unsigned) i + envsize, sizeof(char *));
	    memmove((char *) envnew, (char *) environ, i * sizeof(char *));
	    environ = envnew;
	    xtermSetenv("TERM=", TermName);
	    if (!TermName)
		*newtc = 0;

	    sprintf(buf, "%lu",
		    ((unsigned long) XtWindow(XtParent(term))));
	    xtermSetenv("WINDOWID=", buf);

	    /* put the display into the environment of the shell */
	    xtermSetenv("DISPLAY=", XDisplayString(screen->display));

	    signal(SIGTERM, SIG_DFL);

	    /* this is the time to go and set up stdin, out, and err
	     */
	    {
#if defined(CRAY) && (OSMAJORVERSION >= 6)
		(void) close(tty);
		(void) close(0);

		if (open("/dev/tty", O_RDWR)) {
		    SysError(ERROR_OPDEVTTY);
		}
		(void) close(1);
		(void) close(2);
		dup(0);
		dup(0);
#else
		/* dup the tty */
		for (i = 0; i <= 2; i++)
		    if (i != tty) {
			(void) close(i);
			(void) dup(tty);
		    }
#ifndef ATT
		/* and close the tty */
		if (tty > 2)
		    (void) close(tty);
#endif
#endif /* CRAY */
	    }

#if !defined(USE_SYSV_PGRP)
#ifdef TIOCSCTTY
	    setsid();
	    ioctl(0, TIOCSCTTY, 0);
#endif
	    ioctl(0, TIOCSPGRP, (char *) &pgrp);
	    setpgrp(0, 0);
	    close(open(ttydev, O_WRONLY));
	    setpgrp(0, pgrp);
#if defined(__QNX__)
	    tcsetpgrp(0, pgrp /*setsid() */ );
#endif
#endif /* !USE_SYSV_PGRP */

#ifdef Lynx
	    {
		struct termio t;
		if (ioctl(0, TCGETA, &t) >= 0) {
		    /* this gets lost somewhere on our way... */
		    t.c_oflag |= OPOST;
		    ioctl(0, TCSETA, &t);
		}
	    }
#endif

#ifdef HAVE_UTMP
	    pw = getpwuid(screen->uid);
	    login_name = NULL;
	    if (pw && pw->pw_name) {
#ifdef HAVE_GETLOGIN
		/*
		 * If the value from getlogin() differs from the value we
		 * get by looking in the password file, check if it does
		 * correspond to the same uid.  If so, allow that as an
		 * alias for the uid.
		 *
		 * Of course getlogin() will fail if we're started from
		 * a window-manager, since there's no controlling terminal
		 * to fuss with.  In that case, try to get something useful
		 * from the user's $LOGNAME or $USER environment variables.
		 */
		if (((login_name = getlogin()) != NULL
		     || (login_name = getenv("LOGNAME")) != NULL
		     || (login_name = getenv("USER")) != NULL)
		    && strcmp(login_name, pw->pw_name)) {
		    struct passwd *pw2 = getpwnam(login_name);
		    if (pw2 != 0
			&& pw->pw_uid != pw2->pw_uid) {
			login_name = NULL;
		    }
		}
#endif
		if (login_name == NULL)
		    login_name = pw->pw_name;
		if (login_name != NULL)
		    login_name = x_strdup(login_name);
	    }
	    if (login_name != NULL) {
		xtermSetenv("LOGNAME=", login_name);	/* for POSIX */
	    }
#ifndef USE_UTEMPTER
#ifdef USE_SYSV_UTMP
	    /* Set up our utmp entry now.  We need to do it here
	     * for the following reasons:
	     *   - It needs to have our correct process id (for
	     *     login).
	     *   - If our parent was to set it after the fork(),
	     *     it might make it out before we need it.
	     *   - We need to do it before we go and change our
	     *     user and group id's.
	     */
	    (void) setutent();
	    /* set up entry to search for */
	    bzero((char *) &utmp, sizeof(utmp));
	    (void) strncpy(utmp.ut_id, my_utmp_id(ttydev), sizeof(utmp.ut_id));

	    utmp.ut_type = DEAD_PROCESS;

	    /* position to entry in utmp file */
	    /* Test return value: beware of entries left behind: PSz 9 Mar 00 */
	    if (!(utret = getutid(&utmp))) {
		(void) setutent();
		utmp.ut_type = USER_PROCESS;
		if (!(utret = getutid(&utmp))) {
		    (void) setutent();
		}
	    }

	    /* set up the new entry */
	    utmp.ut_type = USER_PROCESS;
#ifdef HAVE_UTMP_UT_XSTATUS
	    utmp.ut_xstatus = 2;
#endif
	    (void) strncpy(utmp.ut_user,
			   (login_name != NULL) ? login_name : "????",
			   sizeof(utmp.ut_user));
	    /* why are we copying this string again?  (see above) */
	    (void) strncpy(utmp.ut_id, my_utmp_id(ttydev), sizeof(utmp.ut_id));
	    (void) strncpy(utmp.ut_line,
			   my_pty_name(ttydev), sizeof(utmp.ut_line));

#ifdef HAVE_UTMP_UT_HOST
	    (void) strncpy(buf, DisplayString(screen->display), sizeof(buf));
#ifndef linux
	    {
		char *disfin = strrchr(buf, ':');
		if (disfin)
		    *disfin = '\0';
	    }
#endif
	    (void) strncpy(utmp.ut_host, buf, sizeof(utmp.ut_host));
#endif
	    (void) strncpy(utmp.ut_name,
			   (login_name) ? login_name : "????",
			   sizeof(utmp.ut_name));

	    utmp.ut_pid = getpid();
#if defined(HAVE_UTMP_UT_XTIME)
#if defined(HAVE_UTMP_UT_SESSION)
	    utmp.ut_session = getsid(0);
#endif
	    utmp.ut_xtime = time((time_t *) 0);
	    utmp.ut_tv.tv_usec = 0;
#else
	    utmp.ut_time = time((time_t *) 0);
#endif

	    /* write out the entry */
	    if (!resource.utmpInhibit) {
		errno = 0;
		pututline(&utmp);
	    }
#ifdef WTMP
#if defined(WTMPX_FILE) && (defined(SVR4) || defined(__SCO__))
	    if (term->misc.login_shell)
		updwtmpx(WTMPX_FILE, &utmp);
#elif defined(linux) && defined(__GLIBC__) && (__GLIBC__ >= 2) && !(defined(__powerpc__) && (__GLIBC__ == 2) && (__GLIBC_MINOR__ == 0))
	    if (term->misc.login_shell)
		updwtmp(etc_wtmp, &utmp);
#else
	    if (term->misc.login_shell &&
		(i = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
		write(i, (char *) &utmp, sizeof(utmp));
		close(i);
	    }
#endif
#endif
	    /* close the file */
	    (void) endutent();

#else /* USE_SYSV_UTMP */
	    /* We can now get our ttyslot!  We can also set the initial
	     * utmp entry.
	     */
	    tslot = ttyslot();
	    added_utmp_entry = False;
	    {
		if (tslot > 0 && pw && !resource.utmpInhibit &&
		    (i = open(etc_utmp, O_WRONLY)) >= 0) {
		    bzero((char *) &utmp, sizeof(utmp));
		    (void) strncpy(utmp.ut_line,
				   my_pty_name(ttydev),
				   sizeof(utmp.ut_line));
		    (void) strncpy(utmp.ut_name, login_name,
				   sizeof(utmp.ut_name));
#ifdef HAVE_UTMP_UT_HOST
		    (void) strncpy(utmp.ut_host,
				   XDisplayString(screen->display),
				   sizeof(utmp.ut_host));
#endif
		    /* cast needed on Ultrix 4.4 */
		    time((time_t *) & utmp.ut_time);
		    lseek(i, (long) (tslot * sizeof(utmp)), 0);
		    write(i, (char *) &utmp, sizeof(utmp));
		    close(i);
		    added_utmp_entry = True;
#if defined(WTMP)
		    if (term->misc.login_shell &&
			(i = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
			int status;
			status = write(i, (char *) &utmp, sizeof(utmp));
			status = close(i);
		    }
#elif defined(MNX_LASTLOG)
		    if (term->misc.login_shell &&
			(i = open(_U_LASTLOG, O_WRONLY)) >= 0) {
			lseek(i, (long) (screen->uid *
					 sizeof(utmp)), 0);
			write(i, (char *) &utmp, sizeof(utmp));
			close(i);
		    }
#endif /* WTMP or MNX_LASTLOG */
		} else
		    tslot = -tslot;
	    }

	    /* Let's pass our ttyslot to our parent so that it can
	     * clean up after us.
	     */
#if OPT_PTY_HANDSHAKE
	    if (resource.ptyHandshake) {
		handshake.tty_slot = tslot;
	    }
#endif /* OPT_PTY_HANDSHAKE */
#endif /* USE_SYSV_UTMP */

#ifdef USE_LASTLOGX
	    if (term->misc.login_shell) {
		bzero((char *) &lastlog, sizeof(lastlog));
		(void) strncpy(lastlog.ll_line,
			       my_pty_name(ttydev),
			       sizeof(lastlog.ll_line));
		X_GETTIMEOFDAY(&lastlog.ll_tv);
		(void) strncpy(lastlog.ll_host,
			       XDisplayString(screen->display),
			       sizeof(lastlog.ll_host));
		updlastlogx(_PATH_LASTLOGX, screen->uid, &lastlog);
	    }
#endif

#ifdef USE_LASTLOG
	    if (term->misc.login_shell &&
		(i = open(etc_lastlog, O_WRONLY)) >= 0) {
		bzero((char *) &lastlog, sizeof(struct lastlog));
		(void) strncpy(lastlog.ll_line,
			       my_pty_name(ttydev),
			       sizeof(lastlog.ll_line));
		(void) strncpy(lastlog.ll_host,
			       XDisplayString(screen->display),
			       sizeof(lastlog.ll_host));
		time(&lastlog.ll_time);
		lseek(i, (long) (screen->uid * sizeof(struct lastlog)), 0);
		write(i, (char *) &lastlog, sizeof(struct lastlog));
		close(i);
	    }
#endif /* USE_LASTLOG */

#ifdef __OpenBSD__
	    /* Switch to real gid after writing utmp entry */
	    utmpGid = getegid();
	    if (getgid() != getegid()) {
		utmpGid = getegid();
		setegid(getgid());
	    }
#endif

#if OPT_PTY_HANDSHAKE
	    /* Let our parent know that we set up our utmp entry
	     * so that it can clean up after us.
	     */
	    if (resource.ptyHandshake) {
		handshake.status = UTMP_ADDED;
		handshake.error = 0;
		strcpy(handshake.buffer, ttydev);
		(void) write(cp_pipe[1], (char *) &handshake, sizeof(handshake));
	    }
#endif /* OPT_PTY_HANDSHAKE */
#endif /* USE_UTEMPTER */
#endif /* HAVE_UTMP */

	    (void) setgid(screen->gid);
#ifdef HAS_BSD_GROUPS
	    if (geteuid() == 0 && pw) {
		if (initgroups(login_name, pw->pw_gid)) {
		    perror("initgroups failed");
		    SysError(ERROR_INIGROUPS);
		}
	    }
#endif
	    if (setuid(screen->uid)) {
		SysError(ERROR_SETUID);
	    }
#if OPT_PTY_HANDSHAKE
	    if (resource.ptyHandshake) {
		/* mark the pipes as close on exec */
		fcntl(cp_pipe[1], F_SETFD, 1);
		fcntl(pc_pipe[0], F_SETFD, 1);

		/* We are at the point where we are going to
		 * exec our shell (or whatever).  Let our parent
		 * know we arrived safely.
		 */
		handshake.status = PTY_GOOD;
		handshake.error = 0;
		(void) strcpy(handshake.buffer, ttydev);
		(void) write(cp_pipe[1], (char *) &handshake, sizeof(handshake));

		if (waiting_for_initial_map) {
		    i = read(pc_pipe[0], (char *) &handshake,
			     sizeof(handshake));
		    if (i != sizeof(handshake) ||
			handshake.status != PTY_EXEC) {
			/* some very bad problem occurred */
			exit(ERROR_PTY_EXEC);
		    }
		    if (handshake.rows > 0 && handshake.cols > 0) {
			screen->max_row = handshake.rows;
			screen->max_col = handshake.cols;
#ifdef TTYSIZE_STRUCT
			TTYSIZE_ROWS(ts) = screen->max_row + 1;
			TTYSIZE_COLS(ts) = screen->max_col + 1;
#if defined(USE_STRUCT_WINSIZE)
			ts.ws_xpixel = FullWidth(screen);
			ts.ws_ypixel = FullHeight(screen);
#endif
#endif /* TTYSIZE_STRUCT */
		    }
		}
	    }
#endif /* OPT_PTY_HANDSHAKE */

#ifdef USE_SYSV_ENVVARS
	    {
		char numbuf[12];
		sprintf(numbuf, "%d", screen->max_col + 1);
		xtermSetenv("COLUMNS=", numbuf);
		sprintf(numbuf, "%d", screen->max_row + 1);
		xtermSetenv("LINES=", numbuf);
	    }
#ifdef HAVE_UTMP
	    if (pw) {		/* SVR4 doesn't provide these */
		if (!getenv("HOME"))
		    xtermSetenv("HOME=", pw->pw_dir);
		if (!getenv("SHELL"))
		    xtermSetenv("SHELL=", pw->pw_shell);
	    }
#endif /* HAVE_UTMP */
#ifdef OWN_TERMINFO_DIR
	    xtermSetenv("TERMINFO=", OWN_TERMINFO_DIR);
#endif
#else /* USE_SYSV_ENVVARS */
	    if (!screen->TekEmu && *newtc) {
		strcpy(termcap, newtc);
		resize(screen, termcap, newtc);
	    }
/*
	    if (term->misc.titeInhibit && !term->misc.tiXtraScroll) {
*/
	    if (term->misc.titeInhibit) {
		remove_termcap_entry(newtc, "ti=");
		remove_termcap_entry(newtc, "te=");
	    }
	    /*
	     * work around broken termcap entries */
	    if (resource.useInsertMode) {
		remove_termcap_entry(newtc, "ic=");
		/* don't get duplicates */
		remove_termcap_entry(newtc, "im=");
		remove_termcap_entry(newtc, "ei=");
		remove_termcap_entry(newtc, "mi");
		if (*newtc)
		    strcat(newtc, ":im=\\E[4h:ei=\\E[4l:mi:");
	    }
	    if (*newtc) {
#if OPT_INITIAL_ERASE
		unsigned len;
		remove_termcap_entry(newtc, TERMCAP_ERASE "=");
		len = strlen(newtc);
		if (len != 0 && newtc[len - 1] == ':')
		    len--;
		sprintf(newtc + len, ":%s=\\%03o:",
			TERMCAP_ERASE,
			initial_erase & 0377);
#endif
		xtermSetenv("TERMCAP=", newtc);
	    }
#endif /* USE_SYSV_ENVVARS */

	    /* need to reset after all the ioctl bashing we did above */
#if OPT_PTY_HANDSHAKE
	    if (resource.ptyHandshake) {
#ifdef TTYSIZE_STRUCT
		i = SET_TTYSIZE(0, ts);
#endif /* TTYSIZE_STRUCT */
	    }
#endif /* OPT_PTY_HANDSHAKE */
	    signal(SIGHUP, SIG_DFL);

#ifdef HAVE_UTMP
	    if (((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		((pw == NULL && (pw = getpwuid(screen->uid)) == NULL) ||
		 *(ptr = pw->pw_shell) == 0))
#else /* HAVE_UTMP */
	    if (((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		((pw = getpwuid(screen->uid)) == NULL ||
		 *(ptr = pw->pw_shell) == 0))
#endif /* HAVE_UTMP */
		ptr = "/bin/sh";
	    shname = x_basename(ptr);

#if OPT_LUIT_PROG
	    /*
	     * Use two copies of command_to_exec, in case luit is not actually
	     * there, or refuses to run.  In that case we will fall-through to
	     * to command that the user gave anyway.
	     */
	    if (command_to_exec_with_luit) {
		execvp(*command_to_exec_with_luit, command_to_exec_with_luit);
		/* print error message on screen */
		fprintf(stderr, "%s: Can't execvp %s: %s\n",
			"xgterm", *command_to_exec_with_luit, strerror(errno));
		fprintf(stderr, "%s: cannot support your locale.\n",
			"xgterm");
	    }
#endif
	    if (command_to_exec) {
		execvp(*command_to_exec, command_to_exec);
		if (command_to_exec[1] == 0)
		    execlp(ptr, shname, "-c", command_to_exec[0], (void *) 0);
		/* print error message on screen */
		fprintf(stderr, "%s: Can't execvp %s: %s\n",
			"xgterm", *command_to_exec, strerror(errno));
	    }
#ifdef USE_SYSV_SIGHUP
	    /* fix pts sh hanging around
	    signal(SIGHUP, SIG_DFL);
	     */
#endif

	    shname_minus = (char *) malloc(strlen(shname) + 2);
	    (void) strcpy(shname_minus, "-");
	    (void) strcat(shname_minus, shname);
#if !defined(USE_ANY_SYSV_TERMIO) && !defined(USE_POSIX_TERMIOS)
	    ldisc = XStrCmp("csh", shname + strlen(shname) - 3) == 0 ?
		NTTYDISC : 0;
	    ioctl(0, TIOCSETD, (char *) &ldisc);
#endif /* !USE_ANY_SYSV_TERMIO && !USE_POSIX_TERMIOS */

#ifdef USE_LOGIN_DASH_P
	    if (term->misc.login_shell && pw && added_utmp_entry)
		execl(bin_login, "login", "-p", "-f", login_name, (void *) 0);
#endif
	    execlp(ptr,
		   (term->misc.login_shell ? shname_minus : shname),
		   (void *) 0);

	    /* Exec failed. */
	    fprintf(stderr, "%s: Could not exec %s: %s\n", "xgterm",
		    ptr, strerror(errno));
	    (void) sleep(5);
	    exit(ERROR_EXEC);
	}
	/* end if in child after fork */
#if OPT_PTY_HANDSHAKE
	if (resource.ptyHandshake) {
	    /* Parent process.  Let's handle handshaked requests to our
	     * child process.
	     */

	    /* close childs's sides of the pipes */
	    close(cp_pipe[1]);
	    close(pc_pipe[0]);

	    for (done = 0; !done;) {
		if (read(cp_pipe[0],
			 (char *) &handshake,
			 sizeof(handshake)) <= 0) {
		    /* Our child is done talking to us.  If it terminated
		     * due to an error, we will catch the death of child
		     * and clean up.
		     */
		    break;
		}

		switch (handshake.status) {
		case PTY_GOOD:
		    /* Success!  Let's free up resources and
		     * continue.
		     */
		    done = 1;
		    break;

		case PTY_BAD:
		    /* The open of the pty failed!  Let's get
		     * another one.
		     */
		    (void) close(screen->respond);
#ifdef NEW_GET_PTY
		    if (get_pty(&screen->respond, XDisplayString(screen->display))) {
#else
		    if (get_pty(&screen->respond)) {
#endif
			/* no more ptys! */
			fprintf(stderr,
				"%s: child process can find no available ptys: %s\n",
				"xgterm", strerror(errno));
			handshake.status = PTY_NOMORE;
			write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
			exit(ERROR_PTYS);
		    }
		    handshake.status = PTY_NEW;
		    (void) strcpy(handshake.buffer, ttydev);
		    write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
		    break;

		case PTY_FATALERROR:
		    errno = handshake.error;
		    close(cp_pipe[0]);
		    close(pc_pipe[1]);
		    SysError(handshake.fatal_error);
		    /*NOTREACHED */

		case UTMP_ADDED:
		    /* The utmp entry was set by our slave.  Remember
		     * this so that we can reset it later.
		     */
		    added_utmp_entry = True;
#ifndef	USE_SYSV_UTMP
		    tslot = handshake.tty_slot;
#endif /* USE_SYSV_UTMP */
		    free(ttydev);
		    ttydev = x_strdup(handshake.buffer);
		    break;
		default:
		    fprintf(stderr, "%s: unexpected handshake status %d\n",
			    "xgterm", handshake.status);
		}
	    }
	    /* close our sides of the pipes */
	    if (!waiting_for_initial_map) {
		close(cp_pipe[0]);
		close(pc_pipe[1]);
	    }
	}
#endif /* OPT_PTY_HANDSHAKE */
    }

    /* end if no slave */
    /*
     * still in parent (xterm process)
     */
#ifdef USE_SYSV_SIGHUP
    /* hung sh problem?  (Fedora)
    signal(SIGHUP, SIG_DFL);
	*/
    signal(SIGHUP, SIG_IGN);
#else
    signal(SIGHUP, SIG_IGN);
#endif

/*
 * Unfortunately, System V seems to have trouble divorcing the child process
 * from the process group of xterm.  This is a problem because hitting the
 * INTR or QUIT characters on the keyboard will cause xterm to go away if we
 * don't ignore the signals.  This is annoying.
 */

#if defined(USE_SYSV_SIGNALS) && !defined(SIGTSTP)
    signal(SIGINT, SIG_IGN);

#ifndef SYSV
    /* hung shell problem */
    signal(SIGQUIT, SIG_IGN);
#endif
    signal(SIGTERM, SIG_IGN);
#elif defined(SYSV) || defined(__osf__)
    /* if we were spawned by a jobcontrol smart shell (like ksh or csh),
     * then our pgrp and pid will be the same.  If we were spawned by
     * a jobcontrol dumb shell (like /bin/sh), then we will be in our
     * parent's pgrp, and we must ignore keyboard signals, or we will
     * tank on everything.
     */
    if (getpid() == getpgrp()) {
	(void) signal(SIGINT, Exit);
	(void) signal(SIGQUIT, Exit);
	(void) signal(SIGTERM, Exit);
    } else {
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTERM, SIG_IGN);
    }
    (void) signal(SIGPIPE, Exit);
#else /* SYSV */
    signal(SIGINT, Exit);
    signal(SIGQUIT, Exit);
    signal(SIGTERM, Exit);
    signal(SIGPIPE, Exit);
#endif /* USE_SYSV_SIGNALS and not SIGTSTP */

    return 0;
}				/* end spawn */

#else


spawn ()
/* 
 *  Inits pty and tty and forks a login process.
 *  Does not close fd Xsocket.
 *  If slave, the pty named in passedPty is already open for use
 */
{
	extern char *SysErrorMsg();
	register TScreen *screen = &term->screen;
	int Xsocket = ConnectionNumber(screen->display);
#ifdef USE_HANDSHAKE
	handshake_t handshake;
#else
	int fds[2];
#endif
	int tty = -1;
	int discipline;
	int done;
#ifdef USE_SYSV_TERMIO
	struct termio tio;
	struct termio dummy_tio;
#ifdef TIOCLSET
	unsigned lmode;
#endif	/* TIOCLSET */
#ifdef TIOCSLTC
	struct ltchars ltc;
#endif	/* TIOCSLTC */
	int one = 1;
	int zero = 0;
	int status;
#else	/* else not USE_SYSV_TERMIO */
	unsigned lmode;
	struct tchars tc;
	struct ltchars ltc;
	struct sgttyb sg;
#ifdef sony
	int jmode;
	struct jtchars jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */

	char termcap [1024];
	char newtc [1024];
	char *ptr, *shname, *shname_minus;
	int i, no_dev_tty = FALSE;
#ifdef USE_SYSV_TERMIO
	char *dev_tty_name = (char *) 0;
	int fd;			/* for /etc/wtmp */
#endif	/* USE_SYSV_TERMIO */
	char **envnew;		/* new environment */
	int envsize;		/* elements in new environment */
	char buf[64];
	char *TermName = NULL;
	int ldisc = 0;
#if defined(sun) && !defined(SVR4)
#ifdef TIOCSSIZE
	struct ttysize ts;
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif	/* TIOCSWINSZ */
#endif	/* sun */
	struct passwd *pw = NULL;
#ifdef UTMP
#ifdef SVR4
	struct utmpx utmp;
#else
	struct utmp utmp;
#endif
#ifdef LASTLOG
	struct lastlog lastlog;
#endif	/* LASTLOG */
#endif	/* UTMP */

	screen->uid = getuid();
	screen->gid = getgid();

#ifdef linux
	memset(termcap, 0, sizeof(termcap));
	memset(newtc, 0, sizeof(newtc));
#endif

#ifdef SIGTTOU
	/* so that TIOCSWINSZ || TIOCSIZE doesn't block */
	signal(SIGTTOU,SIG_IGN);
#endif

	if (am_slave) {
		screen->respond = am_slave;
		ptydev[strlen(ptydev) - 2] = ttydev[strlen(ttydev) - 2] =
			passedPty[0];
		ptydev[strlen(ptydev) - 1] = ttydev[strlen(ttydev) - 1] =
			passedPty[1];

		setgid (screen->gid);
		setuid (screen->uid);
	} else {
		Bool tty_got_hung = False;

 		/*
 		 * Sometimes /dev/tty hangs on open (as in the case of a pty
 		 * that has gone away).  Simply make up some reasonable
 		 * defaults.
 		 */
 		signal(SIGALRM, hungtty);
 		alarm(2);		/* alarm(1) might return too soon */
 		if (! setjmp(env)) {
 			tty = open ("/dev/tty", O_RDWR, 0);
 			alarm(0);
 		} else {
			tty_got_hung = True;
 			tty = -1;
 			errno = ENXIO;
 		}
 		signal(SIGALRM, SIG_DFL);
 
		/*
		 * Check results and ignore current control terminal if
		 * necessary.  ENXIO is what is normally returned if there is
		 * no controlling terminal, but some systems (e.g. SunOS 4.0)
		 * seem to return EIO.
		 */
 		if (tty < 0) {
			if (tty_got_hung || errno == ENXIO || errno == EIO ||
			    errno == ENOTTY) {
				no_dev_tty = TRUE;
#ifdef TIOCSLTC
				ltc = d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
				lmode = d_lmode;
#endif	/* TIOCLSET */
#ifdef USE_SYSV_TERMIO
				tio = d_tio;
#else	/* not USE_SYSV_TERMIO */
				sg = d_sg;
				tc = d_tc;
				discipline = d_disipline;
#ifdef sony
				jmode = d_jmode;
				jtc = d_jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */
			} else {
			    SysError(ERROR_OPDEVTTY);
			}
		} else {
			/* Get a copy of the current terminal's state,
			 * if we can.  Some systems (e.g., SVR4 and MacII)
			 * may not have a controlling terminal at this point
			 * if started directly from xdm or xinit,     
			 * in which case we just use the defaults as above.
			 */
#ifdef TIOCSLTC
			if(ioctl(tty, TIOCGLTC, &ltc) == -1)
				ltc = d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
			if(ioctl(tty, TIOCLGET, &lmode) == -1)
				lmode = d_lmode;
#endif	/* TIOCLSET */
#ifdef USE_SYSV_TERMIO
		        if(ioctl(tty, TCGETA, &tio) == -1)
			        tio = d_tio;

#else	/* not USE_SYSV_TERMIO */
			if(ioctl(tty, TIOCGETP, (char *)&sg) == -1)
			        sg = d_sg;
			if(ioctl(tty, TIOCGETC, (char *)&tc) == -1)
			        tc = d_tc;
			if(ioctl(tty, TIOCGETD, (char *)&discipline) == -1)
			        discipline = d_disipline;
#ifdef sony
			if(ioctl(tty, TIOCKGET, (char *)&jmode) == -1)
			        jmode = d_jmode;
			if(ioctl(tty, TIOCKGETC, (char *)&jtc) == -1)
				jtc = d_jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */
			close (tty);
			/* tty is no longer an open fd! */
			tty = -1;
		}

#ifdef 	PUCC_PTYD
		if(-1 == (screen->respond = openrpty(ttydev, ptydev,
				(resource.utmpInhibit ?  OPTY_NOP : OPTY_LOGIN),
				getuid(), XDisplayString(screen->display)))) {
#else /* not PUCC_PTYD */
#ifdef NEW_GET_PTY
		if (get_pty(&screen->respond,XDisplayString(screen->display))) {
#else
		if (get_pty(&screen->respond)) {
#endif
#endif /* PUCC_PTYD */
			/*  no ptys! */
			(void) fprintf(stderr, "%s: no available ptys\n",
				       xgterm_name);
			exit (ERROR_PTYS);
#ifdef PUCC_PTYD
		}
#else
		}			/* keep braces balanced for emacs */
#endif
#ifdef PUCC_PTYD
		  else {
			/*
			 *  set the fd of the master in a global var so
			 *  we can undo all this on exit
			 *
			 */
			Ptyfd = screen->respond;
		  }
#endif /* PUCC_PTYD */
	}

	/* avoid double MapWindow requests */
	XtSetMappedWhenManaged( XtParent(term), False );
	wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				       False);
	VTInit();		/* realize now so know window size for tty driver */
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
	if (Console) {
	    /*
	     * Inform any running xconsole program
	     * that we are going to steal the console.
	     */
	    XmuGetHostname (mit_console_name + MIT_CONSOLE_LEN, 255);
	    mit_console = XInternAtom(screen->display, mit_console_name, False);
	    /* the user told us to be the console, so we can use CurrentTime */
	    XtOwnSelection (XtParent(term),
			   mit_console, CurrentTime,
			   ConvertConsoleSelection, NULL, NULL);
	}
#endif
        if(screen->TekEmu) {
                envnew = tekterm;
                ptr = newtc;
        } else {
                envnew = vtterm;
                ptr = termcap;
        }
	TermName = NULL;
	if (resource.term_name) {
	    if (tgetent (ptr, resource.term_name) == 1) {
		TermName = resource.term_name;
                if (!screen->TekEmu)
		    resize (screen, TermName, termcap, newtc);
	    } else {
		fprintf (stderr, "%s:  invalid termcap entry \"%s\".\n",
			 ProgramName, resource.term_name);
	    }
	}
	if (!TermName) {
	    while (*envnew != NULL) {
		if(tgetent(ptr, *envnew) == 1) {
			TermName = *envnew;
                        if(!screen->TekEmu)
			    resize(screen, TermName, termcap, newtc);
			break;
		}
		envnew++;
	    }
	    if (TermName == NULL) {
		fprintf (stderr, "%s:  unable to find usable termcap entry.\n",
			 ProgramName);
		Exit (1);
	    }
	}

#if defined(sun) && !defined(SVR4)
#ifdef TIOCSSIZE
	/* tell tty how big window is */
        if(screen->TekEmu) {
                ts.ts_lines = 35;
                ts.ts_cols = 80;
        } else {
                ts.ts_lines = screen->max_row + 1;
                ts.ts_cols = screen->max_col + 1;
        }
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
	/* tell tty how big window is */
        if(screen->TekEmu) {
		/* We can only guess at the size at this point. */
                ws.ws_row = 35;
                ws.ws_col = 80;
                ws.ws_xpixel = 640;
                ws.ws_ypixel = 480;
        } else {
                ws.ws_row = screen->max_row + 1;
                ws.ws_col = screen->max_col + 1;
                ws.ws_xpixel = FullWidth(screen);
                ws.ws_ypixel = FullHeight(screen);
        }
#endif	/* TIOCSWINSZ */
#endif	/* sun */

	if (!am_slave) {
#ifdef USE_HANDSHAKE
	    if (pipe(pc_pipe) || pipe(cp_pipe))
		SysError (ERROR_FORK);
#endif
	    if ((screen->pid = fork ()) == -1)
		SysError (ERROR_FORK);
		
	    if (screen->pid == 0) {
		/*
		 * now in child process
		 */
		extern char **environ;
#if defined(_POSIX_SOURCE) || defined(SVR4) || defined(__convex__)
		int pgrp = setsid();
#else
		int pgrp = getpid();
#endif
#ifdef USE_SYSV_TERMIO
		char numbuf[12];
#endif	/* USE_SYSV_TERMIO */
#if defined(UTMP) && defined(USE_SYSV_UTMP)
		char *ptyname;
#endif

#ifdef USE_USG_PTYS
#if defined(SYSV) && defined(SYSV386)
                if (IsPts) {	/* SYSV386 supports both, which did we open? */
#endif /* SYSV && SYSV386 */
		int ptyfd;

		setpgrp();
		grantpt (screen->respond);
		unlockpt (screen->respond);
		if ((ptyfd = open (ptsname(screen->respond), O_RDWR)) < 0) {
		    SysError (1);
		}
#ifdef I_PUSH
		if (ioctl (ptyfd, I_PUSH, "ptem") < 0) {
		    SysError (2);
		}
#if !defined(SVR4) && !defined(SYSV386)
		if (!getenv("CONSEM") && ioctl (ptyfd, I_PUSH, "consem") < 0) {
		    SysError (3);
		}
#endif /* !SVR4 */
		if (ioctl (ptyfd, I_PUSH, "ldterm") < 0) {
		    SysError (4);
		}
#ifdef SVR4			/* from Sony */
#if !defined(sgi) && !defined(ibm)
		if (ioctl (ptyfd, I_PUSH, "ttcompat") < 0) {
		    SysError (5);
		}
#endif /* !defined(sgi) */
#endif /* SVR4 */
#endif /* I_PUSH */
		tty = ptyfd;
		close (screen->respond);
#ifdef TIOCSWINSZ
                /* tell tty how big window is */
                if(screen->TekEmu) {
                        ws.ws_row = 35;
                        ws.ws_col = 80;
                        ws.ws_xpixel = 640;
                        ws.ws_ypixel = 480;
                } else {
                        ws.ws_row = screen->max_row + 1;
                        ws.ws_col = screen->max_col + 1;
                        ws.ws_xpixel = FullWidth(screen);
                        ws.ws_ypixel = FullHeight(screen);
                }
#endif
#if defined(SYSV) && defined(SYSV386)
                } else {	/* else pty, not pts */
#endif /* SYSV && SYSV386 */
#endif /* USE_USG_PTYS */

#ifdef USE_HANDSHAKE		/* warning, goes for a long ways */
		/* close parent's sides of the pipes */
		close (cp_pipe[0]);
		close (pc_pipe[1]);

		/* Make sure that our sides of the pipes are not in the
		 * 0, 1, 2 range so that we don't fight with stdin, out
		 * or err.
		 */
		if (cp_pipe[1] <= 2) {
			if ((i = fcntl(cp_pipe[1], F_DUPFD, 3)) >= 0) {
				(void) close(cp_pipe[1]);
				cp_pipe[1] = i;
			}
		}
		if (pc_pipe[0] <= 2) {
			if ((i = fcntl(pc_pipe[0], F_DUPFD, 3)) >= 0) {
				(void) close(pc_pipe[0]);
				pc_pipe[0] = i;
			}
		}

		/* we don't need the socket, or the pty master anymore */
		close (Xsocket);
		close (screen->respond);

		/* Now is the time to set up our process group and
		 * open up the pty slave.
		 */
#ifdef	USE_SYSV_PGRP
#if defined(CRAY) && (OSMAJORVERSION > 5)
		(void) setsid();
#else
		(void) setpgrp();
#endif
#endif /* USE_SYSV_PGRP */
		while (1) {

#ifdef TIOCNOTTY
			if (!no_dev_tty && (tty = open ("/dev/tty", O_RDWR)) >= 0) {
				ioctl (tty, TIOCNOTTY, (char *) NULL);
				close (tty);
			}
#endif /* TIOCNOTTY */
			if ((tty = open(ttydev, O_RDWR, 0)) >= 0) {
#if defined(CRAY) && defined(TCSETCTTY)
			    /* make /dev/tty work */
			    ioctl(tty, TCSETCTTY, 0);
#endif
#ifdef	USE_SYSV_PGRP
				/* We need to make sure that we are acutally
				 * the process group leader for the pty.  If
				 * we are, then we should now be able to open
				 * /dev/tty.
				 */
				if ((i = open("/dev/tty", O_RDWR, 0)) >= 0) {
					/* success! */
					close(i);
					break;
				}
#else	/* USE_SYSV_PGRP */
				break;
#endif	/* USE_SYSV_PGRP */
			}

#ifdef TIOCSCTTY
			ioctl(tty, TIOCSCTTY, 0);
#endif
			/* let our master know that the open failed */
			handshake.status = PTY_BAD;
			handshake.error = errno;
			strcpy(handshake.buffer, ttydev);
			write(cp_pipe[1], (char *) &handshake,
			    sizeof(handshake));

			/* get reply from parent */
			i = read(pc_pipe[0], (char *) &handshake,
			    sizeof(handshake));
			if (i <= 0) {
				/* parent terminated */
				exit(1);
			}

			if (handshake.status == PTY_NOMORE) {
				/* No more ptys, let's shutdown. */
				exit(1);
			}

			/* We have a new pty to try */
			free(ttydev);
			ttydev = malloc((unsigned)
			    (strlen(handshake.buffer) + 1));
			strcpy(ttydev, handshake.buffer);
		}

		/* use the same tty name that everyone else will use
		** (from ttyname)
		*/
		if (ptr = ttyname(tty))
		{
			/* it may be bigger */
			ttydev = realloc (ttydev, (unsigned) (strlen(ptr) + 1));
			(void) strcpy(ttydev, ptr);
		}
#if defined(SYSV) && defined(SYSV386)
                } /* end of IsPts else clause */
#endif /* SYSV && SYSV386 */

#endif /* USE_HANDSHAKE -- from near fork */

#ifdef USE_TTY_GROUP
	{ 
#include <grp.h>		/* R6 update	*/
		struct group *ttygrp;
		if (ttygrp = getgrnam("tty")) {
			/* change ownership of tty to real uid, "tty" gid */
			chown (ttydev, screen->uid, ttygrp->gr_gid);
			chmod (ttydev, 0620);
		}
		else {
			/* change ownership of tty to real group and user id */
			chown (ttydev, screen->uid, screen->gid);
			chmod (ttydev, 0622);
		}
		endgrent();
	}
#else /* else !USE_TTY_GROUP */
		/* change ownership of tty to real group and user id */
		chown (ttydev, screen->uid, screen->gid);

		/* change protection of tty */
		chmod (ttydev, 0622);
#endif /* USE_TTY_GROUP */

		/*
		 * set up the tty modes
		 */
		{
/*
#ifdef USE_SYSV_TERMIO
*/
#if defined(USE_ANY_SYSV_TERMIO) || defined(USE_POSIX_TERMIOS)
#if defined(umips) || defined(CRAY)  || defined(linux)
		    /* If the control tty had its modes screwed around with,
		       eg. by lineedit in the shell, or emacs, etc. then tio
		       will have bad values.  Let's just get termio from the
		       new tty and tailor it.  */
		    if (ioctl (tty, TCGETA, &tio) == -1)
		      SysError (ERROR_TIOCGETP);
		    tio.c_lflag |= ECHOE;
#endif /* umips */
		    /* Now is also the time to change the modes of the
		     * child pty.
		     */
		    /* input: nl->nl, don't ignore cr, cr->nl */
		    tio.c_iflag &= ~(INLCR|IGNCR);
		    tio.c_iflag |= ICRNL;
		    /* ouput: cr->cr, nl is not return, no delays, ln->cr/nl */
		    tio.c_oflag &=
		     ~(OCRNL|ONLRET|NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
		    tio.c_oflag |= ONLCR;
#ifdef OPOST
		    tio.c_oflag |= OPOST;
#endif /* OPOST */		    
#ifdef BAUD_0
		    /* baud rate is 0 (don't care) */
		    tio.c_cflag &= ~(CBAUD);
#else	/* !BAUD_0 */
		    /* baud rate is 9600 (nice default) */
		    tio.c_cflag &= ~(CBAUD);
		    tio.c_cflag |= B9600;
#endif	/* !BAUD_0 */
		    /* enable signals, canonical processing (erase, kill, etc),
		    ** echo
		    */
		    tio.c_lflag |= ISIG|ICANON|ECHO;
		    /* reset EOL to defalult value */
		    tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
		    /* certain shells (ksh & csh) change EOF as well */
		    tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/

#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value;
		    if (override_tty_modes) {
			/* sysv-specific */
			TMODE (XTTYMODE_intr, tio.c_cc[VINTR]);
			TMODE (XTTYMODE_quit, tio.c_cc[VQUIT]);
			TMODE (XTTYMODE_erase, tio.c_cc[VERASE]);
			TMODE (XTTYMODE_kill, tio.c_cc[VKILL]);
			TMODE (XTTYMODE_eof, tio.c_cc[VEOF]);
			TMODE (XTTYMODE_eol, tio.c_cc[VEOL]);
#ifdef VSWTCH
			TMODE (XTTYMODE_swtch, d_tio.c_cc[VSWTCH]);
#endif
#ifdef TIOCSLTC
			/* both SYSV and BSD have ltchars */
			TMODE (XTTYMODE_susp, ltc.t_suspc);
			TMODE (XTTYMODE_dsusp, ltc.t_dsuspc);
			TMODE (XTTYMODE_rprnt, ltc.t_rprntc);
			TMODE (XTTYMODE_flush, ltc.t_flushc);
			TMODE (XTTYMODE_weras, ltc.t_werasc);
			TMODE (XTTYMODE_lnext, ltc.t_lnextc);
#endif
		    }
#undef TMODE

		    if (ioctl (tty, TCSETA, &tio) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCSETP);
#ifdef TIOCSLTC
#ifndef hpux
		    if (ioctl (tty, TIOCSLTC, &ltc) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCSETC);
#endif
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
		    if (ioctl (tty, TIOCLSET, (char *)&lmode) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCLSET);
#endif	/* TIOCLSET */
#else	/* USE_SYSV_TERMIO */
		    sg.sg_flags &= ~(ALLDELAY | XTABS | CBREAK | RAW);
		    sg.sg_flags |= ECHO | CRMOD;
		    /* make sure speed is set on pty so that editors work right*/
		    sg.sg_ispeed = B9600;
		    sg.sg_ospeed = B9600;
		    /* reset t_brkc to default value */
		    tc.t_brkc = -1;
#ifdef sony
		    if (screen->input_eight_bits)
			lmode |= LPASS8;
		    else
			lmode &= ~(LPASS8);
		    jmode &= ~KM_KANJI;
#endif /* sony */

#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value;
		    if (override_tty_modes) {
			TMODE (XTTYMODE_intr, tc.t_intrc);
			TMODE (XTTYMODE_quit, tc.t_quitc);
			TMODE (XTTYMODE_erase, sg.sg_erase);
			TMODE (XTTYMODE_kill, sg.sg_kill);
			TMODE (XTTYMODE_eof, tc.t_eofc);
			TMODE (XTTYMODE_start, tc.t_startc);
			TMODE (XTTYMODE_stop, tc.t_stopc);
			TMODE (XTTYMODE_brk, tc.t_brkc);
			/* both SYSV and BSD have ltchars */
			TMODE (XTTYMODE_susp, ltc.t_suspc);
			TMODE (XTTYMODE_dsusp, ltc.t_dsuspc);
			TMODE (XTTYMODE_rprnt, ltc.t_rprntc);
			TMODE (XTTYMODE_flush, ltc.t_flushc);
			TMODE (XTTYMODE_weras, ltc.t_werasc);
			TMODE (XTTYMODE_lnext, ltc.t_lnextc);
		    }
#undef TMODE

		    if (ioctl (tty, TIOCSETP, (char *)&sg) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETP);
#ifndef hpux
		    if (ioctl (tty, TIOCSETC, (char *)&tc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETC);
#endif
		    if (ioctl (tty, TIOCSETD, (char *)&discipline) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETD);
		    if (ioctl (tty, TIOCSLTC, (char *)&ltc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSLTC);
		    if (ioctl (tty, TIOCLSET, (char *)&lmode) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCLSET);
#ifdef sony
		    if (ioctl (tty, TIOCKSET, (char *)&jmode) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCKSET);
		    if (ioctl (tty, TIOCKSETC, (char *)&jtc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCKSETC);
#endif /* sony */
#endif	/* !USE_SYSV_TERMIO */
#if defined(TIOCCONS) || defined(SRIOCSREDIR)
		    if (Console) {
#ifdef TIOCCONS
			int on = 1;
			if (ioctl (tty, TIOCCONS, (char *)&on) == -1)
			    fprintf(stderr, "%s: cannot open console\n",
				    xgterm_name);
#endif
#ifdef SRIOCSREDIR
			int fd = open("/dev/console",O_RDWR);
			if (fd == -1 || ioctl (fd, SRIOCSREDIR, tty) == -1)
			    fprintf(stderr, "%s: cannot open console\n",
				    xgterm_name);
			(void) close (fd);
#endif
		    }
#endif	/* TIOCCONS */
		}

		signal (SIGCHLD, SIG_DFL);
#ifdef USE_SYSV_SIGHUP
		/* watch out for extra shells (I don't understand either) */
		signal (SIGHUP, SIG_DFL);
#else
		signal (SIGHUP, SIG_IGN);
#endif
		/* restore various signals to their defaults */
		signal (SIGINT, SIG_DFL);
		signal (SIGQUIT, SIG_DFL);
		signal (SIGTERM, SIG_DFL);

		/* copy the environment before Setenving */
		for (i = 0 ; environ [i] != NULL ; i++)
		    ;
		/* compute number of Setenv() calls below */
		envsize = 1;	/* (NULL terminating entry) */
		envsize += 3;	/* TERM, WINDOWID, DISPLAY */
#ifdef UTMP
		envsize += 1;   /* LOGNAME */
#endif /* UTMP */
#ifdef USE_SYSV_ENVVARS
#ifndef TIOCSWINSZ		/* window size not stored in driver? */
		envsize += 2;	/* COLUMNS, LINES */
#endif /* TIOCSWINSZ */
#ifdef UTMP
		envsize += 2;   /* HOME, SHELL */
#endif /* UTMP */
#else /* USE_SYSV_ENVVARS */
		envsize += 1;	/* TERMCAP */
#endif /* USE_SYSV_ENVVARS */
		envnew = (char **) calloc ((unsigned) i + envsize, sizeof(char *));
		memmove( (char *)envnew, (char *)environ, i * sizeof(char *));
		environ = envnew;
		Setenv ("TERM=", TermName);
		if(!TermName)
			*newtc = 0;

		sprintf (buf, "%lu",
			 ((unsigned long) XtWindow (XtParent(term))));
		Setenv ("WINDOWID=", buf);
		/* put the display into the environment of the shell*/
		Setenv ("DISPLAY=", XDisplayString (screen->display));

		signal(SIGTERM, SIG_DFL);

		/* this is the time to go and set up stdin, out, and err
		 */
		{
#if defined(CRAY) && (OSMAJORVERSION >= 6)
		    (void) close(tty);
		    (void) close(0);

		    if (open ("/dev/tty", O_RDWR)) {
			fprintf(stderr, "cannot open /dev/tty\n");
			exit(1);
		    }
		    (void) close(1);
		    (void) close(2);
		    dup(0);
		    dup(0);
#else
		    /* dup the tty */
		    for (i = 0; i <= 2; i++)
			if (i != tty) {
			    (void) close(i);
			    (void) dup(tty);
			}

#ifndef ATT
		    /* and close the tty */
		    if (tty > 2)
			(void) close(tty);
#endif
#endif /* CRAY */
		}

#ifndef	USE_SYSV_PGRP
#ifdef TIOCSCTTY
		setsid();
		ioctl(0, TIOCSCTTY, 0);
#endif
		ioctl(0, TIOCSPGRP, (char *)&pgrp);
		setpgrp(0,0);
		close(open(ttydev, O_WRONLY, 0));
		setpgrp (0, pgrp);
#endif /* !USE_SYSV_PGRP */

#ifdef UTMP
		pw = getpwuid(screen->uid);
		if (pw && pw->pw_name)
		    Setenv ("LOGNAME=", pw->pw_name); /* for POSIX */
#ifdef USE_SYSV_UTMP
		/* Set up our utmp entry now.  We need to do it here
		** for the following reasons:
		**   - It needs to have our correct process id (for
		**     login).
		**   - If our parent was to set it after the fork(),
		**     it might make it out before we need it.
		**   - We need to do it before we go and change our
		**     user and group id's.
		*/
#ifdef CRAY
#define PTYCHARLEN 4
#else
#define PTYCHARLEN 2
#endif

		(void) setutent ();
		/* set up entry to search for */
		ptyname = ttydev;
		(void) strncpy(utmp.ut_id,ptyname + strlen(ptyname)-PTYCHARLEN,
			       sizeof (utmp.ut_id));
		utmp.ut_type = DEAD_PROCESS;

		/* position to entry in utmp file */
		(void) getutid(&utmp);

		/* set up the new entry */
		utmp.ut_type = USER_PROCESS;
#ifndef linux
		utmp.ut_exit.e_exit = 2;
#endif
		(void) strncpy(utmp.ut_user,
			       (pw && pw->pw_name) ? pw->pw_name : "????",
			       sizeof(utmp.ut_user));
		    
		(void)strncpy(utmp.ut_id, ptyname + strlen(ptyname)-PTYCHARLEN,
			sizeof(utmp.ut_id));
		(void) strncpy (utmp.ut_line,
			ptyname + strlen("/dev/"), sizeof (utmp.ut_line));

#ifdef HAS_UTMP_UT_HOST
		(void) strncpy(buf, DisplayString(screen->display),
			       sizeof(buf));
#ifndef linux 			/* R6 update	*/
	        {
		    char *disfin = strrchr(buf, ':');
		    if (disfin)
			*disfin = '\0';
		}
#endif				/* R6 update	*/
		(void) strncpy(utmp.ut_host, buf, sizeof(utmp.ut_host));
#endif
		(void) strncpy(utmp.ut_name, pw->pw_name, 
			       sizeof(utmp.ut_name));

		utmp.ut_pid = getpid();
#ifdef SVR4
		utmp.ut_session = getsid(0);
		utmp.ut_xtime = time ((Time_t *) 0);
		utmp.ut_tv.tv_usec = 0;
#else
		utmp.ut_time = time ((Time_t *) 0);
#endif

		/* write out the entry */
		if (!resource.utmpInhibit)
		    (void) pututline(&utmp);
#ifdef WTMP
#ifdef SVR4
		if (term->misc.login_shell)
		    updwtmpx(WTMPX_FILE, &utmp);
#else
		if (term->misc.login_shell &&
		     (i = open(etc_wtmp, O_WRONLY|O_APPEND)) >= 0) {
		    write(i, (char *)&utmp, sizeof(struct utmp));
		    close(i);
		}
#endif
#endif
		/* close the file */
		(void) endutent();

#else	/* USE_SYSV_UTMP */
		/* We can now get our ttyslot!  We can also set the initial
		 * UTMP entry.
		 */
		tslot = ttyslot();
		added_utmp_entry = False;
		{
			if (pw && !resource.utmpInhibit &&
			    (i = open(etc_utmp, O_WRONLY)) >= 0) {
				memset((char *)&utmp, 0, sizeof(struct utmp));
				(void) strncpy(utmp.ut_line,
					       ttydev + strlen("/dev/"),
					       sizeof(utmp.ut_line));
				(void) strncpy(utmp.ut_name, pw->pw_name,
					       sizeof(utmp.ut_name));
#ifdef HAS_UTMP_UT_HOST
				(void) strncpy(utmp.ut_host, 
					       XDisplayString (screen->display),
					       sizeof(utmp.ut_host));
#endif
				time(&utmp.ut_time);
				lseek(i, (long)(tslot * sizeof(struct utmp)), 0);
				write(i, (char *)&utmp, sizeof(struct utmp));
				close(i);
				added_utmp_entry = True;
#ifdef WTMP
				if (term->misc.login_shell &&
				(i = open(etc_wtmp, O_WRONLY|O_APPEND)) >= 0) {
				    int status;
				    status = write(i, (char *)&utmp,
						   sizeof(struct utmp));
				    status = close(i);
				}
#endif /* WTMP */
#ifdef MNX_LASTLOG
                                if (term->misc.login_shell &&
                                (i = open(_U_LASTLOG, O_WRONLY)) >= 0) {
                                    lseek(i, (long)(screen->uid *
                                        sizeof (struct utmp)), 0);
                                    write(i, (char *)&utmp,
                                        sizeof (struct utmp));
                                    close(i);
                                }
#endif /* MNX_LASTLOG */
#ifdef LASTLOG
				if (term->misc.login_shell &&
				(i = open(etc_lastlog, O_WRONLY)) >= 0) {
				    memset((char *)&lastlog, 0,
					sizeof (struct lastlog));
				    (void) strncpy(lastlog.ll_line, ttydev +
					sizeof("/dev"),
					sizeof (lastlog.ll_line));
				    (void) strncpy(lastlog.ll_host, 
					  XDisplayString (screen->display),
					  sizeof (lastlog.ll_host));
				    time(&lastlog.ll_time);
				    lseek(i, (long)(screen->uid *
					sizeof (struct lastlog)), 0);
				    write(i, (char *)&lastlog,
					sizeof (struct lastlog));
				    close(i);
				}
#endif /* LASTLOG */
			} else
				tslot = -tslot;
		}

		/* Let's pass our ttyslot to our parent so that it can
		 * clean up after us.
		 */
#ifdef USE_HANDSHAKE
		handshake.tty_slot = tslot;
#endif /* USE_HANDSHAKE */
#endif /* USE_SYSV_UTMP */

#ifdef USE_HANDSHAKE
		/* Let our parent know that we set up our utmp entry
		 * so that it can clean up after us.
		 */
		handshake.status = UTMP_ADDED;
		handshake.error = 0;
		strcpy(handshake.buffer, ttydev);
		(void)write(cp_pipe[1], (char *)&handshake, sizeof(handshake));
#endif /* USE_HANDSHAKE */
#endif/* UTMP */

		(void) setgid (screen->gid);
#ifdef HAS_BSD_GROUPS
		if (geteuid() == 0 && pw)
		  initgroups (pw->pw_name, pw->pw_gid);
#endif
		(void) setuid (screen->uid);

#ifdef USE_HANDSHAKE
		/* mark the pipes as close on exec */
		fcntl(cp_pipe[1], F_SETFD, 1);
		fcntl(pc_pipe[0], F_SETFD, 1);

		/* We are at the point where we are going to
		 * exec our shell (or whatever).  Let our parent
		 * know we arrived safely.
		 */
		handshake.status = PTY_GOOD;
		handshake.error = 0;
		(void)strcpy(handshake.buffer, ttydev);
		(void)write(cp_pipe[1], (char *)&handshake, sizeof(handshake));

		if (waiting_for_initial_map) {
		    i = read (pc_pipe[0], (char *) &handshake,
			      sizeof(handshake));
		    if (i != sizeof(handshake) ||
			handshake.status != PTY_EXEC) {
			/* some very bad problem occurred */
			exit (ERROR_PTY_EXEC);
		    }
		    if(handshake.rows > 0 && handshake.cols > 0) {
			screen->max_row = handshake.rows;
			screen->max_col = handshake.cols;
#if defined(sun) && !defined(SVR4)
#ifdef TIOCSSIZE
			ts.ts_lines = screen->max_row + 1;
			ts.ts_cols = screen->max_col + 1;
#endif /* TIOCSSIZE */
#else /* !sun */
#ifdef TIOCSWINSZ
			ws.ws_row = screen->max_row + 1;
			ws.ws_col = screen->max_col + 1;
			ws.ws_xpixel = FullWidth(screen);
			ws.ws_ypixel = FullHeight(screen);
#endif /* TIOCSWINSZ */
#endif /* sun else !sun */
		    }
		}
#endif /* USE_HANDSHAKE */

#ifdef USE_SYSV_ENVVARS
		sprintf (numbuf, "%d", screen->max_col + 1);
		Setenv("COLUMNS=", numbuf);
		sprintf (numbuf, "%d", screen->max_row + 1);
		Setenv("LINES=", numbuf);
#ifdef UTMP
		if (pw) {	/* SVR4 doesn't provide these */
		    if (!getenv("HOME"))
			Setenv("HOME=", pw->pw_dir);
		    if (!getenv("SHELL"))
			Setenv("SHELL=", pw->pw_shell);
		}
#endif /* UTMP */
#else /* USE_SYSV_ENVVAR */
                if(!screen->TekEmu) {
                    strcpy (termcap, newtc);
                    resize (screen, TermName, termcap, newtc);
                }
		if (term->misc.titeInhibit) {
		    remove_termcap_entry (newtc, ":ti=");
		    remove_termcap_entry (newtc, ":te=");
		}
		/*
		 * work around broken termcap entries */
		if (resource.useInsertMode)	{
		    remove_termcap_entry (newtc, ":ic=");
		    /* don't get duplicates */
		    remove_termcap_entry (newtc, ":im=");
		    remove_termcap_entry (newtc, ":ei=");
		    remove_termcap_entry (newtc, ":mi");
		    strcat (newtc, ":im=\\E[4h:ei=\\E[4l:mi:");
		}
		Setenv ("TERMCAP=", newtc);
#endif /* USE_SYSV_ENVVAR */


		/* need to reset after all the ioctl bashing we did above */
#if defined(sun) && !defined(SVR4)
#ifdef TIOCSSIZE
		ioctl  (0, TIOCSSIZE, &ts);
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
		ioctl (0, TIOCSWINSZ, (char *)&ws);
#endif	/* TIOCSWINSZ */
#endif	/* sun */

		signal(SIGHUP, SIG_DFL);
		if (command_to_exec) {
			execvp(*command_to_exec, command_to_exec);
			/* print error message on screen */
			fprintf(stderr, "%s: Can't execvp %s\n", xgterm_name,
			 *command_to_exec);
		} 

#ifdef USE_SYSV_SIGHUP
		/* fix pts sh hanging around */
		signal (SIGHUP, SIG_DFL);
#endif
#ifdef _IBMR2
		/* The AIX 3.1 "rsh" command turns off INTR in sigmask. We
		   must turn it back on.  No you cannot solve this by calling
		   signal(SIGINT,SIG_DFL).  Tried.
		   -- Dan Greening dgreen@ibm.com  */
		{
		  extern int sigprocmask();
		  int mask;
		  sigset_t set, oset;
	  
		  sigemptyset(&set);
		  sigprocmask(SIG_SETMASK,&set,&oset);
		}
#endif /* _IBMR2 */	

#ifdef UTMP
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw == NULL && (pw = getpwuid(screen->uid)) == NULL) ||
		 *(ptr = pw->pw_shell) == 0))
#else	/* UTMP */
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw = getpwuid(screen->uid)) == NULL ||
		 *(ptr = pw->pw_shell) == 0))
#endif	/* UTMP */
			ptr = "/bin/sh";
		if(shname = strrchr(ptr, '/'))
			shname++;
		else
			shname = ptr;
		shname_minus = malloc(strlen(shname) + 2);
		(void) strcpy(shname_minus, "-");
		(void) strcat(shname_minus, shname);
#ifndef USE_SYSV_TERMIO
		ldisc = XStrCmp("csh", shname + strlen(shname) - 3) == 0 ?
		 NTTYDISC : 0;
		ioctl(0, TIOCSETD, (char *)&ldisc);
#endif	/* !USE_SYSV_TERMIO */

#ifdef USE_LOGIN_DASH_P
		if (term->misc.login_shell && pw && added_utmp_entry)
		  execl (bin_login, "login", "-p", "-f", pw->pw_name, 0);
#endif
		execlp (ptr, (term->misc.login_shell ? shname_minus : shname),
			0);

		/* Exec failed. */
		fprintf (stderr, "%s: Could not exec %s!\n", xgterm_name, ptr);
		(void) sleep(5);
		exit(ERROR_EXEC);
	    }				/* end if in child after fork */

#ifdef USE_HANDSHAKE
	    /* Parent process.  Let's handle handshaked requests to our
	     * child process.
	     */

	    /* close childs's sides of the pipes */
	    close (cp_pipe[1]);
	    close (pc_pipe[0]);

	    for (done = 0; !done; ) {
		if (read(cp_pipe[0], (char *) &handshake, sizeof(handshake)) <= 0) {
			/* Our child is done talking to us.  If it terminated
			 * due to an error, we will catch the death of child
			 * and clean up.
			 */
			break;
		}

		switch(handshake.status) {
		case PTY_GOOD:
			/* Success!  Let's free up resources and
			 * continue.
			 */
			done = 1;
			break;

		case PTY_BAD:
			/* The open of the pty failed!  Let's get
			 * another one.
			 */
			(void) close(screen->respond);
#ifdef NEW_GET_PTY
			if (get_pty(&screen->respond, XDisplayString(screen->display))) {
#else
			if (get_pty(&screen->respond)) {
#endif
			    /* no more ptys! */
			    (void) fprintf(stderr,
			      "%s: child process can find no available ptys\n",
			      xgterm_name);
			    handshake.status = PTY_NOMORE;
			    write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
			    exit (ERROR_PTYS);
			}
			handshake.status = PTY_NEW;
			(void) strcpy(handshake.buffer, ttydev);
			write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
			break;

		case PTY_FATALERROR:
			errno = handshake.error;
			close(cp_pipe[0]);
			close(pc_pipe[1]);
			SysError(handshake.fatal_error);

		case UTMP_ADDED:
			/* The utmp entry was set by our slave.  Remember
			 * this so that we can reset it later.
			 */
			added_utmp_entry = True;
#ifndef	USE_SYSV_UTMP
			tslot = handshake.tty_slot;
#endif	/* USE_SYSV_UTMP */
			free(ttydev);
			ttydev = malloc((unsigned) strlen(handshake.buffer) + 1);
			strcpy(ttydev, handshake.buffer);
			break;
		default:
			fprintf(stderr, "%s: unexpected handshake status %d\n",
			        xgterm_name, handshake.status);
		}
	    }
	    /* close our sides of the pipes */
	    if (!waiting_for_initial_map) {
		close (cp_pipe[0]);
		close (pc_pipe[1]);
	    }
#endif /* USE_HANDSHAKE */
	}				/* end if no slave */

	/*
	 * still in parent (xgterm process)
	 */

#ifdef USE_SYSV_SIGHUP
	/* hung sh problem? */
	signal (SIGHUP, SIG_DFL);
#else
	signal (SIGHUP,SIG_IGN);
#endif

/*
 * Unfortunately, System V seems to have trouble divorcing the child process
 * from the process group of xgterm.  This is a problem because hitting the 
 * INTR or QUIT characters on the keyboard will cause xgterm to go away if we
 * don't ignore the signals.  This is annoying.
 */

#if defined(USE_SYSV_SIGNALS) && !defined(SIGTSTP)
	signal (SIGINT, SIG_IGN);

#ifndef SYSV
	/* hung shell problem */
	signal (SIGQUIT, SIG_IGN);
#endif
	signal (SIGTERM, SIG_IGN);
#else /* else is bsd or has job control */
#ifdef SYSV
	/* if we were spawned by a jobcontrol smart shell (like ksh or csh),
	 * then our pgrp and pid will be the same.  If we were spawned by
	 * a jobcontrol dumb shell (like /bin/sh), then we will be in our
	 * parent's pgrp, and we must ignore keyboard signals, or we will
	 * tank on everything.
	 */
	if (getpid() == getpgrp()) {
	    (void) signal(SIGINT, Exit);
	    (void) signal(SIGQUIT, Exit);
	    (void) signal(SIGTERM, Exit);
	} else {
	    (void) signal(SIGINT, SIG_IGN);
	    (void) signal(SIGQUIT, SIG_IGN);
	    (void) signal(SIGTERM, SIG_IGN);
	}
	(void) signal(SIGPIPE, Exit);
#else	/* SYSV */
	signal (SIGINT, Exit);
	signal (SIGQUIT, Exit);
	signal (SIGTERM, Exit);
        signal (SIGPIPE, Exit);
#endif	/* SYSV */
#endif /* USE_SYSV_SIGNALS and not SIGTSTP */

	return 0;
}							/* end spawn */
#endif



SIGNAL_T
Exit(n)
	int n;
{
	register TScreen *screen = &term->screen;
        int pty = term->screen.respond;  /* file descriptor of pty */
#ifdef UTMP
#ifdef USE_SYSV_UTMP
#ifdef SVR4
	struct utmpx utmp;
	struct utmpx *utptr;
#else
	struct utmp utmp;
	struct utmp *utptr;
#endif
	char *ptyname;
#if defined(WTMP) && !defined(SVR4)
	int fd;			/* for /etc/wtmp */
	int i;
#endif

#ifdef PUCC_PTYD
	closepty(ttydev, ptydev, (resource.utmpInhibit ?  OPTY_NOP : OPTY_LOGIN), Ptyfd);
#endif /* PUCC_PTYD */

	/* cleanup the utmp entry we forged earlier */
	if (!resource.utmpInhibit
#ifdef USE_HANDSHAKE		/* without handshake, no way to know */
	    && added_utmp_entry
#endif /* USE_HANDSHAKE */
	    ) {
	    ptyname = ttydev;
	    utmp.ut_type = USER_PROCESS;
	    (void) strncpy(utmp.ut_id, ptyname + strlen(ptyname) - PTYCHARLEN,
			   sizeof(utmp.ut_id));
	    (void) setutent();
	    utptr = getutid(&utmp);
	    /* write it out only if it exists, and the pid's match */
	    if (utptr && (utptr->ut_pid == screen->pid)) {
		    utptr->ut_type = DEAD_PROCESS;
#ifdef SVR4
		    utmp.ut_session = getsid(0);
		    utmp.ut_xtime = time ((Time_t *) 0);
		    utmp.ut_tv.tv_usec = 0;
#else
		    utptr->ut_time = time((Time_t *) 0);
#endif
		    (void) pututline(utptr);
#ifdef WTMP
#ifdef SVR4
		    updwtmpx(WTMPX_FILE, &utmp);
#else
		    /* set wtmp entry if wtmp file exists */
		    if ((fd = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
		      i = write(fd, utptr, sizeof(utmp));
		      i = close(fd);
		    }
#endif
#endif

	    }
	    (void) endutent();
	}
#else	/* not USE_SYSV_UTMP */
	register int wfd;
	register int i;
	struct utmp utmp;

	if (!resource.utmpInhibit && added_utmp_entry &&
	    (!am_slave && tslot > 0 && (wfd = open(etc_utmp, O_WRONLY)) >= 0)){
		memset((char *)&utmp, 0, sizeof(struct utmp));
		lseek(wfd, (long)(tslot * sizeof(struct utmp)), 0);
		write(wfd, (char *)&utmp, sizeof(struct utmp));
		close(wfd);
#ifdef WTMP
		if (term->misc.login_shell &&
		    (wfd = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
			(void) strncpy(utmp.ut_line, ttydev +
			    sizeof("/dev"), sizeof (utmp.ut_line));
			time(&utmp.ut_time);
			i = write(wfd, (char *)&utmp, sizeof(struct utmp));
			i = close(wfd);
		}
#endif /* WTMP */
	}
#endif	/* USE_SYSV_UTMP */
#endif	/* UTMP */
        close(pty); /* close explicitly to avoid race with slave side */
#ifdef ALLOWLOGGING
	if(screen->logging)
		CloseLog(screen);
#endif

	if (!am_slave) {
		/* restore ownership of tty and pty */
		chown (ttydev, 0, 0);
#ifndef sgi
		chown (ptydev, 0, 0);
#endif /* !sgi */

		/* restore modes of tty and pty */
		chmod (ttydev, 0666);
#ifndef sgi
		chmod (ptydev, 0666);
#endif /* !sgi */
	}
	exit(n);
	SIGNAL_RETURN;
}

/* ARGSUSED */
resize(screen, TermName, oldtc, newtc)
TScreen *screen;
char *TermName;
register char *oldtc, *newtc;
{
#ifndef USE_SYSV_ENVVARS
	register char *ptr1, *ptr2;
	register int i;
	register int li_first = 0;
	register char *temp;

	if ((ptr1 = strindex (oldtc, "co#")) == NULL){
		strcat (oldtc, "co#80:");
		ptr1 = strindex (oldtc, "co#");
	}
	if ((ptr2 = strindex (oldtc, "li#")) == NULL){
		strcat (oldtc, "li#24:");
		ptr2 = strindex (oldtc, "li#");
	}
	if(ptr1 > ptr2) {
		li_first++;
		temp = ptr1;
		ptr1 = ptr2;
		ptr2 = temp;
	}
	ptr1 += 3;
	ptr2 += 3;
	strncpy (newtc, oldtc, i = ptr1 - oldtc);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_row + 1 :
	 screen->max_col + 1);
	newtc += strlen(newtc);
	ptr1 = strchr(ptr1, ':');
	strncpy (newtc, ptr1, i = ptr2 - ptr1);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_col + 1 :
	 screen->max_row + 1);
	ptr2 = strchr(ptr2, ':');
	strcat (newtc, ptr2);
#endif /* USE_SYSV_ENVVARS */
}

/*
 * Does a non-blocking wait for a child process.  If the system
 * doesn't support non-blocking wait, do nothing.
 * Returns the pid of the child, or 0 or -1 if none or error.
 */
int
nonblocking_wait()
{
#ifdef USE_POSIX_WAIT
        pid_t pid;

	pid = waitpid(-1, NULL, WNOHANG);
#else /* USE_POSIX_WAIT */
#if defined(USE_SYSV_SIGNALS) && (defined(CRAY) || !defined(SIGTSTP))
	/* cannot do non-blocking wait */
	int pid = 0;
#else	/* defined(USE_SYSV_SIGNALS) && (defined(CRAY) || !defined(SIGTSTP)) */
	/* union wait status; */ int status;
	register int pid;

	pid = wait3 (&status, WNOHANG, (struct rusage *)NULL);
#endif /* defined(USE_SYSV_SIGNALS) && !defined(SIGTSTP) */
#endif /* USE_POSIX_WAIT else */

	return pid;
}

/* ARGSUSED */
static SIGNAL_T reapchild (n)
    int n;
{
    int pid;

    pid = wait(NULL);

#ifdef USE_SYSV_SIGNALS
    /* cannot re-enable signal before waiting for child
       because then SVR4 loops.  Sigh.  HP-UX 9.01 too.  */
    (void) signal(SIGCHLD, reapchild);
#endif

    do {
	if (pid == term->screen.pid) {
#ifdef DEBUG
	    if (debug) fputs ("Exiting\n", stderr);
#endif
	    Cleanup (0);
        }
    } while ( (pid=nonblocking_wait()) > 0);

    SIGNAL_RETURN;
}

/* VARARGS1 */
consolepr(fmt,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
char *fmt;
{
	extern char *SysErrorMsg();
	int oerrno;
	int f;
 	char buf[ BUFSIZ ];

	oerrno = errno;
 	strcpy(buf, "new-xgterm: ");
 	sprintf(buf+strlen(buf), fmt, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9);
 	strcat(buf, ": ");
 	strcat(buf, SysErrorMsg (oerrno));
 	strcat(buf, "\n");	
	f = open("/dev/console",O_WRONLY);
	write(f, buf, strlen(buf));
	close(f);
#ifdef TIOCNOTTY
	if ((f = open("/dev/tty", 2)) >= 0) {
		ioctl(f, TIOCNOTTY, (char *)NULL);
		close(f);
	}
#endif	/* TIOCNOTTY */
}


remove_termcap_entry (buf, str)
    char *buf;
    char *str;
{
    register char *strinbuf;

    strinbuf = strindex (buf, str);
    if (strinbuf) {
        register char *colonPtr = strchr(strinbuf+1, ':');
        if (colonPtr) {
            while (*colonPtr) {
                *strinbuf++ = *colonPtr++;      /* copy down */
            }
            *strinbuf = '\0';
        } else {
            strinbuf[1] = '\0';
        }
    }
    return 0;
}

/*
 * parse_tty_modes accepts lines of the following form:
 *
 *         [SETTING] ...
 *
 * where setting consists of the words in the modelist followed by a character
 * or ^char.
 */
static int parse_tty_modes (s, modelist)
    char *s;
    struct _xttymodes *modelist;
{
    struct _xttymodes *mp;
    int c;
    int count = 0;

    while (1) {
	while (*s && isascii(*s) && isspace(*s)) s++;
	if (!*s) return count;

	for (mp = modelist; mp->name; mp++) {
	    if (strncmp (s, mp->name, mp->len) == 0) break;
	}
	if (!mp->name) return -1;

	s += mp->len;
	while (*s && isascii(*s) && isspace(*s)) s++;
	if (!*s) return -1;

	if (*s == '^') {
	    s++;
	    c = ((*s == '?') ? 0177 : *s & 31);	 /* keep control bits */
	} else {
	    c = *s;
	}
	mp->value = c;
	mp->set = 1;
	count++;
	s++;
    }
}


int GetBytesAvailable (fd)
    int fd;
{
#ifdef FIONREAD
    static long arg;
    ioctl (fd, FIONREAD, (char *) &arg);
    return (int) arg;
#else
#ifdef FIORDCK
    return (ioctl (fd, FIORDCHK, NULL));
#else
    struct pollfd pollfds[1];

    pollfds[0].fd = fd;
    pollfds[0].events = POLLIN;
    return poll (pollfds, 1, 0);
#endif
#endif
}

/* Utility function to try to hide system differences from
   everybody who used to call killpg() */

int
kill_process_group(pid, sig)
    int pid;
    int sig;
{
#ifndef X_NOT_POSIX
    return kill (-pid, sig);
#else
#if defined(SVR4) || defined(SYSV)
    return kill (-pid, sig);
#else
    return killpg (pid, sig);
#endif
#endif
}
