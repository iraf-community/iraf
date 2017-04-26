/*
 *    $XConsortium: misc.c,v 1.102 94/03/28 18:27:08 gildea Exp $
 */

/*
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *                         All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#include "ptyx.h"		/* X headers included here. */

#include <X11/Xos.h>
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <ctype.h>
#include <pwd.h>
#include <errno.h>

#include <X11/Xatom.h>
#include <X11/cursorfont.h>

#include <X11/Shell.h>
#include <X11/Xmu/Error.h>
#include <X11/Xmu/SysUtil.h>
#include <X11/Xmu/WinUtil.h>

#include "data.h"
#include "error.h"
#include "menu.h"
#include "gtermio.h"

extern jmp_buf VTend;

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *malloc();
extern char *getenv();
#endif

static void DoSpecialEnterNotify();
static void DoSpecialLeaveNotify();

extern XtAppContext app_con;

xevents()
{
        XEvent event;
        XtInputMask input_mask;
        register TScreen *screen = &term->screen;

        if(screen->scroll_amt)
                FlushScroll(screen);
        /*
         * process timeouts, relying on the fact that XtAppProcessEvent
         * will process the timeout and return without blockng on the
         * XEvent queue.  Other sources i.e. the pty are handled elsewhere
         * with select().
         */
        while ((input_mask = XtAppPending(app_con)) & XtIMTimer)
                XtAppProcessEvent(app_con, XtIMTimer);
        /*
         * If there's no XEvents, don't wait around...
         */
        if ((input_mask & XtIMXEvent) != XtIMXEvent)
                return;
        do {
                if (waitingForTrackInfo)
                        return;
                XtAppNextEvent (app_con, &event);
                /*
                 * Hack to get around problems with the toolkit throwing away
                * eventing during the exclusive grab of the menu popup.  By
                 * looking at the event ourselves we make sure that we can
                 * do the right thing.
                 */
                if (event.type == EnterNotify &&
                    (event.xcrossing.window == XtWindow(XtParent(term))))
                  DoSpecialEnterNotify (&event.xcrossing);
                else if (event.type == LeaveNotify &&
                    (event.xcrossing.window == XtWindow(XtParent(term))))
                  DoSpecialLeaveNotify (&event.xcrossing);

                if (!event.xany.send_event ||
                    screen->allowSendEvents ||
                    ((event.xany.type != KeyPress) &&
                     (event.xany.type != KeyRelease) &&
                     (event.xany.type != ButtonPress) &&
                     (event.xany.type != ButtonRelease)))
                    XtDispatchEvent(&event);
        } while ((input_mask = XtAppPending(app_con)) & XtIMXEvent);
}


Cursor make_colored_cursor (cursorindex, fg, bg)
	int cursorindex;			/* index into font */
	unsigned long fg, bg;			/* pixel value */
{
	register TScreen *screen = &term->screen;
	Cursor c;
	register Display *dpy = screen->display;
	
	c = XCreateFontCursor (dpy, cursorindex);
	if (c == (Cursor) 0) return (c);

	recolor_cursor (c, fg, bg);
	return (c);
}

/* ARGSUSED */
void HandleKeyPressed(w, event, params, nparams)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register TScreen *screen = &term->screen;

#ifdef ACTIVEWINDOWINPUTONLY
    if (w == (Widget)term)
#endif
	Input (&term->keyboard, screen, &event->xkey, False);
}
/* ARGSUSED */
void HandleEightBitKeyPressed(w, event, params, nparams)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register TScreen *screen = &term->screen;

#ifdef ACTIVEWINDOWINPUTONLY
    if (w == (Widget)term)
#endif
	Input (&term->keyboard, screen, &event->xkey, True);
}

/* ARGSUSED */
void HandleStringEvent(w, event, params, nparams)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register TScreen *screen = &term->screen;

#ifdef ACTIVEWINDOWINPUTONLY
    if (w != (Widget)term)
	return;
#endif

    if (*nparams != 1) return;

    if ((*params)[0] == '0' && (*params)[1] == 'x' && (*params)[2] != '\0') {
	char c, *p, hexval[2];
	hexval[0] = hexval[1] = 0;
	for (p = *params+2; (c = *p); p++) {
	    hexval[0] *= 16;
	    if (isupper(c)) c = tolower(c);
	    if (c >= '0' && c <= '9')
		hexval[0] += c - '0';
	    else if (c >= 'a' && c <= 'f')
		hexval[0] += c - 'a' + 10;
	    else break;
	}
	if (c == '\0')
	    StringInput (screen, hexval, 1);
    }
    else {
	StringInput (screen, *params, strlen(*params));
    }
}

static void DoSpecialEnterNotify (ev)
    register XEnterWindowEvent *ev;
{
    register TScreen *screen = &term->screen;

#ifdef ACTIVEWINDOWINPUTONLY
    if (ev->window == (Widget)term)
#endif
      if (((ev->detail) != NotifyInferior) &&
	  ev->focus &&
	  !(screen->select & FOCUS))
	selectwindow(screen, INWINDOW);
}

/*ARGSUSED*/
void HandleEnterWindow(w, eventdata, event)
Widget w;
register XEnterWindowEvent *event;
caddr_t eventdata;
{
    /* This code is necessary as xevent does not see all events anymore. */
    XEvent *ev = (XEvent *) event;
    if (ev->type == EnterNotify &&
            (ev->xcrossing.window == XtWindow(XtParent(term))))
        DoSpecialEnterNotify (&ev->xcrossing);
}


static void DoSpecialLeaveNotify (ev)
    register XEnterWindowEvent *ev;
{
    register TScreen *screen = &term->screen;

#ifdef ACTIVEWINDOWINPUTONLY
    if (ev->window == (Widget)term)
#endif
      if (((ev->detail) != NotifyInferior) &&
	  ev->focus &&
	  !(screen->select & FOCUS))
	unselectwindow(screen, INWINDOW);
}


/*ARGSUSED*/
void HandleLeaveWindow(w, eventdata, event)
Widget w;
register XEnterWindowEvent *event;
caddr_t eventdata;
{
    /* This code is necessary as xevent does not see all events anymore. */
    XEvent *ev = (XEvent *) event;
    if (ev->type == LeaveNotify &&
            (ev->xcrossing.window == XtWindow(XtParent(term))))
        DoSpecialLeaveNotify (&ev->xcrossing);
}


/*ARGSUSED*/
void HandleFocusChange(w, eventdata, event)
Widget w;
register XFocusChangeEvent *event;
caddr_t eventdata;
{
        register TScreen *screen = &term->screen;

        if(event->type == FocusIn)
                selectwindow(screen,
			     (event->detail == NotifyPointer) ? INWINDOW :
								FOCUS);
        else {
                unselectwindow(screen,
			       (event->detail == NotifyPointer) ? INWINDOW :
								  FOCUS);
		if (screen->grabbedKbd && (event->mode == NotifyUngrab)) {
		    XBell(screen->display, 100);
		    ReverseVideo(term);
		    screen->grabbedKbd = FALSE;
		    update_securekbd();
		}
	}
}



selectwindow(screen, flag)
register TScreen *screen;
register int flag;
{
#ifdef I18N
        if (screen->xic)
            XSetICFocus(screen->xic);
#endif
	if(screen->cursor_state &&
	   (screen->cursor_col != screen->cur_col ||
	    screen->cursor_row != screen->cur_row))
	    HideCursor();
	screen->select |= flag;
	if(screen->cursor_state)
		ShowCursor();
	return;
}

unselectwindow(screen, flag)
register TScreen *screen;
register int flag;
{
	if (screen->always_highlight) return;

#ifdef I18N
        if (screen->xic)
            XUnsetICFocus(screen->xic);
#endif
	screen->select &= ~flag;
	if(screen->cursor_state &&
	   (screen->cursor_col != screen->cur_col ||
	    screen->cursor_row != screen->cur_row))
	      HideCursor();
	if(screen->cursor_state)
	  ShowCursor();
}

static long lastBellTime;	/* in milliseconds */

Bell()
{
    extern XgtermWidget term;
    register TScreen *screen = &term->screen;
    struct timeval curtime;
    long now_msecs;

    /* has enough time gone by that we are allowed to ring
       the bell again? */
    if(screen->bellSuppressTime) {
	if(screen->bellInProgress) {
	    if (XtAppPending(app_con) ||
		GetBytesAvailable (ConnectionNumber(screen->display)) > 0)
		xevents();
	    if(screen->bellInProgress) { /* even after new events? */
		return;
	    }
	}
#ifdef X_GETTIMEOFDAY
	X_GETTIMEOFDAY(&curtime);
#else
	gettimeofday(&curtime, NULL);
#endif
	now_msecs = 1000*curtime.tv_sec + curtime.tv_usec/1000;
	if(lastBellTime != 0  &&  now_msecs - lastBellTime >= 0  &&
	   now_msecs - lastBellTime < screen->bellSuppressTime) {
	    return;
	}
	lastBellTime = now_msecs;
    }

    if (screen->visualbell)
	VisualBell();
    else
	XBell(screen->display, 0);

    if(screen->bellSuppressTime) {
	/* now we change a property and wait for the notify event to come
	   back.  If the server is suspending operations while the bell
	   is being emitted (problematic for audio bell), this lets us
	   know when the previous bell has finished */
	Widget w = (Widget) term;
	XChangeProperty(XtDisplay(w), XtWindow(w),
			XA_NOTICE, XA_NOTICE, 8, PropModeAppend, NULL, 0);
	screen->bellInProgress = TRUE;
    }
}


VisualBell()
{
    extern XgtermWidget term;
    register TScreen *screen = &term->screen;
    register Pixel xorPixel = screen->foreground ^ term->core.background_pixel;
    XGCValues gcval;
    GC visualGC;

    gcval.function = GXxor;
    gcval.foreground = xorPixel;
    visualGC = XtGetGC((Widget)term, GCFunction+GCForeground, &gcval);
    XFillRectangle(
		   screen->display,
		   VWindow(screen), 
		   visualGC,
		   0, 0,
		   (unsigned) FullWidth(screen),
		   (unsigned) FullHeight(screen));
    XFlush(screen->display);
    XFillRectangle(
		   screen->display,
		   VWindow(screen), 
		   visualGC,
		   0, 0,
		   (unsigned) FullWidth(screen),
		   (unsigned) FullHeight(screen));
}

/* ARGSUSED */
void HandleBellPropertyChange(w, data, ev, more)
    Widget w;
    XtPointer data;
    XEvent *ev;
    Boolean *more;
{
    register TScreen *screen = &term->screen;

    if (ev->xproperty.atom == XA_NOTICE) {
	screen->bellInProgress = FALSE;
    }
}

Redraw()
{
	extern XgtermWidget term;
	register TScreen *screen = &term->screen;
	XExposeEvent event;

	event.type = Expose;
	event.display = screen->display;
	event.x = 0;
	event.y = 0;
	event.count = 0; 
	
	if (VWindow(screen)) {
	    event.window = VWindow(screen);
	    event.width = term->core.width;
	    event.height = term->core.height;
	    (*term->core.widget_class->core_class.expose) (
		(Widget)term, (XEvent *)&event, NULL);
	    if (screen->scrollbar) 
		(*screen->scrollWidget->core.widget_class->core_class.expose) (
		    screen->scrollWidget, (XEvent *)&event, NULL);
	}
}

#if defined(ALLOWLOGGING) || defined(DEBUG)

#ifndef X_NOT_POSIX
#define HAS_WAITPID
#endif

/*
 * create a file only if we could with the permissions of the real user id.
 * We could emulate this with careful use of access() and following
 * symbolic links, but that is messy and has race conditions.
 * Forking is messy, too, but we can't count on setreuid() or saved set-uids
 * being available.
 */
void
creat_as(uid, gid, pathname, mode)
    int uid;
    int gid;
    char *pathname;
    int mode;
{
    int fd;
    int waited;
    int pid;
#ifndef HAS_WAITPID
    int (*chldfunc)();

    chldfunc = signal(SIGCHLD, SIG_DFL);
#endif
    pid = fork();
    switch (pid)
    {
    case 0:			/* child */
	setgid(gid);
	setuid(uid);
	fd = open(pathname, O_WRONLY|O_CREAT|O_APPEND, mode);
	if (fd >= 0) {
	    close(fd);
	    _exit(0);
	} else
	    _exit(1);
    case -1:			/* error */
	return;
    default:			/* parent */
#ifdef HAS_WAITPID
	waitpid(pid, NULL, 0);
#else
	waited = wait(NULL);
	signal(SIGCHLD, chldfunc);
	/*
	  Since we had the signal handler uninstalled for a while,
	  we might have missed the termination of our screen child.
	  If we can check for this possibility without hanging, do so.
	*/
	do
	    if (waited == term->screen.pid)
		Cleanup(0);
	while ( (waited=nonblocking_wait()) > 0);
#endif
    }
}
#endif

#ifdef ALLOWLOGGING
/*
 * logging is a security hole, since it allows a setuid program to
 * write arbitrary data to an arbitrary file.  So it is disabled
 * by default.
 */ 

StartLog(screen)
register TScreen *screen;
{
	register char *cp;
	register int i;
	static char *log_default;
#ifdef ALLOWLOGFILEEXEC
	void logpipe();
#ifdef SYSV
	/* SYSV has another pointer which should be part of the
	** FILE structure but is actually a separate array.
	*/
	unsigned char *old_bufend;
#endif	/* SYSV */
#endif /* ALLOWLOGFILEEXEC */

	if(screen->logging || (screen->inhibit & I_LOG))
		return;
	if(screen->logfile == NULL || *screen->logfile == 0) {
		if(screen->logfile)
			free(screen->logfile);
		if(log_default == NULL)
			log_default = log_def_name;
		mkstemp(log_default);
		if((screen->logfile = malloc((unsigned)strlen(log_default) + 1)) == NULL)
			return;
		strcpy(screen->logfile, log_default);
	}
	if(*screen->logfile == '|') {	/* exec command */
#ifdef ALLOWLOGFILEEXEC
		/*
		 * Warning, enabling this "feature" allows arbitrary programs
		 * to be run.  If ALLOWLOGFILECHANGES is enabled, this can be
		 * done through escape sequences....  You have been warned.
		 */
		int p[2];
		static char *shell;

		if(pipe(p) < 0 || (i = fork()) < 0)
			return;
		if(i == 0) {	/* child */
			close(p[1]);
			dup2(p[0], 0);
			close(p[0]);
			dup2(fileno(stderr), 1);
			dup2(fileno(stderr), 2);
#ifdef SYSV
			old_bufend = _bufend(stderr);
#endif	/* SYSV */
			close(fileno(stderr));
			stderr->_file = 2;
#ifdef SYSV
			_bufend(stderr) = old_bufend;
#endif	/* SYSV */
			close(ConnectionNumber(screen->display));
			close(screen->respond);
			if(!shell) {
				register struct passwd *pw;
				struct passwd *getpwuid();

				if(((cp = getenv("SHELL")) == NULL || *cp == 0)
				 && ((pw = getpwuid(screen->uid)) == NULL ||
				 *(cp = pw->pw_shell) == 0) ||
				 (shell = malloc((unsigned) strlen(cp) + 1)) == NULL)
					shell = "/bin/sh";
				else
					strcpy(shell, cp);
			}
			signal(SIGHUP, SIG_DFL);
			signal(SIGCHLD, SIG_DFL);
			setgid(screen->gid);
			setuid(screen->uid);
			execl(shell, shell, "-c", &screen->logfile[1], 0);
			fprintf(stderr, "%s: Can't exec `%s'\n", xgterm_name,
			 &screen->logfile[1]);
			exit(ERROR_LOGEXEC);
		}
		close(p[0]);
		screen->logfd = p[1];
		signal(SIGPIPE, logpipe);
#else
		Bell();
		Bell();
		return;
#endif
	} else {
		if(access(screen->logfile, F_OK) != 0) {
		    if (errno == ENOENT)
			creat_as(screen->uid, screen->gid,
				 screen->logfile, 0644);
		    else
			return;
		}

		if(access(screen->logfile, F_OK) != 0
		   || access(screen->logfile, W_OK) != 0)
		    return;
		if((screen->logfd = open(screen->logfile, O_WRONLY | O_APPEND,
					 0644)) < 0)
			return;
	}
	screen->logstart = bptr;
	screen->logging = TRUE;
	update_logging();
}

CloseLog(screen)
register TScreen *screen;
{
	if(!screen->logging || (screen->inhibit & I_LOG))
		return;
	FlushLog(screen);
	close(screen->logfd);
	screen->logging = FALSE;
	update_logging();
}

FlushLog(screen)
register TScreen *screen;
{
	register Char *cp;
	register int i;

/*
 * With xgterm pty input is read only in one place, hence logging is done
 * immediately upon input.
 *
 *	cp = bptr;
 *	if((i = cp - screen->logstart) > 0)
 *		write(screen->logfd, (char *)screen->logstart, i);
 */
	screen->logstart = buffer;
}

#ifdef ALLOWLOGFILEEXEC
void logpipe()
{
	register TScreen *screen = &term->screen;

#ifdef SYSV
	(void) signal(SIGPIPE, SIG_IGN);
#endif	/* SYSV */
	if(screen->logging)
		CloseLog(screen);
}
#endif /* ALLOWLOGFILEEXEC */
#endif /* ALLOWLOGGING */


do_osc(func)
int (*func)();
{
	register int mode, c;
	register char *cp;
	char buf[512];
	char *bufend = &buf[(sizeof buf) - 1];	/* leave room for null */
	Bool okay = True;

	/* 
	 * lines should be of the form <ESC> ] number ; string <BEL>
	 *
	 * where number is one of 0, 1, 2, or 46
	 */
	mode = 0;
	while(isdigit(c = (*func)()))
		mode = 10 * mode + (c - '0');
	if (c != ';') okay = False;
	cp = buf;
	while(isprint((c = (*func)()) & 0x7f) && cp < bufend)
		*cp++ = c;
	if (c != 7) okay = False;
	*cp = 0;
	if (okay) switch(mode) {
	 case 0:	/* new icon name and title*/
		Changename(buf);
		Changetitle(buf);
		break;

	 case 1:	/* new icon name only */
		Changename(buf);
		break;

	 case 2:	/* new title only */
		Changetitle(buf);
		break;
        case 10:       case 11:        case 12:
        case 13:       case 14:        case 15:
        case 16:
               {
                   extern Boolean ChangeColorsRequest();
                   if (term->misc.dynamicColors)
                       ChangeColorsRequest(term,mode-10,buf);
               }
               break;


#ifdef ALLOWLOGGING
	 case 46:	/* new log file */
#ifdef ALLOWLOGFILECHANGES
		/*
		 * Warning, enabling this feature allows people to overwrite
		 * arbitrary files accessible to the person running xgterm.
		 */
		if((cp = malloc((unsigned)strlen(buf) + 1)) == NULL)
			break;
		strcpy(cp, buf);
		if(term->screen.logfile)
			free(term->screen.logfile);
		term->screen.logfile = cp;
#else
		Bell();
		Bell();
#endif
		break;
#endif /* ALLOWLOGGING */

	case 50:
		SetVTFont (fontMenu_fontescape, True, buf, NULL);
		break;

	/*
	 * One could write code to send back the display and host names,
	 * but that could potentially open a fairly nasty security hole.
	 */
	}
}

static ChangeGroup(attribute, value)
     String attribute;
     XtArgVal value;
{
	extern Widget toplevel;
	Arg args[1];

	XtSetArg( args[0], attribute, value );
	XtSetValues( toplevel, args, 1 );
}

Changename(name)
register char *name;
{
    ChangeGroup( XtNiconName, (XtArgVal)name );
}

Changetitle(name)
register char *name;
{
    ChangeGroup( XtNtitle, (XtArgVal)name );
}

/***====================================================================***/

ScrnColors      *pOldColors= NULL;

Boolean
GetOldColors(pTerm)
XgtermWidget     pTerm;
{
int     i;
    if (pOldColors==NULL) {
        pOldColors=     (ScrnColors *)XtMalloc(sizeof(ScrnColors));
        if (pOldColors==NULL) {
            fprintf(stderr,"allocation failure in GetOldColors\n");
            return(FALSE);
        }
        pOldColors->which=      0;
        for (i=0;i<NCOLORS;i++) {
            pOldColors->colors[i]=      0;
            pOldColors->names[i]=       NULL;
        }
        GetColors(pTerm,pOldColors);
    }
    return(TRUE);
}

Boolean
UpdateOldColors(pTerm,pNew)
XgtermWidget     pTerm;
ScrnColors      *pNew;
{
int     i;

    /* if we were going to free old colors, this would be the place to
     * do it.   I've decided not to (for now), because it seems likely
     * that we'd have a small set of colors we use over and over, and that
     * we could save some overhead this way.   The only case in which this
     * (clearly) fails is if someone is trying a boatload of colors, in
     * which case they can restart xterm
     */
    for (i=0;i<NCOLORS;i++) {
        if (COLOR_DEFINED(pNew,i)) {
            if (pOldColors->names[i]!=NULL) {
                XtFree(pOldColors->names[i]);
                pOldColors->names[i]= NULL;
            }
            if (pNew->names[i]) {
                pOldColors->names[i]= pNew->names[i];
            }
            pOldColors->colors[i]=      pNew->colors[i];
        }
    }
    return(TRUE);
}

void
ReverseOldColors()
{
register ScrnColors     *pOld= pOldColors;
Pixel    tmpPix;
char    *tmpName;

    if (pOld) {
        /* change text cursor, if necesary */
        if (pOld->colors[TEXT_CURSOR]==pOld->colors[TEXT_FG]) {
            pOld->colors[TEXT_CURSOR]=  pOld->colors[TEXT_BG];
            if (pOld->names[TEXT_CURSOR]) {
                XtFree(pOldColors->names[TEXT_CURSOR]);
                pOld->names[TEXT_CURSOR]= NULL;
            }
            if (pOld->names[TEXT_BG]) {
                tmpName= XtMalloc(strlen(pOld->names[TEXT_BG])+1);
                if (tmpName) {
                    strcpy(tmpName,pOld->names[TEXT_BG]);
                    pOld->names[TEXT_CURSOR]= tmpName;
                }
            }
        }

        /* swap text FG and BG */
        tmpPix=         pOld->colors[TEXT_FG];
        tmpName=        pOld->names[TEXT_FG];
        pOld->colors[TEXT_FG]=  pOld->colors[TEXT_BG];
        pOld->names[TEXT_FG]=   pOld->names[TEXT_BG];
        pOld->colors[TEXT_BG]=  tmpPix;
        pOld->names[TEXT_BG]=   tmpName;

        /* swap mouse FG and BG */
        tmpPix=         pOld->colors[MOUSE_FG];
        tmpName=        pOld->names[MOUSE_FG];
        pOld->colors[MOUSE_FG]= pOld->colors[MOUSE_BG];
        pOld->names[MOUSE_FG]=  pOld->names[MOUSE_BG];
        pOld->colors[MOUSE_BG]= tmpPix;
        pOld->names[MOUSE_BG]=  tmpName;

        /* swap Tek FG and BG */
        tmpPix=         pOld->colors[TEK_FG];
        tmpName=        pOld->names[TEK_FG];
        pOld->colors[TEK_FG]=   pOld->colors[TEK_BG];
        pOld->names[TEK_FG]=    pOld->names[TEK_BG];
        pOld->colors[TEK_BG]=   tmpPix;
        pOld->names[TEK_BG]=    tmpName;
    }
    return;
}

Boolean
AllocateColor(pTerm,pNew,ndx,name)
XgtermWidget     pTerm;
ScrnColors      *pNew;
int              ndx;
char            *name;
{
XColor                   def;
register TScreen        *screen=        &pTerm->screen;
Colormap                 cmap=          pTerm->core.colormap;
char                    *newName;

    if ((XParseColor(screen->display,cmap,name,&def))&&
        (XAllocColor(screen->display,cmap,&def))) {
        SET_COLOR_VALUE(pNew,ndx,def.pixel);
        newName= XtMalloc(strlen(name)+1);
        if (newName) {
            strcpy(newName,name);
            SET_COLOR_NAME(pNew,ndx,newName);
        }
        return(TRUE);
    }
    return(FALSE);
}

Boolean
ChangeColorsRequest(pTerm,start,names)
XgtermWidget     pTerm;
int              start;
register char   *names;
{
char            *thisName;
ScrnColors      newColors;
int             i,ndx;

    if ((pOldColors==NULL)&&(!GetOldColors(pTerm))) {
        return(FALSE);
    }
    newColors.which=    0;
    for (i=0;i<NCOLORS;i++) {
        newColors.names[i]=     NULL;
    }
    for (i=start;i<NCOLORS;i++) {
        if (term->misc.re_verse)        ndx=    OPPOSITE_COLOR(i);
        else                            ndx=    i;
        if ((names==NULL)||(names[0]=='\0')) {
            newColors.names[ndx]=       NULL;
        }
        else {
            if (names[0]==';')
                 thisName=      NULL;
            else thisName=      names;
            names=      index(names,';');
            if (names!=NULL) {
                *names= '\0';
                names++;
            }
            if ((!pOldColors->names[ndx])||
                (thisName&&(strcmp(thisName,pOldColors->names[ndx])))) {
                AllocateColor(pTerm,&newColors,ndx,thisName);
            }
        }
    }

    if (newColors.which==0)
        return(TRUE);

    ChangeColors(pTerm,&newColors);
    UpdateOldColors(pTerm,&newColors);
    return(TRUE);
}

/***====================================================================***/



#ifndef DEBUG
/* ARGSUSED */
#endif
Panic(s, a)
char	*s;
int a;
{
#ifdef DEBUG
	if(debug) {
		fprintf(stderr, "%s: PANIC!	", xgterm_name);
		fprintf(stderr, s, a);
		fputs("\r\n", stderr);
		fflush(stderr);
	}
#endif	/* DEBUG */
}

char *SysErrorMsg (n)
    int n;
{
#if __STDC__
    return strerror(n);
#else

    return((n >= 0) ? (char *)strerror(n) : "unknown error");
#endif /* __STDC__ */
}


SysError (i)
int i;
{
	int oerrno;

	oerrno = errno;
	/* perror(3) write(2)s to file descriptor 2 */
	fprintf (stderr, "%s: Error %d, errno %d: ", xgterm_name, i, oerrno);
	fprintf (stderr, "%s\n", SysErrorMsg (oerrno));
	Cleanup(i);
}

Error (i)
int i;
{
	fprintf (stderr, "%s: Error %d\n", xgterm_name, i);
	Cleanup(i);
}


/*
 * cleanup by sending SIGHUP to client processes
 */
Cleanup (code)
int code;
{
	extern XgtermWidget term;
	register TScreen *screen;

	screen = &term->screen;
	if (screen->pid > 1) {
	    (void) kill_process_group (screen->pid, SIGHUP);
	}
	Exit (code);
}

/*
 * sets the value of var to be arg in the Unix 4.2 BSD environment env.
 * Var should end with '=' (bindings are of the form "var=value").
 * This procedure assumes the memory for the first level of environ
 * was allocated using calloc, with enough extra room at the end so not
 * to have to do a realloc().
 */
Setenv (var, value)
register char *var, *value;
{
	extern char **environ;
	register int envindex = 0;
	register int len = strlen(var);

	while (environ [envindex] != NULL) {
	    if (strncmp (environ [envindex], var, len) == 0) {
		/* found it */
		environ[envindex] = (char *)malloc ((unsigned)len + strlen (value) + 1);
		strcpy (environ [envindex], var);
		strcat (environ [envindex], value);
		return;
	    }
	    envindex ++;
	}

#ifdef DEBUG
	if (debug) fputs ("expanding env\n", stderr);
#endif	/* DEBUG */

	environ [envindex] = (char *) malloc ((unsigned)len + strlen (value) + 1);
	(void) strcpy (environ [envindex], var);
	strcat (environ [envindex], value);
	environ [++envindex] = NULL;
}

/*
 * returns a pointer to the first occurrence of s2 in s1,
 * or NULL if there are none.
 */
char *strindex (s1, s2)
register char	*s1, *s2;
{
	register char	*s3;
	int s2len = strlen (s2);

	while ((s3=strchr(s1, *s2)) != NULL) {
		if (strncmp(s3, s2, s2len) == 0)
			return (s3);
		s1 = ++s3;
	}
	return (NULL);
}


/* xerror -- Handle an XLIB server error.  A standard X error message is
 * printed and then the program either dumps core, exits, or ignores the error,
 * depending upon the value of the environment variable XGXERROR, if defined.
 */
/*ARGSUSED*/
xerror (display, event)
Display *display;
register XErrorEvent *event;
{
	static char *envvar = "XGXERROR";
	static char *env_maxerrs = "XGMAXERROR";
	static int nerrs = 0, maxerrs = -1;
	extern char *getenv();
	char fname[128];
	char *action = NULL, *err = NULL;
	int pid;

	/* Get the max number of allowable errors before we exit, defaults
	 * to 50.  This is handy either for debugging to trap errors right
  	 * away, or to increase the max value to run longer.
	 */
	if (maxerrs < 0) 
	    maxerrs = ((err = getenv (env_maxerrs)) ? atoi(err) : 50);

	/* If we define XGXERROR to be 'ignore' don't print out the standard
	 * error message, and don't count it.
	 */
	     
	action = getenv (envvar);
	if (!action || (action && strcmp (action, "ignore") != 0)) {

	    /* The default action is to ignore BadCursor messages but we
	     * define a 'catchall' action to let us bypass this.  Otherwise,
	     * print the standard X error message and count it towards the
	     * final shutdown.
	     */

            if (event->error_code == BadCursor ||
		(action && strcmp (action, "catchall") != 0)) {
                    return (0);
	    } else {
	        fprintf (stderr, 
	            "%s: warning, error event received:\n", xgterm_name);
	        (void) XmuPrintDefaultErrorMessage (display, event, stderr);

	        if (nerrs++ > maxerrs)
	            Exit (ERROR_XERROR);
	    }
	}

	if (action) {
	    if (strcmp (action, "dumpcore") == 0) {
		if ((pid = fork()) >= 0) {
		    if (pid) {
			fprintf (stderr, "dumping core... ");
			fflush (stderr);
			sprintf (fname, "core.%d", pid);
			wait(NULL); rename ("core", fname);
			fprintf (stderr, "core file core.%d written\n", pid);
			fflush (stderr);
		    } else
			kill (getpid(), 6);
		} else
		    fprintf (stderr, "fork failed, no core dump produced\n");
	    } else if (strcmp (action, "exit") == 0) {
		fprintf (stderr, "program terminated\n");
		Exit (ERROR_XERROR);
	    } else if (strcmp (action, "ignore") != 0)
		fprintf (stderr, "%s: unknown action %s\n", envvar, action);
	}

	return (0);
}

/*ARGSUSED*/
xioerror(dpy)
Display *dpy;
{
	(void) fprintf (stderr, 
	    "%s:  fatal IO error %d (%s) or KillClient on X server \"%s\"\r\n",
	    xgterm_name, errno, SysErrorMsg (errno),
	    DisplayString (dpy));

	Exit(ERROR_XIOERROR);
}

void xt_error(message)
    String message;
{
    extern char *ProgramName;

    (void) fprintf (stderr, "%s Xt error: %s\n", ProgramName, message);
    exit(1);
}

XStrCmp(s1, s2)
char *s1, *s2;
{
  if (s1 && s2) return(strcmp(s1, s2));
  if (s1 && *s1) return(1);
  if (s2 && *s2) return(-1);
  return(0);
}

static void withdraw_window (dpy, w, scr)
    Display *dpy;
    Window w;
    int scr;
{
    (void) XmuUpdateMapHints (dpy, w, NULL);
    XWithdrawWindow (dpy, w, scr);
    return;
}

void set_vt_visibility (on)
    Boolean on;
{
    register TScreen *screen = &term->screen;

    if (on) {
        if (!screen->Vshow && term) {
            VTInit ();
            XtMapWidget (term->core.parent);
            screen->Vshow = TRUE;
        }
    } else {
        if (screen->Vshow && term) {
            withdraw_window (XtDisplay (term),
                             XtWindow(XtParent(term)),
                             XScreenNumberOfScreen(XtScreen(term)));
            screen->Vshow = FALSE;
        }
    }
    set_vthide_sensitivity();
    set_tekhide_sensitivity();
    set_tekreset_sensitivity();
    update_vttekmode();
    update_tekshow();
    update_tekreset();
    update_vtshow();
}

extern Atom wm_delete_window;	/* for ICCCM delete window */

void set_tek_visibility (on)
Boolean on;
{
    register TScreen *screen = &term->screen;
    if (on) {
	if (!gt_activated())
	    gt_activate();
    } else {
	if (gt_activated())
	    gt_deactivate();
    }
    set_vthide_sensitivity();
    set_tekhide_sensitivity();
    set_tekreset_sensitivity();
    update_vttekmode();
    update_tekshow();
    update_tekreset();
    update_vtshow();
}

void end_tek_mode ()
{
    register TScreen *screen = &term->screen;

    if (gt_tekmode (2)) {
#ifdef ALLOWLOGGING
        if (screen->logging) {
            FlushLog (screen);
            screen->logstart = buffer;
        }
#endif
	gt_tekmode (0);
	update_vttekmode();
    } 
}

void end_vt_mode ()
{
    register TScreen *screen = &term->screen;

    if (!gt_tekmode (2)) {
#ifdef ALLOWLOGGING
        if(screen->logging) {
            FlushLog(screen);
            screen->logstart = Tbuffer;
        }
#endif
	gt_tekmode (1);
	update_vttekmode();
    } 
    return;
}

void switch_modes (tovt)
    Bool tovt;				/* if true, then become vt mode */
{
    if (tovt)
	gt_tekmode (0);
    else {
	gt_activate();
	gt_tekmode (1);
    }

    set_vthide_sensitivity();
    set_tekhide_sensitivity();
    set_tekreset_sensitivity();
    update_vttekmode();
    update_tekshow();
    update_tekreset();
    update_vtshow();
}

void hide_vt_window ()
{
    register TScreen *screen = &term->screen;

    set_vt_visibility (FALSE);
    switch_modes (False);
}

void hide_tek_window ()
{
    register TScreen *screen = &term->screen;

    set_tek_visibility (FALSE);
    switch_modes (True);
}


/*
 * The GTERMIO routines are called by the gtermio code during initialization
 * to provide hooks into the xgterm code.
 */

/* The following is called when a gtermio UI display connection is opened
 * or closed.  The gtermio UI code uses a separate display connection and
 * passes in the display and toplevel widget for this connection.
 */
void
gtermio_connect (notused, display, toplevel, state)
int notused;
Display *display;		/* UI display */
Widget toplevel;		/* toplevel widget */
int state;			/* 1=open, 0=close */
{
	TScreen *screen = &term->screen;

	if (state) {
	    gtermio_display = display;
	    gtermio_toplevel = toplevel;
	} else {
	    gtermio_display = NULL;
	    gtermio_toplevel = NULL;
            screen->tek_menu_item_bitmap = (Pixmap) NULL;
	    init_menu ("tekMenu");
	}
}


/* The following are called by the gtermio code when switching between
 * vt100 and graphics mode.
 */
static void
set_workstation_state (state)
{
	register TScreen *screen = &term->screen;

	screen->Tshow = state;
	set_tekhide_sensitivity();
	set_vthide_sensitivity();
	set_tekreset_sensitivity();
	update_vtshow();
	update_tekshow();
	update_tekreset();
	update_vttekmode();
}

gtermio_open_workstation()  { set_workstation_state(1); }
gtermio_close_workstation() { set_workstation_state(0); }


/* GTERMIO protocol module functions.
 */
int (*gtermio_reset)();		XtPointer gtermio_reset_data;
int (*gtermio_clear)();		XtPointer gtermio_clear_data;
int (*gtermio_input)();		XtPointer gtermio_input_data;
int (*gtermio_output)();	XtPointer gtermio_output_data;
int (*gtermio_activate)();	XtPointer gtermio_activate_data;
int (*gtermio_status)();	XtPointer gtermio_status_data;
int (*gtermio_enable)();	XtPointer gtermio_enable_data;
int (*gtermio_tekmode)();	XtPointer gtermio_tekmode_data;
int (*gtermio_SGMT)();		XtPointer gtermio_SGMT_data;

/* gtermio_register -- This routine is called by the GTERMIO protocol
 * module code during startup to register the protocol module's public
 * functions, called by the xgterm code during execution to process
 * graphics data, activate or deactivate the graphics window, and so on.
 */
gtermio_register (functions, nfunc)
struct GT_function *functions;
int nfunc;
{
	register struct GT_function *fp;
	register int i;

	for (i=0;  i < nfunc;  i++) {
	    fp = &functions[i];
	    if (strcmp (fp->name, "reset") == 0) {
		gtermio_reset = fp->func;
		gtermio_reset_data = fp->data;
	    } else if (strcmp (fp->name, "clear") == 0) {
		gtermio_clear = fp->func;
		gtermio_clear_data = fp->data;
	    } else if (strcmp (fp->name, "input") == 0) {
		gtermio_input = fp->func;
		gtermio_input_data = fp->data;
	    } else if (strcmp (fp->name, "output") == 0) {
		gtermio_output = fp->func;
		gtermio_output_data = fp->data;
	    } else if (strcmp (fp->name, "activate") == 0) {
		gtermio_activate = fp->func;
		gtermio_activate_data = fp->data;
	    } else if (strcmp (fp->name, "status") == 0) {
		gtermio_status = fp->func;
		gtermio_status_data = fp->data;
	    } else if (strcmp (fp->name, "enable") == 0) {
		gtermio_enable = fp->func;
		gtermio_enable_data = fp->data;
	    } else if (strcmp (fp->name, "tekmode") == 0) {
		gtermio_tekmode = fp->func;
		gtermio_tekmode_data = fp->data;
	    } else if (strcmp (fp->name, "setGinmodeTrailers") == 0) {
		gtermio_SGMT = fp->func;
		gtermio_SGMT_data = fp->data;
	    }
	}
}

/* gtermio_getResource -- Called by the gtermio code to get resource values
 * from Xgterm.
 */
char *
gtermio_getResource (name)
char *name;
{
	if (strcmp (name, "geometry") == 0)
	    return (term->misc.T_geometry);
	else
	    return (NULL);
}

/* gt_reset -- Reset the graphics window.
 */
gt_reset()
{
	if (gtermio_reset)
	    (*gtermio_reset)(gtermio_reset_data);
}

/* gt_clear -- Clear the graphics window.
 */
gt_clear()
{
	if (gtermio_clear)
	    (*gtermio_clear)(gtermio_clear_data);
}

/* gt_input -- Filter any graphics data out of the input data stream.  The
 * number of bytes of data left after the graphics data is removed is
 * returned as the function value.
 */
gt_input (bptr, bcnt)
char *bptr;
int bcnt;
{
	if (gtermio_input)
	    return ((*gtermio_input)(gtermio_input_data, bptr, bcnt));
	else
	    return (bcnt);
}

/* gt_flush -- Process the gterm output buffer once.  Any buffered input
 * graphics data is processed and output to the screen.
 */
gt_flush()
{
    if (gtermio_output)
	return ((*gtermio_output) (gtermio_output_data));
    else
	return (1);
}

/* gt_activate -- Activate the graphics window.
 */
gt_activate()
{
	if (gtermio_activate)
	    (*gtermio_activate)(gtermio_activate_data, 1);
}

/* gt_deactivate -- Deactivate the graphics window.
 */
gt_deactivate()
{
	if (gtermio_activate)
	    (*gtermio_activate)(gtermio_activate_data, 0);
}

/* gt_activated -- Test whether the graphics window is activated.
 */
gt_activated()
{
	if (gtermio_activate)
	    return ((*gtermio_activate)(gtermio_activate_data, 2));
	else
	    return (0);
}

/* gt_status -- Test whether the graphics window is instantiated or reset.
 */
gt_status()
{
	if (gtermio_status) {
	    char name[256];
	    int status;

	    if (status = (*gtermio_status)(gtermio_status_data, name, NULL)) {
		strncpy (gtermio_appname, name, SZ_APPNAME);
		gtermio_appname[SZ_APPNAME] = '\0';
	    } else
		gtermio_appname[0] = '\0';
	    return (status);

	} else {
	    gtermio_appname[0] = '\0';
	    return (0);
	}
}

/* gt_enable -- Enable the graphics window.  If the graphics window is
 * disabled graphics/text mode switches will be ignored and all output will
 * be directed to the text window.
 */
gt_enable (state)
int state;
{
	if (gtermio_enable)
	    return ((*gtermio_enable)(gtermio_enable_data, state));
	else
	    return (0);
}

/* gt_tekmode -- Activate the graphics window.
 */
gt_tekmode (state)
int state;
{
	register TScreen *screen = &term->screen;
	int tekEmu;

	if (gtermio_tekmode) {
	    tekEmu = ((*gtermio_tekmode)(gtermio_tekmode_data, state));
	    screen->TekEmu = tekEmu;
	} else
	    return (0);
}

/* g_set_ginmode_trailers -- Set the trailer codes used to deliminate a
 * cursor read value in the gtermio code.
 */
gt_set_ginmode_trailers (trailers)
char *trailers;
{
	if (gtermio_SGMT)
	    (*gtermio_SGMT)(gtermio_SGMT_data, trailers);
}
