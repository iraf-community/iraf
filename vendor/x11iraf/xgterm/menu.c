/* $XConsortium: menu.c,v 1.63 94/04/17 20:23:30 gildea Exp $ */
/*

Copyright (c) 1989  X Consortium

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

*/

#include "ptyx.h"
#include "data.h"
#include "menu.h"
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <stdio.h>
#include <signal.h>

extern void FindFontSelection();

Arg menuArgs[2] = {{ XtNleftBitmap, (XtArgVal) 0 },
		   { XtNsensitive, (XtArgVal) 0 }};

void do_hangup();

static void do_securekbd(), do_allowsends(), do_visualbell(),
#ifdef ALLOWLOGGING
    do_logging(),
#endif
    do_redraw(), do_suspend(), do_continue(), do_interrupt(), 
    do_terminate(), do_kill(), do_quit(), do_scrollbar(), do_jumpscroll(),
    do_reversevideo(), do_autowrap(), do_reversewrap(), do_autolinefeed(),
    do_appcursor(), do_appkeypad(), do_scrollkey(), do_scrollttyoutput(),
    do_allow132(), do_cursesemul(), do_marginbell(), do_altscreen(),
    do_softreset(), do_hardreset(), do_clearsavedlines(),
    do_vthide(), do_vtshow(), do_vtmode(), do_vtfont(),
    do_gioenable(), do_tekshow(), do_tekmode(), do_tekpage(),
    do_tekreset(), do_tekcopy(), do_tekhide(), do_colortext();


/*
 * The order entries MUST match the values given in menu.h
 */
MenuEntry mainMenuEntries[] = {
    { "securekbd",	do_securekbd, NULL },		/*  0 */
    { "allowsends",	do_allowsends, NULL },		/*  1 */
#ifdef ALLOWLOGGING
    { "logging",	do_logging, NULL },		/*  2 */
#endif
    { "redraw",		do_redraw, NULL },		/*  3 */
    { "line1",		NULL, NULL },			/*  4 */
    { "suspend",	do_suspend, NULL },		/*  5 */
    { "continue",	do_continue, NULL },		/*  6 */
    { "interrupt",	do_interrupt, NULL },		/*  7 */
    { "hangup",		do_hangup, NULL },		/*  8 */
    { "terminate",	do_terminate, NULL },		/*  9 */
    { "kill",		do_kill, NULL },		/* 10 */
    { "line2",		NULL, NULL },			/* 11 */
    { "quit",		do_quit, NULL }};		/* 12 */

MenuEntry vtMenuEntries[] = {
    { "scrollbar",	do_scrollbar, NULL },		/*  0 */
    { "jumpscroll",	do_jumpscroll, NULL },		/*  1 */
    { "reversevideo",	do_reversevideo, NULL },	/*  2 */
    { "colortext",	do_colortext, NULL },		/*  3 */
    { "line1",		NULL, NULL },			/*  4 */
    { "gioenable",	do_gioenable, NULL },		/*  5 */
    { "tekshow",	do_tekshow, NULL },		/*  6 */
    { "tekmode",	do_tekmode, NULL },		/*  7 */
    { "tekreset",       do_tekreset, NULL },            /*  8 */
    { "vthide",		do_vthide, NULL },		/*  9 */
    { "line2",		NULL, NULL },			/* 10 */
    { "autowrap",	do_autowrap, NULL },		/* 11 */
    { "reversewrap",	do_reversewrap, NULL },		/* 12 */
    { "autolinefeed",	do_autolinefeed, NULL },	/* 13 */
    { "appcursor",	do_appcursor, NULL },		/* 14 */
    { "appkeypad",	do_appkeypad, NULL },		/* 15 */
    { "scrollkey",	do_scrollkey, NULL },		/* 16 */
    { "scrollttyoutput",do_scrollttyoutput, NULL },	/* 17 */
    { "allow132",	do_allow132, NULL },		/* 18 */
    { "cursesemul",	do_cursesemul, NULL },		/* 19 */
    { "visualbell",	do_visualbell, NULL },		/* 20 */
    { "marginbell",	do_marginbell, NULL },		/* 21 */
    { "altscreen",	do_altscreen, NULL },		/* 22 */
    { "line3",		NULL, NULL },			/* 23 */
    { "softreset",	do_softreset, NULL },		/* 24 */
    { "hardreset",	do_hardreset, NULL },		/* 25 */
    { "clearsavedlines",do_clearsavedlines, NULL }};	/* 26 */

MenuEntry fontMenuEntries[] = {
    { "fontdefault",	do_vtfont, NULL },		/*  0 */
    { "font1",		do_vtfont, NULL },		/*  1 */
    { "font2",		do_vtfont, NULL },		/*  2 */
    { "font3",		do_vtfont, NULL },		/*  3 */
    { "font4",		do_vtfont, NULL },		/*  4 */
    { "font5",		do_vtfont, NULL },		/*  5 */
    { "font6",		do_vtfont, NULL },		/*  6 */
    { "fontescape",	do_vtfont, NULL },		/*  7 */
    { "fontsel",	do_vtfont, NULL }};		/*  8 */
    /* this should match NMENUFONTS in ptyx.h */

MenuEntry tekMenuEntries[] = {
    { "tekpage",        do_tekpage, NULL },		/*  0 */
    { "tekhide",        do_tekhide, NULL },		/*  1 */
    { "vtshow",         do_vtshow, NULL },		/*  2 */
    { "tekreset",       do_tekreset, NULL }};		/*  3 */

static Widget create_menu();
extern Widget toplevel;

/*
 * we really want to do these dynamically
 */
#define check_width 9
#define check_height 8
static unsigned char check_bits[] = {
   0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
   0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};


/*
 * public interfaces
 */

/* ARGSUSED */
static Bool domenu (w, event, params, param_count)
    Widget w;
    XEvent *event;              /* unused */
    String *params;             /* mainMenu, vtMenu, or tekMenu */
    Cardinal *param_count;      /* 0 or 1 */
{
    TScreen *screen = &term->screen;
    Widget ww;

    if (*param_count != 1) {
	XBell (XtDisplay(w), 0);
	return False;
    }

    switch (params[0][0]) {
      case 'm':
	if (!screen->mainMenu) {
	    screen->mainMenu = create_menu (term, toplevel, "mainMenu",
					    mainMenuEntries,
					    XtNumber(mainMenuEntries));
	    update_securekbd();
	    update_allowsends();
#ifdef ALLOWLOGGING
	    update_logging();
#endif
#ifndef SIGTSTP
	    set_sensitivity (screen->mainMenu,
			     mainMenuEntries[mainMenu_suspend].widget, FALSE);
#endif
#ifndef SIGCONT
	    set_sensitivity (screen->mainMenu, 
			     mainMenuEntries[mainMenu_continue].widget, FALSE);
#endif
	}
	break;

      case 'v':
	if (!screen->vtMenu) {
	    screen->vtMenu = create_menu (term, toplevel, "vtMenu",
					  vtMenuEntries,
					  XtNumber(vtMenuEntries));
	    /* and turn off the alternate screen entry */
	    set_altscreen_sensitivity (FALSE);
	    update_scrollbar();
	    update_jumpscroll();
	    update_reversevideo();
	    update_colortext();
	    update_autowrap();
	    update_reversewrap();
	    update_autolinefeed();
	    update_appcursor();
	    update_appkeypad();
	    update_scrollkey();
	    update_scrollttyoutput();
	    update_allow132();
	    update_cursesemul();
	    update_visualbell();
	    update_marginbell();
	    update_gioenable();
	    update_vttekmode();
	    update_tekshow();
	    update_tekreset();
	    update_vtshow();
	    set_vthide_sensitivity();
	    set_tekhide_sensitivity();
	    set_tekreset_sensitivity();
	}

	/* Update tekshow entry to reflect current graphics application. */
	if (ww = vtMenuEntries[vtMenu_tekshow].widget) {
	    char *ip, *op, *start;
	    char label[128];

	    XtVaGetValues (ww, XtNlabel, &ip, NULL);
	    for (op=label, start=NULL;  *op = *ip;  op++, ip++)
		if (*op == '(')
		    start = isspace(*(op-1)) ? op - 1 : op;
	    if (!start)
		start = label + strlen(label);

	    op = start;
	    if (gt_status()) {
		*op++ = ' ';
		*op++ = '(';
		for (ip=gtermio_appname;  *ip;  )
		    *op++ = *ip++;
		*op++ = ')';
	    }
	    *op = '\0';

	    XtVaSetValues (ww, XtNlabel, label, NULL);
	}
	break;

      case 'f':
	if (!screen->fontMenu) {
	    screen->fontMenu = create_menu (term, toplevel, "fontMenu",
					    fontMenuEntries,
					    NMENUFONTS);  
	    set_menu_font (True);
	    set_sensitivity (screen->fontMenu,
			     fontMenuEntries[fontMenu_fontescape].widget,
			     (screen->menu_font_names[fontMenu_fontescape]
			      ? TRUE : FALSE));
	}
	FindFontSelection (NULL, True);
	set_sensitivity (screen->fontMenu,
			 fontMenuEntries[fontMenu_fontsel].widget,
			 (screen->menu_font_names[fontMenu_fontsel]
			  ? TRUE : FALSE));
	break;

      case 't':
        if (!screen->tekMenu) {
            screen->tekMenu = create_menu (w, gtermio_toplevel, "tekMenu",
                                           tekMenuEntries,
                                           XtNumber(tekMenuEntries));
	    set_vthide_sensitivity();
	    set_tekhide_sensitivity();
	    set_tekreset_sensitivity();
	    update_vttekmode();
	    update_tekshow();
	    update_tekreset();
	    update_vtshow();
        }
        break;

      default:
	XBell (XtDisplay(w), 0);
	return False;
    }

    return True;
}

void HandleCreateMenu (w, event, params, param_count)
    Widget w;
    XEvent *event;              /* unused */
    String *params;             /* mainMenu, vtMenu, or tekMenu */
    Cardinal *param_count;      /* 0 or 1 */
{
    (void) domenu (w, event, params, param_count);
}

void HandlePopupMenu (w, event, params, param_count)
    Widget w;
    XEvent *event;              /* unused */
    String *params;             /* mainMenu, vtMenu, or tekMenu */
    Cardinal *param_count;      /* 0 or 1 */
{
    if (domenu (w, event, params, param_count)) {
	XtCallActionProc (w, "XawPositionSimpleMenu", event, params, 1);
	XtCallActionProc (w, "MenuPopup", event, params, 1);
    }
}


/*
 * private interfaces - keep out!
 */

/*
 * create_menu - create a popup shell and stuff the menu into it.
 */

static Widget create_menu (w, toplevelw, name, entries, nentries)
    Widget w;
    Widget toplevelw;
    char *name;
    struct _MenuEntry *entries;
    int nentries;
{
    TScreen *screen = &term->screen;
    static XtCallbackRec cb[2] = { { NULL, NULL }, { NULL, NULL }};
    static Arg arg = { XtNcallback, (XtArgVal) cb };
    Arg args[10];
    int nargs;
    Widget m;

    if (XtDisplay(w) == gtermio_display) {
	if (screen->tek_menu_item_bitmap == None) {
	    screen->tek_menu_item_bitmap =
	      XCreateBitmapFromData (XtDisplay(w),
		  RootWindowOfScreen(XtScreen(w)),
		  (char *)check_bits, check_width, check_height);
	}
    } else {
	if (screen->menu_item_bitmap == None) {
	    screen->menu_item_bitmap =
		XCreateBitmapFromData (XtDisplay(w),
		    RootWindowOfScreen(XtScreen(w)),
		    (char *)check_bits, check_width, check_height);
	}
    }

    m = XtCreatePopupShell (name, simpleMenuWidgetClass, toplevelw, NULL, 0);

    /* The following produces a double line to separate the menu title from
     * it's items.  It's not in the R6 xterm code but I left it here because
     * it's a nice effect.
     */
    nargs = 0;
    XtSetArg (args[nargs], XtNheight, 2);
	nargs++;
    XtCreateManagedWidget ("tline1", smeLineObjectClass, m, args, nargs);
    XtCreateManagedWidget ("tline2", smeLineObjectClass, m, args, nargs);

    for (; nentries > 0; nentries--, entries++) {
	cb[0].callback = (XtCallbackProc) entries->function;
	cb[0].closure = (caddr_t) entries->name;
	entries->widget = XtCreateManagedWidget (entries->name, 
						 (entries->function ?
						  smeBSBObjectClass :
						  smeLineObjectClass), m,
						 &arg, (Cardinal) 1);
    }

    /* do not realize at this point */
    return m;
}

init_menu (menu)
    char *menu;
{
    register TScreen *screen = &term->screen;
    register int i;

    switch (menu[0]) {
    case 'f':
	screen->fontMenu = NULL;
	for (i=0;  i < XtNumber(fontMenuEntries);  i++)
	    fontMenuEntries[i].widget = NULL;
	break;
    case 'm':
	screen->mainMenu = NULL;
	for (i=0;  i < XtNumber(mainMenuEntries);  i++)
	    mainMenuEntries[i].widget = NULL;
	break;
    case 'v':
	screen->vtMenu = NULL;
	for (i=0;  i < XtNumber(vtMenuEntries);  i++)
	    vtMenuEntries[i].widget = NULL;
	break;
    case 't':
	screen->tekMenu = NULL;
	for (i=0;  i < XtNumber(tekMenuEntries);  i++)
	    tekMenuEntries[i].widget = NULL;
	break;
    }
}

/* ARGSUSED */
static void handle_send_signal (gw, sig)
    Widget gw;
    int sig;
{
    register TScreen *screen = &term->screen;

    if (screen->pid > 1) kill_process_group (screen->pid, sig);
}


/*
 * action routines
 */

/* ARGSUSED */
void DoSecureKeyboard (time)
    Time time;
{
    do_securekbd (term->screen.mainMenu, NULL, NULL);
}

static void do_securekbd (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;
    Time time = CurrentTime;		/* XXX - wrong */

    if (screen->grabbedKbd) {
	XUngrabKeyboard (screen->display, time);
	ReverseVideo (term);
	screen->grabbedKbd = FALSE;
    } else {
	if (XGrabKeyboard (screen->display, term->core.window,
			   True, GrabModeAsync, GrabModeAsync, time)
	    != GrabSuccess) {
	    XBell (screen->display, 100);
	} else {
	    ReverseVideo (term);
	    screen->grabbedKbd = TRUE;
	}
    }
    update_securekbd();
}


static void do_allowsends (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    /*
    screen->allowSendEvents = !screen->allowSendEvents;
    update_allowsends ();
     */
}

static void do_visualbell (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->visualbell = !screen->visualbell;
    update_visualbell();
}

#ifdef ALLOWLOGGING
static void do_logging (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    if (screen->logging) {
	CloseLog (screen);
    } else {
	StartLog (screen);
    }
    /* update_logging done by CloseLog and StartLog */
}
#endif

static void do_redraw (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    Redraw ();
}


/*
 * The following cases use the pid instead of the process group so that we
 * don't get hosed by programs that change their process group
 */


/* ARGSUSED */
static void do_suspend (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
#ifdef SIGTSTP
    handle_send_signal (gw, SIGTSTP);
#endif
}

/* ARGSUSED */
static void do_continue (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
#ifdef SIGCONT
    handle_send_signal (gw, SIGCONT);
#endif
}

/* ARGSUSED */
static void do_interrupt (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_send_signal (gw, SIGINT);
}

/* ARGSUSED */
void do_hangup (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_send_signal (gw, SIGHUP);
}

/* ARGSUSED */
static void do_terminate (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_send_signal (gw, SIGTERM);
}

/* ARGSUSED */
static void do_kill (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_send_signal (gw, SIGKILL);
}

static void do_quit (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    Cleanup (0);
}



/*
 * vt menu callbacks
 */

static void do_scrollbar (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    if (screen->scrollbar) {
	ScrollBarOff (screen);
    } else {
	ScrollBarOn (term, FALSE, FALSE);
    }
    update_scrollbar();
}


static void do_jumpscroll (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    term->flags ^= SMOOTHSCROLL;
    if (term->flags & SMOOTHSCROLL) {
	screen->jumpscroll = FALSE;
	if (screen->scroll_amt) FlushScroll(screen);
    } else {
	screen->jumpscroll = TRUE;
    }
    update_jumpscroll();
}


static void do_reversevideo (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->flags ^= REVERSE_VIDEO;
    ReverseVideo (term);
    /* update_reversevideo done in ReverseVideo */
}


static void do_colortext (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->misc.dynamicColors = !term->misc.dynamicColors;
    update_colortext ();
    Redraw ();
}


static void do_autowrap (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->flags ^= WRAPAROUND;
    update_autowrap();
}


static void do_reversewrap (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->flags ^= REVERSEWRAP;
    update_reversewrap();
}


static void do_autolinefeed (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->flags ^= LINEFEED;
    update_autolinefeed();
}


static void do_appcursor (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->keyboard.flags ^= CURSOR_APL;
    update_appcursor();
}


static void do_appkeypad (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    term->keyboard.flags ^= KYPD_APL;
    update_appkeypad();
}


static void do_scrollkey (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->scrollkey = !screen->scrollkey;
    update_scrollkey();
}


static void do_scrollttyoutput (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->scrollttyoutput = !screen->scrollttyoutput;
    update_scrollttyoutput();
}


static void do_allow132 (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->c132 = !screen->c132;
    update_allow132();
}


static void do_cursesemul (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->curses = !screen->curses;
    update_cursesemul();
}


static void do_marginbell (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    if (!(screen->marginbell = !screen->marginbell)) screen->bellarmed = -1;
    update_marginbell();
}


static void do_altscreen (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    /* do nothing for now; eventually, will want to flip screen */
}


static void do_softreset (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    VTReset (FALSE);
}


static void do_hardreset (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    gt_reset();
    VTReset (TRUE);		/* does a longjmp */
}


static void do_clearsavedlines (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    screen->savedlines = 0;
    ScrollBarDrawThumb(screen->scrollWidget);
    VTReset (TRUE); 		/* does a longjmp */
}

/* ARGSUSED */
static void do_gioenable (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    gt_enable (!gt_enable(2));
    update_gioenable();
}

/* ARGSUSED */
static void do_tekmode (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    switch_modes (gt_tekmode(2));	/* switch to tek mode */
}

static void handle_tekshow (gw, allowswitch)
    Widget gw;
    Bool allowswitch;
{
    register TScreen *screen = &term->screen;

    if (!gt_activated()) {               /* not showing, turn on */
        set_tek_visibility (TRUE);
    } else if (screen->Vshow || allowswitch) {  /* is showing, turn off */
        set_tek_visibility (FALSE);
    } else
	Bell();
}

/* ARGSUSED */
static void do_tekshow (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_tekshow (gw, True);
}

/* ARGSUSED */
static void do_tekonoff (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_tekshow (gw, False);
}


/* ARGSUSED */
static void do_vthide (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    hide_vt_window();
}


/*
 * vtfont menu
 */

static void do_vtfont (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    char *entryname = (char *) closure;
    int i;

    for (i = 0; i < NMENUFONTS; i++) {
	if (strcmp (entryname, fontMenuEntries[i].name) == 0) {
	    SetVTFont (i, True, NULL, NULL);
	    return;
	}
    }
    Bell();
}


/*
 * tek menu
 */


static void do_tekpage (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    gt_clear();
}


static void do_tekreset (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    gt_reset();
    set_vthide_sensitivity();
    set_tekhide_sensitivity();
    set_tekreset_sensitivity();
    update_vttekmode();
    update_tekshow();
    update_tekreset();
    update_vtshow();
}


static void do_tekcopy (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    /* TekCopy (); */
}


static void handle_vtshow (gw, allowswitch)
    Widget gw;
    Bool allowswitch;
{
    register TScreen *screen = &term->screen;

    if (!screen->Vshow) {		/* not showing, turn on */
	set_vt_visibility (TRUE);
    } else if (gt_activated() || allowswitch) {  /* is showing, turn off */
	set_vt_visibility (FALSE);
	end_vt_mode ();
    } else 
	Bell();
}

static void do_vtshow (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_vtshow (gw, True);
}

static void do_vtonoff (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    handle_vtshow (gw, False);
}

static void do_vtmode (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    register TScreen *screen = &term->screen;

    switch_modes (gt_activated());	/* switch to vt, or from */
}


/* ARGSUSED */
static void do_tekhide (gw, closure, data)
    Widget gw;
    caddr_t closure, data;
{
    gt_deactivate();
    set_vthide_sensitivity();
    set_tekhide_sensitivity();
    set_tekreset_sensitivity();
    update_vttekmode();
    update_tekshow();
    update_tekreset();
    update_vtshow();
}



/*
 * public handler routines
 */

static void handle_toggle (proc, var, params, nparams, w, closure, data)
    void (*proc)();
    int var;
    String *params;
    Cardinal nparams;
    Widget w;
    caddr_t closure, data;
{
    int dir = -2;

    switch (nparams) {
      case 0:
	dir = -1;
	break;
      case 1:
	if (XmuCompareISOLatin1 (params[0], "on") == 0) dir = 1;
	else if (XmuCompareISOLatin1 (params[0], "off") == 0) dir = 0;
	else if (XmuCompareISOLatin1 (params[0], "toggle") == 0) dir = -1;
	break;
    }

    switch (dir) {
      case -2:
	Bell();
	break;

      case -1:
	(*proc) (w, closure, data);
	break;

      case 0:
	if (var) (*proc) (w, closure, data);
	else Bell();
	break;

      case 1:
	if (!var) (*proc) (w, closure, data);
	else Bell();
	break;
    }
    return;
}

void HandleAllowSends(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    /* Currently since we use Xt event processing we cannot easily disable
     * sendevents as is done in xevent().
     *
    handle_toggle (do_allowsends, (int) term->screen.allowSendEvents,
		   params, *param_count, w, NULL, NULL);
     */
}

void HandleSetVisualBell(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_visualbell, (int) term->screen.visualbell,
		   params, *param_count, w, NULL, NULL);
}

#ifdef ALLOWLOGGING
void HandleLogging(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_logging, (int) term->screen.logging,
		   params, *param_count, w, NULL, NULL);
}
#endif

/* ARGSUSED */
void HandleRedraw(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_redraw(w, NULL, NULL);
}

/* ARGSUSED */
void HandleSendSignal(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;
    Cardinal *param_count;
{
    static struct sigtab {
	char *name;
	int sig;
    } signals[] = {
#ifdef SIGTSTP
	{ "suspend",	SIGTSTP },
	{ "tstp",	SIGTSTP },
#endif
#ifdef SIGCONT
	{ "cont",	SIGCONT },
#endif
	{ "int",	SIGINT },
	{ "hup",	SIGHUP },
	{ "quit",	SIGQUIT },
	{ "alrm",	SIGALRM },
	{ "alarm",	SIGALRM },
	{ "term",	SIGTERM },
	{ "kill",	SIGKILL },
	{ NULL, 0 },
    };

    if (*param_count == 1) {
	struct sigtab *st;

	for (st = signals; st->name; st++) {
	    if (XmuCompareISOLatin1 (st->name, params[0]) == 0) {
		handle_send_signal (w, st->sig);
		return;
	    }
	}
	/* one could allow numeric values, but that would be a security hole */
    }

    Bell();
}

/* ARGSUSED */
void HandleQuit(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_quit(w, NULL, NULL);
}

void HandleScrollbar(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_scrollbar, (int) term->screen.scrollbar,
		   params, *param_count, w, NULL, NULL);
}

void HandleJumpscroll(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_jumpscroll, (int) term->screen.jumpscroll,
		   params, *param_count, w, NULL, NULL);
}

void HandleReverseVideo(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_reversevideo, (int) (term->flags & REVERSE_VIDEO),
		   params, *param_count, w, NULL, NULL);
}

void HandleAutoWrap(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_autowrap, (int) (term->flags & WRAPAROUND),
		   params, *param_count, w, NULL, NULL);
}

void HandleReverseWrap(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_reversewrap, (int) (term->flags & REVERSEWRAP),
		   params, *param_count, w, NULL, NULL);
}

void HandleAutoLineFeed(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_autolinefeed, (int) (term->flags & LINEFEED),
		   params, *param_count, w, NULL, NULL);
}

void HandleAppCursor(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_appcursor, (int) (term->keyboard.flags & CURSOR_APL),
		   params, *param_count, w, NULL, NULL);
}

void HandleAppKeypad(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_appkeypad, (int) (term->keyboard.flags & KYPD_APL),
		   params, *param_count, w, NULL, NULL);
}

void HandleScrollKey(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_scrollkey, (int) term->screen.scrollkey,
		   params, *param_count, w, NULL, NULL);
}

void HandleScrollTtyOutput(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_scrollttyoutput, (int) term->screen.scrollttyoutput,
		   params, *param_count, w, NULL, NULL);
}

void HandleAllow132(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_allow132, (int) term->screen.c132,
		   params, *param_count, w, NULL, NULL);
}

void HandleCursesEmul(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_cursesemul, (int) term->screen.curses,
		   params, *param_count, w, NULL, NULL);
}

void HandleMarginBell(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    handle_toggle (do_marginbell, (int) term->screen.marginbell,
		   params, *param_count, w, NULL, NULL);
}

void HandleAltScreen(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    /* eventually want to see if sensitive or not */
    handle_toggle (do_altscreen, (int) term->screen.alternate,
		   params, *param_count, w, NULL, NULL);
}

/* ARGSUSED */
void HandleSoftReset(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_softreset(w, NULL, NULL);
}

/* ARGSUSED */
void HandleHardReset(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_hardreset(w, NULL, NULL);
}

/* ARGSUSED */
void HandleClearSavedLines(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_clearsavedlines(w, NULL, NULL);
}

void HandleSetTerminalType(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    if (*param_count == 1) {
	switch (params[0][0]) {
	  case 'v': case 'V':
	    if (gt_activated()) do_vtmode (w, NULL, NULL);
	    break;
	  case 't': case 'T':
	    if (!gt_activated()) do_tekmode (w, NULL, NULL);
	    break;
	  default:
	    Bell();
	}
    } else {
	Bell();
    }
}

void HandleVisibility(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    if (*param_count == 2) {
	switch (params[0][0]) {
	  case 'v': case 'V':
	    handle_toggle (do_vtonoff, (int) term->screen.Vshow,
			   params+1, (*param_count) - 1, w, NULL, NULL);
	    break;
	  case 't': case 'T':
	    handle_toggle (do_tekonoff, (int) gt_activated(),
			   params+1, (*param_count) - 1, w, NULL, NULL);
	    break;
	  default:
	    Bell();
	}
    } else {
	Bell();
    }
}

/* ARGSUSED */
void HandleTekPage(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_tekpage(w, NULL, NULL);
}

/* ARGSUSED */
void HandleTekReset(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_tekreset(w, NULL, NULL);
}

/* ARGSUSED */
void HandleTekCopy(w, event, params, param_count)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *param_count;
{
    do_tekcopy(w, NULL, NULL);
}
