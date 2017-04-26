/* $XConsortium: menu.h,v 1.24 93/02/25 17:21:31 gildea Exp $ */

/* Copyright 1989 Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

typedef struct _MenuEntry {
    char *name;
    void (*function)();
    Widget widget;
} MenuEntry;

extern MenuEntry mainMenuEntries[], vtMenuEntries[], tekMenuEntries[];
extern MenuEntry fontMenuEntries[];
extern Arg menuArgs[];

extern void HandleAllowSends();
extern void HandleSetVisualBell();
#ifdef ALLOWLOGGING
extern void HandleLogging();
#endif
extern void HandleRedraw();
extern void HandleSendSignal();
extern void HandleQuit();
extern void HandleScrollbar();
extern void HandleJumpscroll();
extern void HandleReverseVideo();
extern void HandleColorText();
extern void HandleAutoWrap();
extern void HandleReverseWrap();
extern void HandleAutoLineFeed();
extern void HandleAppCursor();
extern void HandleAppKeypad();
extern void HandleScrollKey();
extern void HandleScrollTtyOutput();
extern void HandleAllow132();
extern void HandleCursesEmul();
extern void HandleMarginBell();
extern void HandleAltScreen();
extern void HandleSoftReset();
extern void HandleHardReset();
extern void HandleClearSavedLines();
extern void HandleSetTerminalType();
extern void HandleVisibility();
extern void HandleTekPage();
extern void HandleTekReset();
extern void HandleTekCopy();
extern void DoSecureKeyboard();

/*
 * The following definitions MUST match the order of entries given in 
 * the mainMenuEntries, vtMenuEntries, and tekMenuEntries arrays in menu.c.
 */

/*
 * items in primary menu
 */
#define mainMenu_securekbd 	0
#define mainMenu_allowsends 	1
#ifdef ALLOWLOGGING
#define mainMenu_logging 	2
#endif
#define mainMenu_redraw 	3
#define mainMenu_line1 		4
#define mainMenu_suspend 	5
#define mainMenu_continue 	6
#define mainMenu_interrupt 	7
#define mainMenu_hangup 	8
#define mainMenu_terminate 	9
#define mainMenu_kill 		10
#define mainMenu_line2 		11
#define mainMenu_quit 		12

/*
 * items in vt100 mode menu
 */
#define vtMenu_scrollbar 	0
#define vtMenu_jumpscroll 	1
#define vtMenu_reversevideo 	2
#define vtMenu_colortext 	3
#define vtMenu_line1 		4
#define vtMenu_gioenable 	5
#define vtMenu_tekshow 		6
#define vtMenu_tekmode 		7
#define vtMenu_tekreset 	8
#define vtMenu_vthide 		9
#define vtMenu_line2 		10
#define vtMenu_autowrap 	11
#define vtMenu_reversewrap 	12
#define vtMenu_autolinefeed 	13
#define vtMenu_appcursor 	14
#define vtMenu_appkeypad 	15
#define vtMenu_scrollkey 	16
#define vtMenu_scrollttyoutput 	17
#define vtMenu_allow132 	18
#define vtMenu_cursesemul 	19
#define vtMenu_visualbell 	20
#define vtMenu_marginbell 	21
#define vtMenu_altscreen 	22
#define vtMenu_line3 		23
#define vtMenu_softreset 	24
#define vtMenu_hardreset 	25
#define vtMenu_clearsavedlines 	26

/*
 * items in vt100 font menu
 */
#define fontMenu_fontdefault 	0
#define fontMenu_font1 		1
#define fontMenu_font2 		2
#define fontMenu_font3 		3
#define fontMenu_font4 		4
#define fontMenu_font5 		5
#define fontMenu_font6 		6
#define fontMenu_lastBuiltin 	fontMenu_font6
#define fontMenu_fontescape 	7
#define fontMenu_fontsel 	8
/* number of non-line items should match NMENUFONTS in ptyx.h */



/*
 * items in tek4014 mode menu
 */
#define tekMenu_tekpage 	0
#define tekMenu_tekhide 	1
#define tekMenu_vtshow 		2
#define tekMenu_tekreset 	3


/*
 * macros for updating menus
 */

#define update_menu_item(w,mi,val) { if (mi) { \
    menuArgs[0].value = (XtArgVal) ( \
	(val) ? \
	    ((XtDisplay(w) == gtermio_display) ? \
		term->screen.tek_menu_item_bitmap \
		: term->screen.menu_item_bitmap) \
	    : None \
	); \
    XtSetValues (mi, menuArgs, (Cardinal) 1); }}


#define set_sensitivity(w,mi,val) { if (mi) { \
    menuArgs[1].value = (XtArgVal) (val); \
    XtSetValues (mi, menuArgs+1, (Cardinal) 1);  }}



/*
 * there should be one of each of the following for each checkable item
 */


#define update_securekbd() \
  update_menu_item (term->screen.mainMenu, \
		    mainMenuEntries[mainMenu_securekbd].widget, \
		    term->screen.grabbedKbd)

#define update_allowsends() \
  update_menu_item (term->screen.mainMenu, \
		    mainMenuEntries[mainMenu_allowsends].widget, \
		    term->screen.allowSendEvents)

#ifdef ALLOWLOGGING
#define update_logging() \
  update_menu_item (term->screen.mainMenu, \
		    mainMenuEntries[mainMenu_logging].widget, \
		    term->screen.logging)
#endif

#define update_scrollbar() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_scrollbar].widget, \
		    term->screen.scrollbar)

#define update_jumpscroll() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_jumpscroll].widget, \
		    term->screen.jumpscroll)

#define update_reversevideo() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_reversevideo].widget, \
		    (term->flags & REVERSE_VIDEO))

#define update_colortext() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_colortext].widget, \
		    term->misc.dynamicColors)

#define update_autowrap() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_autowrap].widget, \
		    (term->flags & WRAPAROUND))

#define update_reversewrap() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_reversewrap].widget, \
		    (term->flags & REVERSEWRAP))

#define update_autolinefeed() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_autolinefeed].widget, \
		    (term->flags & LINEFEED))

#define update_appcursor() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_appcursor].widget, \
		    (term->keyboard.flags & CURSOR_APL))

#define update_appkeypad() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_appkeypad].widget, \
		    (term->keyboard.flags & KYPD_APL))

#define update_scrollkey() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_scrollkey].widget,  \
		    term->screen.scrollkey)

#define update_scrollttyoutput() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_scrollttyoutput].widget, \
		    term->screen.scrollttyoutput)

#define update_allow132() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_allow132].widget, \
		    term->screen.c132)
  
#define update_cursesemul() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_cursesemul].widget, \
		    term->screen.curses)

#define update_visualbell() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_visualbell].widget, \
		    term->screen.visualbell)

#define update_marginbell() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_marginbell].widget, \
		    term->screen.marginbell)

#define update_gioenable() \
  update_menu_item (term->screen.vtMenu, \
                    vtMenuEntries[vtMenu_gioenable].widget, \
		    gt_enable(2))

#define update_tekshow() \
  update_menu_item (term->screen.vtMenu, \
                    vtMenuEntries[vtMenu_tekshow].widget, \
		    gt_activated())

#define update_tekreset() \
  set_sensitivity (term->screen.vtMenu, \
                    vtMenuEntries[vtMenu_tekreset].widget, \
		    gt_status())

#define update_vttekmode() { \
  update_menu_item (term->screen.vtMenu, \
                    vtMenuEntries[vtMenu_tekmode].widget, \
		    gt_tekmode(2)); }

#define update_vtshow() \
  update_menu_item (term->screen.tekMenu, \
                    tekMenuEntries[tekMenu_vtshow].widget, \
                    term->screen.Vshow)

#define set_vthide_sensitivity() \
  set_sensitivity (term->screen.vtMenu, \
                    vtMenuEntries[vtMenu_vthide].widget, \
		    gt_activated())

#define set_tekhide_sensitivity() \
  set_sensitivity (term->screen.tekMenu, \
                    tekMenuEntries[tekMenu_tekhide].widget, \
                    term->screen.Vshow)

#define set_tekreset_sensitivity() \
  set_sensitivity (term->screen.tekMenu, \
                    tekMenuEntries[tekMenu_tekreset].widget, \
                    term->screen.Vshow)

#define update_altscreen() \
  update_menu_item (term->screen.vtMenu, \
		    vtMenuEntries[vtMenu_altscreen].widget, \
		    term->screen.alternate)

#define set_altscreen_sensitivity(val) \
  set_sensitivity (term->screen.vtMenu,\
		    vtMenuEntries[vtMenu_altscreen].widget, (val))

#define set_menu_font(val) \
  update_menu_item (term->screen.fontMenu, \
		    fontMenuEntries[term->screen.menu_font_number].widget, \
		    (val))
