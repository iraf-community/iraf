/*
 * xarchie.h : Definitions of the X globals and misc. functions
 *
 * George Ferguson, ferguson@cs.rochester.edu, 4 Sep 1991.
 *
 */

#ifndef XARCHIE_H
#define XARCHIE_H

extern Display *display;
extern Screen *screen;
extern Window root;

extern XtAppContext appContext;
extern Widget toplevel;
extern Widget hostList,locationList,fileList;
extern Widget searchText;

extern void doPendingEvents();
extern void initWidgetsFromString();
extern void displayHostInfo(), clearHostInfo();
extern void displayLocationInfo(), clearLocationInfo();
extern void displayFileInfo(), clearFileInfo();
extern void setText(), setLabel();
extern void status0(), status1(), status2();
extern void fail0(), fail1();

#endif /* XARCHIE_H */
