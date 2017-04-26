/*
  A converter from string to long is defined in the Xmu library, but
  the opposite direction is not. Here it is.

  Bert Bos <bert@let.rug.nl>
  13 Aug 1993
*/

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "Converters.h"
#include "done.h"


Boolean XfwfCvtLongToString(display, args, num_args, from, to, converter_data)
    Display *display;
    XrmValuePtr args;
    Cardinal *num_args;
    XrmValuePtr from;
    XrmValuePtr to;
    XtPointer *converter_data;
{
    char s[30];

    if (*num_args != 0)
	XtAppErrorMsg(XtDisplayToApplicationContext(display),
		      "cvtLongToString", "wrongParameters",
		      "XtToolkitError",
		      "long to String conversion needs no arguments",
		      (String*) NULL, (Cardinal*) NULL);
    sprintf(s, "%ld", *((long*) from->addr));
    done(String, s);
}

