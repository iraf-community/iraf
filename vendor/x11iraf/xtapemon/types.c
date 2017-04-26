/*
 * types.c : Converters for SearchType and SortType, and an improved
 *	converter for Widget that allows "NULL" to be specified.
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Sep 1991.
 *
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Cardinals.h>
#include "types.h"
extern void XmuConvertStringToWidget();		/* original converter */

/*
 * Functions defined here:
 */
void initConverters();

void cvtStringToWidget();

/* Straight from the Xt manual... */
#define done(VALUE,TYPE)\
{							\
    if (toVal->addr != NULL) {				\
	if (toVal->size < sizeof(TYPE)) {		\
	    toVal->size = sizeof(TYPE);			\
	    return(False);				\
	}						\
	*(TYPE *)(toVal->addr) = (VALUE);		\
    } else {						\
	static TYPE static_val;				\
	static_val = (VALUE);				\
	toVal->addr = (XtPointer)&static_val;		\
    }							\
    toVal->size = sizeof(TYPE);				\
    return(True);					\
}
							
void
initConverters(appContext)
XtAppContext appContext;
{
    static XtConvertArgRec parentCvtArgs[] = {
        {XtBaseOffset, (caddr_t)XtOffset(Widget, core.parent), sizeof(Widget)}
    };

    /* Have to initialize Form class first or our converter will be	*/
    /* overidden by the class initialization function.			*/
    /* Use the old style here on purpose since that what the default is.*/
    XtInitializeWidgetClass(formWidgetClass);
    XtAppAddConverter(appContext,XtRString,XtRWidget,cvtStringToWidget,
					parentCvtArgs,XtNumber(parentCvtArgs));
}

/*
 * cvtStringToWidget() : Allows us to specify "NULL" as a widget name in
 *	a resource file to override compiled-in defaults for composite
 *	widget layouts. Simply calls the regular converter if the string
 *	is not "NULL". Note that this must be registered *after* the
 *	Form class is initialized.
 */
void
cvtStringToWidget(args,num_args,fromVal,toVal)
XrmValuePtr args;
Cardinal *num_args;
XrmValuePtr fromVal;
XrmValuePtr toVal;
{
    if (XmuCompareISOLatin1(fromVal->addr,"NULL") == 0) {
	toVal->addr = NULL;
	toVal->size = 0;
    } else {
	XmuCvtStringToWidget(args,num_args,fromVal,toVal);
    }
}
