#ifndef _Converters_h
#define _Converters_h

Boolean XfwfCvtLongToString(
#if NeedFunctionPrototypes
    Display *display,
    XrmValuePtr args,
    Cardinal *num_args,
    XrmValuePtr from,
    XrmValuePtr to,
    XtPointer *converter_data
#endif
);

Boolean cvtStringToIcon(
#if NeedFunctionPrototypes
    Display *dpy,
    XrmValue *args,
    Cardinal *num_args,
    XrmValue *from,
    XrmValue *to,
    XtPointer *converter_data
#endif
);

#ifndef NO_XPM

#include <xpm.h>

/* The |Icon| type is a convenient combination of a pixmap, a mask and
the pixmaps's attributes. Not all attributes are stored, only width
and height. */

typedef struct _Icon {
    Pixmap pixmap;
    Pixmap mask;
    XpmAttributes attributes;
} Icon;

#endif

#endif /* _Converters_h */
