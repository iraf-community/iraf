/* $XConsortium: XawInit.h,v 1.4 91/07/22 19:05:25 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef _XawInit_
#define _XawInit_

#if !(defined(__STDC__) && __STDC__) && !defined(__cplusplus) && !defined(c_plusplus) && !defined(FUNCPROTO) && !defined(XTFUNCPROTO) && !defined(XAWFUNCPROTO) && !(defined(NeedFunctionPrototypes) && NeedFunctionPrototypes)
#define Xraw_NO_PROTO
#else
#define Xraw_NEED_PROTO
#endif /* __STDC__ */


#ifdef  Xraw_NEED_PROTO
#define Xraw_PROTO(args) args
#else
#define Xraw_PROTO(args) ()
#endif

#define Xraw_VERSION  1
#define Xraw_REVISION 2

#define Xraw_3d        1
#define Xraw_SCROLLBAR 20
#define Xraw_TABLE     40
#define Xraw_SEPARATOR 60
#define Xraw_FRAME     80

#define streq(a,b) (XrmStringToQuark(a) == XrmStringToQuark(b))

#define FULL_WIDTH(w)  ((w)->core.width + ((w)->core.border_width << 1))
#define FULL_HEIGHT(w) ((w)->core.height + ((w)->core.border_width << 1))
#define WNULL          (Widget)NULL

/* called from ClassInit procs */
extern void XawInitializeWidgetSet Xraw_PROTO((void));

#if defined(XtSpecificationRelease) && XtSpecificationRelease < 5
#define XPointer XtPointer
#endif       
     
#endif /* _XawInit_ */
