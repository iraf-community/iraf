/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Copyright 2004 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

/*
 * An application should not include this header directly.  Instead it
 * should be included only through the inclusion of other Sun headers.
 *
 * The contents of this header is limited to identifiers specified in the
 * C Standard.  Any new identifiers specified in future amendments to the
 * C Standard must be placed in this header.  If these new identifiers
 * are required to also be in the C++ Standard "std" namespace, then for
 * anything other than macro definitions, corresponding "using" directives
 * must also be added to <stdarg.h>.
 */

#ifndef _ISO_STDARG_ISO_H
#define	_ISO_STDARG_ISO_H

#pragma ident	"@(#)stdarg_iso.h	1.4	04/11/19 SMI" /* SVr4.0 1.8 */

/*
 * This header defines the ISO C 1989 and ISO C++ 1998 variable
 * argument definitions.
 *
 * The varargs definitions within this header are defined in terms of
 * implementation definitions.  These implementation definitions reside
 * in <sys/va_impl.h>.  This organization enables protected use of
 * the implementation by other standard headers without introducing
 * names into the users' namespace.
 */

#include <sys/va_impl.h>

#ifdef	__cplusplus
extern "C" {
#endif

#if __cplusplus >= 199711L
namespace std {
typedef __va_list va_list;
}
#elif !defined(_VA_LIST)
#define	_VA_LIST
typedef __va_list va_list;
#endif

#define	va_start(list, name)	__va_start(list, name)
#define	va_arg(list, type)	__va_arg(list, type)
#define	va_end(list)		__va_end(list)

#ifdef	__cplusplus
}
#endif

#endif	/* _ISO_STDARG_ISO_H */
