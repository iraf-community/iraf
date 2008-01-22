#ifndef	_IRAF_MATH_H
#define	_IRAF_MATH_H

/*
 * MATH.H -- Math functions for C.
 */

#include <iraf/spptypes.h>

#define	XNINT		xnint_
#define	XEXP		xexp_
#define	XLOG		xlog_
#define	XLOG10		xlog10_
#define	XPOW		xpow_
#define	XSQRT		xsqrt_
#define	XSIN		xsin_
#define	XCOS		xcos_
#define	XASIN		xasin_
#define	XACOS		xacos_
#define	XTAN		xtan_
#define	XATAN		xatan_
#define	XATAN2		xatan2_

/* sys/libc/mathf.f */
XINT XNINT(XDOUBLE *);
XDOUBLE	XEXP(XDOUBLE *), XLOG(XDOUBLE *), XLOG10(XDOUBLE *);
XDOUBLE	XPOW(XDOUBLE *,XDOUBLE *), XSQRT(XDOUBLE *);
XDOUBLE	XSIN(XDOUBLE *), XCOS(XDOUBLE *), XASIN(XDOUBLE *), XACOS(XDOUBLE *);
XDOUBLE	XTAN(XDOUBLE *), XATAN(XDOUBLE *), XATAN2(XDOUBLE *,XDOUBLE *);

extern	double um_x, um_y;

#define nint(x)		XNINT((um_x=(x),&um_x))

#ifndef NOLIBCNAMES
#define	_IRAF_MATH_LIBCNAMES

#define	exp(x)		XEXP((um_x=(x),&um_x))
#define	log(x)		XLOG((um_x=(x),&um_x))
#define	log10(x)	XLOG10((um_x=(x),&um_x))
#define	pow(x,y)	XPOW((um_x=(x),&um_x),(um_y=(y),&um_y))
#define	sqrt(x)		XSQRT((um_x=(x),&um_x))
#define	sin(x)		XSIN((um_x=(x),&um_x))
#define	cos(x)		XCOS((um_x=(x),&um_x))
#define	tan(x)		XTAN((um_x=(x),&um_x))
#define	asin(x)		XASIN((um_x=(x),&um_x))
#define	acos(x)		XACOS((um_x=(x),&um_x))
#define	atan(x)		XATAN((um_x=(x),&um_x))
#define	atan2(x,y)	XATAN2((um_x=(x),&um_x),(um_y=(y),&um_y))

#endif	/* ! NOLIBCNAMES */

#endif	/* ! _IRAF_MATH_H */
