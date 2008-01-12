/*
 * MATH.H -- Math functions for C.
 */

#ifndef D_math
#define	D_math

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
long int XNINT(double *);
double	XEXP(double *), XLOG(double *), XLOG10(double *);
double	XPOW(double *,double *), XSQRT(double *);
double	XSIN(double *), XCOS(double *), XASIN(double *), XACOS(double *);
double	XTAN(double *), XATAN(double *), XATAN2(double *,double *);

extern	double um_x, um_y;

#define nint(x)		XNINT((um_x=(x),&um_x))

#ifndef NOLIBCNAMES
#define	D_math_libcnames

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

#endif	/* NOLIBCNAMES */

#endif
