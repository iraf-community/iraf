/*
 * MATH.H -- Math functions for C.
 */

int     XNINT(double *x);
double	XEXP(double *x), XLOG(double *x), XLOG10(double *x);
double  XPOW(double *x, double *y), XSQRT(double *x);
double	XSIN(double *x), XCOS(double *x), XASIN(double *x);
double  XACOS(double *x), XTAN(double *x), XATAN(double *x);
double  XATAN2(double *x, double *y);

static	double um_x, um_y;

#define nint(x)		XNINT((um_x=(x),&um_x))
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

#define	D_math
