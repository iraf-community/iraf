/* zz.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

longint gltoc_(longint *lval, shortint *outstr, longint *maxch, longint *base)
{
    /* zz System generated locals */
    shortint s__1, s__2, s__3;
    longint ret_val;

    /* Local variables */
    static longint d__, n, radix;

    /* Parameter adjustments */
    --outstr;

    /* Function Body */
    s__1 = (shortint) n;
    s__2 = (shortint) radix;
    d__ = (s__3 = s__1 % s__2, abs(s__3));
    return ret_val;
} /* gltoc_ */

