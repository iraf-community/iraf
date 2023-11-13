/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#include <math.h>

float lgammaf (float x);


/* GAMMLN -- Return natural log of gamma function.
 * Argument must greater than 0.
 */

float gammln_ (float *x)
{
        return lgammaf (*x);
}
