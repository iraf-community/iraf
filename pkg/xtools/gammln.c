/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#include <math.h>

double lgamma (double x);
float lgammaf (float x);


/* GAMMLN -- Return natural log of gamma function.
 * Argument must greater than 0.
 */

float gammln_ (float *x)
{
        return lgammaf (*x);
}

double dgammn_ (double *x)
{
        return lgamma (*x);
}
