#define import_spp
#include <iraf.h>

/* GAMMLN -- Return natural log of gamma function.
 * Argument must greater than 0.
 */

XREAL gammln_ (XREAL *xx)
{
  float lgammaf(float);

  return lgammaf(*xx);
}
