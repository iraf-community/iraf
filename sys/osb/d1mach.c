#include <float.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* DOUBLE-PRECISION MACHINE CONSTANTS
 *
 * D1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
 * D1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
 * D1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
 * D1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
 * D1MACH(5) = LOG10(B)
 *
 * With IEEE, one should get the following numbers:
 *
 * D1MACH(1) = 2.2250738585072014e-308
 * D1MACH(2) = 1.7976931348623157e+308
 * D1MACH(3) = 1.1102230246251565e-16
 * D1MACH(4) = 2.2204460492503131e-16
 * D1MACH(5) = 3.010299956639812e-1
 */

XDOUBLE
D1MACH(
  XINT *i
)
{
    double log10(double x);

    switch(*i) {
    case 1: return DBL_MIN;
    case 2: return DBL_MAX;
    case 3: return DBL_EPSILON/FLT_RADIX;
    case 4: return DBL_EPSILON;
    case 5: return log10(FLT_RADIX);
    default: return 0.0;
    }
}
