#include <float.h>

#define import_spp
#define import_knames
#include <iraf.h>

/*  SINGLE-PRECISION MACHINE CONSTANTS
 *
 * R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
 * R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
 * R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
 * R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
 * R1MACH(5) = LOG10(B)
 *
 * With IEEE, one should get the following numbers:
 *
 * R1MACH(1) = 1.1754943508222875e-38
 * R1MACH(2) = 3.4028234663852886e+38
 * R1MACH(3) = 5.960464477539063e-08
 * R1MACH(4) = 1.1920928955078125e-07
 * R1MACH(5) = 3.010299956639812e-1
 */

XREAL
R1MACH(
  XINT *i
)
{
    double log10(double x);

    switch(*i) {
    case 1: return FLT_MIN;
    case 2: return FLT_MAX;
    case 3: return FLT_EPSILON/FLT_RADIX;
    case 4: return FLT_EPSILON;
    case 5: return log10(FLT_RADIX);
    default: return 0.0;
    }
}
