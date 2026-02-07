/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 *
 * LAPACK wrapper
 * Author: Linbo He <Linbo_he@outlook.com>
 * Original Auther: Ole Streicher
 * Date: April 10, 2025
 * 
 * This file is a wrapper for LAPACK functions used in IRAF. Because IRAF SPP
 * is based on FORTRAN, is ILP64 and assumes char to be 2 bytes, to make it
 * compatible with LAPACK (which usually built with LP64 and assumes char to
 * be 1 byte), a wrapper is needed to convert the pointers.
 * 
 * This file is based on the matrix library written in SPP rewritten by Ole
 * Streicher to avoid the known license issues with Numerical Recipes codes.
 */

#define import_kproto
#define import_knames
#define import_spp
#include <iraf.h>
#include <stdlib.h>
#include <string.h>

/* Alias for SPP procedures */
#define LUDCMP  ludcmp_
#define LUDCMD  ludcmd_
#define LUBKSB  lubksb_
#define LUBKSD  lubksd_
#define LUMINV  luminv_

/* Alias for LAPACK functions */
#define SGETRF sgetrf_
#define SGETRS sgetrs_
#define DGETRF dgetrf_
#define DGETRS dgetrs_

/* Apple Accelerate */
#if defined(USE_SYSTEM_BLAS) && defined(__APPLE__) && defined(__MACH__) && defined(USE_APPLE_ACCELERATE)
#include <Availability.h>
#ifndef __MAC_13_3 // Accelerate does not provide ILP64 interfaces until macOS 13.3
#ifdef IRAF_USE_ILP64
#error "Accelerate framework does not support ILP64 interfaces until macOS 13.3 or later. Please consider using other BLAS/LAPACK implementations or upgrading your macOS version."
#endif
#define FORTRAN_RET int
#else
#define ACCELERATE_NEW_LAPACK
#ifdef IRAF_USE_ILP64
#define ACCELERATE_LAPACK_ILP64
#endif
#endif
#include <Accelerate/Accelerate.h>
#else
#ifndef USE_SYSTEM_BLAS
#define FORTRAN_RET int
#else
#define FORTRAN_RET void
#endif

/* Declaring LAPACK functions */
extern FORTRAN_RET  SGETRF (const XINT* m, const XINT* n, XREAL* a, const XINT* lda, XINT* ipiv,
                    XINT* info);
extern FORTRAN_RET  SGETRS (const char* trans, const XINT* n, const XINT* nrhs, XREAL* a, const XINT* lda,
                    const XINT* ipiv, XREAL* b, const XINT* ldb, XINT* info);
extern FORTRAN_RET  DGETRF (const XINT* m, const XINT* n, XDOUBLE* a, const XINT* lda, XINT* ipiv,
                    XINT* info);
extern FORTRAN_RET  DGETRS (const char* trans, const XINT* n, const XINT* nrhs, XDOUBLE* a, const XINT* lda,
                    const XINT* ipiv, XDOUBLE* b, const XINT* ldb, XINT* info);
#endif

/* Consts */
static const XINT c__1 = 1;
static const char c__N = 'N';

/* ----------------------------------------------------------------------------
 * LU Decomposition
 * ----------------------------------------------------------------------------
 */

/* Given an N x N matrix A, with physical dimension N, this routine
 * replaces it by the LU decomposition of a rowwise permutation of
 * itself.  A and N are input.  A is output, arranged as in equation
 * (2.3.14) above; INDX is an output vector which records the row
 * permutation effected by the partial pivioting; D is output as +/-1
 * depending on whether the number of row interchanges was even or odd,
 * respectively.  This routine is used in combination with LUBKSB to
 * solve linear equations or invert a matrix.
 *
 * This routine simply calls the LU decomposition routine provided by LAPACK.
 */

int LUDCMP(XREAL* a, XINT* n, XINT* np, XINT* indx, XREAL* d)
/* List of arguments:
 *   real    a[np,np]        # io: input a, output decomposed a
 *   int     n               # i: logical size of a is n x n
 *   int     np              # i: space allocated for a
 *   int     indx[n]         # o: index to be used by xt_lubksb
 *   real    d               # o: +1 or -1
 */
{
    XINT istat;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
    *d = 1.0;
    SGETRF(N, N, a, NP, INDX, &istat);
    ZZEPRO();
    return 0;
}

/* ludcmd -- lower-upper decomposition
 * Double-precision version of LUDCMP
 *
 * This routine decomposes a matrix (in-place) into lower and upper
 * triangular portions.  Use lubksd to obtain a solution to A * X = B
 * or to compute the inverse of the matrix A.
 * If the matrix is singular, ISTAT is set to one.
 *
 * This routine simply calls the LU decomposition routine provided by LAPACK.
 */

int LUDCMD (XDOUBLE* a, XINT* n, XINT* np, XINT* indx, XDOUBLE* d, XINT* istat)
/* List of arguments:
 *   double  a[np,np]        # io: input a, output decomposed a
 *   int     n               # i: logical size of a is n x n
 *   int     np              # i: space allocated for a
 *   int     indx[n]         # o: index to be used by xt_lubksb
 *   double  d               # o: +1 or -1
 *   int     istat           # o: OK if no problem; 1 if matrix is singular
 */
{
    XINT* ISTAT = istat;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
    *d = 1.0;
    DGETRF(N, N, a, NP, INDX, ISTAT);
    if (*istat > 0) {
        *istat = 1;
    }
    ZZEPRO();
    return 0;
}

/* Solves the set of N linear equations AX = B.  Here A is input, not
 * as the matrix of A but rather as its LU decomposition, determined by
 * the routine LUDCMP.  INDX is input as the permuation vector returned
 * by LUDCMP.  B is input as the right-hand side vector B, and returns
 * with the solution vector X.  A, N, NP and INDX are not modified by
 * this routine and can be left in place for successive calls with
 * different right-hand sides B.  This routine takes into account the
 * possiblity that B will begin with many zero elements, so it is
 * efficient for use in matrix inversion.
 *
 * This routine simply calls the LU decomposition routine provided by LAPACK.
 */

int LUBKSB (XREAL* a, XINT* n, XINT* np, XINT* indx, XREAL* b)
/* List of arguments:
 *   real    a[np,np]        # i: matrix returned by ludcmp
 *   int     n               # i: logical size of a is n x n
 *   int     np              # i: space allocated for a is np x np
 *   int     indx[n]         # i: index returned by ludcmp
 *   real    b[n]            # io: input b, output x in equation ax = b
 */
{
    XINT status;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
    XINT  c__1 = 1;
    SGETRS(&c__N, N, &c__1, a, NP, INDX, b, N, &status);
    ZZEPRO();
    return 0;
}

/* Double-precision version of LUBKSB.
 *
 * Solves the set of N linear equations AX = B.  Here A is input, not
 * as the matrix of A but rather as its LU decomposition, determined by
 * the routine LUDCMP.  INDX is input as the permuation vector returned
 * by LUDCMP.  B is input as the right-hand side vector B, and returns
 * with the solution vector X.  A, N, NP and INDX are not modified by
 * this routine and can be left in place for successive calls with
 * different right-hand sides B.  This routine takes into account the
 * possiblity that B will begin with many zero elements, so it is
 * efficient for use in matrix inversion.
 *
 * This routine simply calls the LU decomposition routine provided by LAPACK.
 */

int LUBKSD (XDOUBLE* a, XINT* n, XINT* np, XINT* indx, XDOUBLE* b)
/* List of arguments:
 *   double  a[np,np]        # i: matrix returned by ludcmd
 *   int     n               # i: logical size of a is n x n
 *   int     np              # i: space allocated for a is np x np
 *   int     indx[n]         # i: index returned by ludcmd
 *   double  b[n]            # io: input b, output x in equation ax = b
 */
{
    XINT  status;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
    DGETRS(&c__N, N, &c__1, a, NP, INDX, b, N, &status);
    ZZEPRO();
    return 0;
}

/* Invert a matrix using LU decomposition using A as both input and output.
 */

int LUMINV (XREAL* a, XINT* n, XINT* np)
/* List of arguments:
 *   real    a[np,np]        # io: input a, output inverse a
 *   int     n               # i: logical size of a is n x n
 *   int     np              # i: space allocated for a
 */
{
float* y = malloc((*n) * (*n) * sizeof(float));
memset(y, 0, (*n) * (*n) * sizeof(float));
for (int i = 0; i < *n; i++) {
    y[i*(*n+1)] = 1.0;
}
    XINT* ipiv = malloc(*n * sizeof(XINT));
    XINT* N = n;
    XINT* NP = np;
    XINT  istat;
    SGETRF(N, N, a, NP, ipiv, &istat);
    SGETRS(&c__N, N, N, a, NP, ipiv, y, N, &istat);
    free(y);
    ZZEPRO();
    return 0;
}
