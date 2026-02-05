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

/* Declaring LAPACK functions */
#ifdef USE_SYSTEM_BLAS
extern void SGETRF (int* m, int* n, float* a, int* lda, int* ipiv,
                    int* info);
extern void SGETRS (char* trans, int* n, int* nrhs, float* a, int* lda, 
                    int* ipiv, float* b, int* ldb, int* info);
extern void DGETRF (int* m, int* n, double* a, int* lda, int* ipiv,
                    int* info);
extern void DGETRS (char* trans, int* n, int* nrhs, double* a, int* lda, 
                    int* ipiv, double* b, int* ldb, int* info);
#else
extern int  SGETRF (XINT* m, XINT* n, XREAL* a, XINT* lda, XINT* ipiv,
                    XINT* info);
extern int  SGETRS (char* trans, XINT* n, XINT* nrhs, XREAL* a, XINT* lda,
                    XINT* ipiv, XREAL* b, XINT* ldb, XINT* info);
extern int  DGETRF (XINT* m, XINT* n, XDOUBLE* a, XINT* lda, XINT* ipiv,
                    XINT* info);
extern int  DGETRS (char* trans, XINT* n, XINT* nrhs, XDOUBLE* a, XINT* lda,
                    XINT* ipiv, XDOUBLE* b, XINT* ldb, XINT* info);
#endif

/* Consts */
#ifdef USE_SYSTEM_BLAS
static const int c__1 = 1;
#else
static const XINT c__1 = 1;
#endif

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
#ifdef USE_SYSTEM_BLAS
    int istat;
    int* N = malloc(sizeof(int));
    *N = *n;
    int* NP = malloc(sizeof(int));
    *NP = *np;
    int* INDX = malloc(*n * sizeof(int));
#else
    XINT istat;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
#endif

    *d = 1.0;
    SGETRF(N, N, a, NP, INDX, &istat);
#ifdef USE_SYSTEM_BLAS
    for (int i = 0; i < *n; i++) {
        indx[i] = INDX[i];
    }
    free(INDX);
    free(NP);
    free(N);
#endif
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
#ifdef USE_SYSTEM_BLAS
    int* ISTAT = malloc(sizeof(int));
    int* N = malloc(sizeof(int));
    *N = *n;
    int* NP = malloc(sizeof(int));
    *NP = *np;
    int* INDX = malloc(*n * sizeof(int));
#else
    XINT* ISTAT = istat;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
#endif
    *d = 1.0;
    DGETRF(N, N, a, NP, INDX, ISTAT);
#ifdef USE_SYSTEM_BLAS
    for (int i = 0; i < *n; i++) {
        indx[i] = INDX[i];
    }
    free(INDX);
    *istat = *ISTAT;
    free(ISTAT);
    free(N);
    free(NP);
#endif
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
#ifdef USE_SYSTEM_BLAS
    int status;
    int* N = malloc(sizeof(int));
    *N = *n;
    int* NP = malloc(sizeof(int));
    *NP = *np;
    int* INDX = malloc(*n * sizeof(int));
    for (int i = 0; i < *n; i++) {
        INDX[i] = indx[i];
    }
#else
    XINT status;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
    XINT  c__1 = 1;
#endif
    SGETRS(&c__N, N, &c__1, a, NP, INDX, b, N, &status);
#ifdef USE_SYSTEM_BLAS
    free (INDX);
    free (NP);
    free (N);
#endif
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
#ifdef USE_SYSTEM_BLAS
    int  status;
    int* N = malloc(sizeof(int));
    *N = *n;
    int* NP = malloc(sizeof(int));
    *NP = *np;
    int* INDX = malloc(*n * sizeof(int));
    for (int i = 0; i < *n; i++) {
        INDX[i] = indx[i];
    }
#else
    XINT  status;
    XINT* N = n;
    XINT* NP = np;
    XINT* INDX = indx;
#endif
    DGETRS(&c__N, N, &c__1, a, NP, INDX, b, N, &status);
#ifdef USE_SYSTEM_BLAS
    free (INDX);
    free (NP);
    free (N);
#endif
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
#ifdef USE_SYSTEM_BLAS
    int* ipiv = malloc(*n * sizeof(int));
    int* N = malloc(sizeof(int));
    *N = *n;
    int* NP = malloc(sizeof(int));
    *NP = *np;
    int istat;
#else
    XINT* ipiv = malloc(*n * sizeof(XINT));
    XINT* N = n;
    XINT* NP = np;
    XINT  istat;
#endif
    SGETRF(N, N, a, NP, ipiv, &istat);
    SGETRS(&c__N, N, N, a, NP, ipiv, y, N, &istat);
#ifdef USE_SYSTEM_BLAS
    free(ipiv);
    free(NP);
    free(N);
#endif
    free(y);
    ZZEPRO();
    return 0;
}
