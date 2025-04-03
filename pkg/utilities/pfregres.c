/* pfregres.c
 * Rewritten by Linbo He <Linbo_He@outlook.com>
 * Rewritten date: April 3, 2025
 *
 * Rewritten in C from the original Fortran code
 * The orginal FORTRAN code was modified from regres.f
 * 
 * The rewitten version uses LAPACK to do matrix inversion
 *
 * source
 *   Bevington, pages 172-175.
 *
 * purpose
 *   make a mulitple linear regression fit to data with a specified
 *      function which is linear in coefficients
 *
 * usage
 *   call regres (x, y, sigmay, npts, nterms, m, mode, yfit,
 *      a0, a, sigma0, sigmaa, r, rmul, chisqr, ftest)
 *
 * description of parameters
 *   x	   - array of points for independent variable
 *   y	   - array of points for dependent variable
 *   sigmay - array of standard deviations for y data points
 *   npts   - number of pairs of data points
 *   nterms - number of coefficients
 *   m	   - array of inclusion/rejection criteria for fctn
 *   mode   - determines method of weighting least-squares fit
 *	     +1 (instrumental) weight(i) = 1./sigmay(i)**2
 *	      0 (no weighting) weight(i) = 1.
 *	     -1 (statistical)  weight(i) = 1./y(i)
 *   yfit   - array of calculated values of y
 *   a0	   - constant term
 *   a	   - array of coefficients
 *   sigma0 - standard deviation of a0
 *   sigmaa - array of standard deviations for coefficients
 *   r	   - array of linear correlation coefficients
 *   rmul   - multiple linear correlation coefficient
 *   chisqr - reduced chi square for fit
 *   ftest  - value of f for test of fit
 *
 * subroutines and function subprograms required
 *   fctn (x, i, j, m)
 *      evaluates the function for the jth term and the ith data point
 *      using the array m to specify terms in the function
 *   matinv (array, nterms, det)
 *      inverts a symmetric two-dimensional matrix of degree nterms
 *      and calculates its determinant
 *
 * comments
 * (dim npts changed 100->1000 21-may-84 dct)
 *   dimension statement valid for npts up to 100 and nterms up to 10
 *   sigmaag changed to sigmaa in statement following statement 132
 */
#define import_spp
#include <iraf.h>
#include <math.h>
#include <stdlib.h>

#define abs(x) ((x) < 0 ? -(x) : (x))
#define POW2(x) ((x)*(x))

extern int dgetrf_ (XINT* m, XINT* n, double* a, XINT* lda, XINT* ipiv, XINT* info);
extern int dgetri_ (XINT* n, double* a, XINT* lda, XINT* ipiv, double* work, XINT* lwork, XINT* info);
int
pfregs_ (
    float* x, float* y, float* sigmay,
    XINT* npts, XINT* nterms, XINT* m, XINT* mode,
    float* yfit, float* a0, float* a, float* sigma0, float* sigmaa,
    float* r, float* rmul, float* chisqr, float* ftest,
    float (*fctn)(float*, XINT*, XINT*, XINT*)
)
{
    /* Initiallize sums and arrays */
    double xmean[10], sigmax[10];
    double sum = 0., ymean = 0., sigma = 0., chisq = 0.;
    float weight[1000];

    double* array = (double*)malloc((*nterms) * (*nterms) * sizeof(double));

    *rmul = 0.;

    for (int i = 0; i < *npts; i++) {
        yfit[i] = 0.;
    }

    for (int i = 0; i < *nterms; i++) {
        xmean[i] = 0.;
        sigmax[i] = 0.;
        r[i] = 0.;
        a[i] = 0.;
        sigmaa[i] = 0.;
        for (int j =0; j < *nterms; j++) {
            array[i+j*(*nterms)] = 0.;
        }
    }

    /* Accumulate weighted sums */
    for (int i = 0; i < *npts; i++) {

        if (*mode == 0 || y[i] == 0) {
            weight[i] = 1.;
        } else if (*mode < 0) {
            weight[i] = 1. / abs(y[i]);
        } else {
            weight[i] = 1. / POW2(sigmay[i]);
        }

        sum += weight[i];
        ymean += weight[i] * y[i];

        for (int j = 0; j < *nterms; j++) {
            XINT I = i+1, J = j+1;
            xmean[j] += weight[i] * fctn(x,&I,&J,m);
        }
    }

    ymean /= sum;

    for (int i = 0; i < *nterms; i++) {
        xmean[i] /= sum;
    }

    float wmean = sum / (*npts);

    for (int i =0; i < *npts; i++) {
        weight[i] /= wmean;
    }

    /* Accumulate matrices r and array */
    for (int i = 0; i < *npts; i++) {
        sigma += weight[i] * (y[i] - ymean) * (y[i] - ymean);
        for (int j = 0; j < *nterms; j++) {
            XINT I = i+1, J = j+1;
            sigmax[j] += weight[i] * POW2(fctn(x,&I,&J,m) - xmean[j]);
            r[j] += weight[i] * (fctn(x,&I,&J,m) - xmean[j]) * (y[i] - ymean);
            for (int k = 0; k <= j; k++) {
                XINT K = k+1;
                array[j+k*(*nterms)] += weight[i] * (fctn(x,&I,&J,m) - xmean[j])
                    * (fctn(x, &I,&K,m) - xmean[k]);
            }
        }
    }

    float free1 = *npts - 1;
    sigma = sqrt(sigma / free1);

    for (int i = 0; i < *nterms; i++) {
        sigmax[i] = sqrt(sigmax[i] / free1);
        r[i] /= free1 * sigmax[i] * sigma;
        for (int j = 0; j <= i; j++) {
            array[i+j*(*nterms)] /= free1 * sigmax[i] * sigmax[j];
            array[j+i*(*nterms)] = array[i+j*(*nterms)];
        }
    }

    /* Invert symmetric matrix */
    XINT info;
    XINT* ipiv = (XINT*)malloc((*nterms)*sizeof(XINT));
    dgetrf_(nterms, nterms, array, nterms, ipiv, &info);
    if (info > 0) {
        free(ipiv);
        free(array);
        *a0 = 0.;
        *sigma0 = 0.;
        *rmul = 0.;
        *chisqr = 0.;
        *ftest = 0.;
        return 0;
    }
    XINT lwork = POW2(*nterms);
    double* work = (double*)malloc(lwork * sizeof(double));
    dgetri_(nterms, array, nterms, ipiv, work, &lwork, &info);
    free(work);
    free(ipiv);


    /* Calculate coefficients, fits, and chi square */
    *a0 = ymean;
    for (int i = 0; i < *nterms; i++) {
        for (int j = 0; j <= i; j++) {
            a[i] += r[j] * array[i+j*(*nterms)];
        }
        a[i] *= sigma / sigmax[i];
        *a0 -= a[i] * xmean[i];
        for (int j = 0; j < *npts; j++) {
            XINT I = i+1, J = j+1;
            yfit[j] += a[i] * fctn(x,&J,&I,m);
        }
    }
    for (int i = 0; i < *npts; i++) {
        yfit[i] += *a0;
        chisq += weight[i] * POW2(y[i] - yfit[i]);
    }
    float freen = *npts - *nterms - 1;
    *chisqr = chisq * wmean / freen;

    /* Calculate uncertainties */
    float varnce;
    if (*mode == 0) {
        varnce = *chisqr;
    } else {
        varnce = 1. / wmean;
    }

    for (int i = 0; i < *nterms; i++) {
        sigmaa[i] = varnce * array[i+i*(*nterms)] / (free1 * POW2(sigmax[i]));
        if (sigmaa[i] > 0) {
            sigmaa[i] = sqrt(sigmaa[i]);
        } else {
            sigmaa[i] = 0.;
        }
        *rmul += a[i] * r[i] * sigmax[i] / sigma;
    }

    /* +noao: When rmul = 1, the following division (stmt 135) would blow up.
     *        It has been changed so ftest is set to -99999. in this case.
     */
    if (*rmul < 0) {
        *ftest = -99999.;
        *rmul = -99999.;
    } else {
        if (abs(*rmul) < 1.) {
            *ftest = (*rmul / (*nterms)) / ((1. - *rmul) / freen); // stmt 135
            *rmul = sqrt(*rmul);
        } else if (abs(*rmul) == 1.) {
            *ftest = -99999;
            *rmul = 1.;
        } else {
            *ftest = -99999;
            *rmul = 99999.;
        }
    }
    /* -noao */

    *sigma0 = varnce / (*npts);

    for (int i = 0; i < *nterms; i++) {
        for (int j = 0; j < *nterms; j++) {
            *sigma0 += varnce * xmean[i] * xmean[j] * array[i + j*(*nterms)]
                / (free1 * sigmax[i] * sigmax[j]);
        }
    }

    *sigma0 = sqrt(*sigma0);
    free(array);
    return 0;
}