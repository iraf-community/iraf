/* pdmcursor.x -- translated by f2c (version 20050501).
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

/* Common Block Declarations */

struct {
    doublereal memd[1];
} mem_;

#define mem_1 mem_

struct {
    logical xerflg, xerpad[84];
} xercom_;

#define xercom_1 xercom_

/* Table of constant values */

static integer c__1023 = 1023;
static integer c__2 = 2;
static integer c__7 = 7;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__4 = 4;
static integer c__3 = 3;

/* Subroutine */ int pdmcur_(pdmp, ptype, infile__, flip)
integer *pdmp, *ptype;
shortint *infile__;
logical *flip;
{
    /* Initialized data */

    static shortint st0003[12] = { 112,100,109,32,111,112,116,105,111,110,115,
	    0 };
    static shortint st0004[5] = { 120,109,105,110,0 };
    static shortint st0005[5] = { 120,109,97,120,0 };
    static shortint st0006[7] = { 99,117,114,115,111,114,0 };
    static shortint st0007[26] = { 67,97,110,39,116,32,102,105,116,32,97,32,
	    84,72,69,84,65,32,112,108,111,116,32,7,10,0 };
    static shortint st0008[41] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,111,110,32,115,99,114,101,101,110,
	    32,102,111,114,32,112,32,107,101,121,7,10,0 };
    static shortint st0009[26] = { 70,105,116,32,104,97,115,32,110,111,116,32,
	    98,101,101,110,32,100,111,110,101,46,32,7,10,0 };
    static shortint st0010[28] = { 65,108,114,101,97,100,121,32,117,115,105,
	    110,103,32,114,101,115,105,100,117,97,108,115,46,32,7,10,0 };
    static shortint st0011[2] = { 42,0 };
    static shortint st0012[8] = { 97,103,97,105,110,58,10,0 };
    static shortint st0013[7] = { 99,117,114,115,111,114,0 };
    static shortint st0014[7] = { 32,37,103,58,37,103,0 };
    static shortint st0015[3] = { 32,10,0 };
    static shortint st0016[32] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,115,32,107,101,121,
	    32,7,10,0 };
    static shortint st0017[2] = { 42,0 };
    static shortint st0018[29] = { 83,105,103,110,105,102,105,99,97,110,99,
	    101,32,97,116,32,99,117,114,115,111,114,32,61,32,37,103,10,0 };
    static shortint st0019[29] = { 83,105,103,110,105,102,105,99,97,110,99,
	    101,32,97,116,32,99,117,114,115,111,114,32,61,32,37,103,10,0 };
    static shortint st0020[34] = { 83,105,103,110,105,102,105,99,97,110,99,
	    101,32,111,102,32,116,104,105,115,32,112,101,114,105,111,100,32,
	    61,32,37,103,10,0 };
    static shortint st0021[32] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,103,32,107,101,121,
	    32,7,10,0 };
    static shortint st0022[36] = { 97,109,112,108,105,116,117,100,101,32,111,
	    102,32,100,97,116,97,32,61,32,37,103,44,32,101,112,111,99,104,32,
	    61,32,37,103,10,0 };
    static shortint st0023[36] = { 97,109,112,108,105,116,117,100,101,32,111,
	    102,32,100,97,116,97,32,61,32,37,103,44,32,101,112,111,99,104,32,
	    61,32,37,103,10,0 };
    static shortint st0024[36] = { 97,109,112,108,105,116,117,100,101,32,111,
	    102,32,100,97,116,97,32,61,32,37,103,44,32,101,112,111,99,104,32,
	    61,32,37,103,10,0 };
    static shortint st0025[32] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,97,32,107,101,121,32,
	    7,10,0 };
    static shortint st0026[13] = { 109,105,110,112,32,110,111,119,32,37,103,
	    10,0 };
    static shortint st0027[13] = { 109,105,110,102,32,110,111,119,32,37,103,
	    10,0 };
    static shortint st0028[32] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,44,32,107,101,121,32,
	    7,10,0 };
    static shortint st0029[13] = { 109,97,120,112,32,110,111,119,32,37,103,10,
	    0 };
    static shortint st0030[13] = { 109,97,120,102,32,110,111,119,32,37,103,10,
	    0 };
    static shortint st0031[32] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,46,32,107,101,121,32,
	    7,10,0 };
    static shortint st0032[8] = { 97,103,97,105,110,58,10,0 };
    static shortint st0033[7] = { 99,117,114,115,111,114,0 };
    static shortint st0034[40] = { 112,101,114,105,111,100,32,97,116,32,109,
	    105,110,105,109,117,109,32,61,32,37,103,44,32,102,114,101,113,117,
	    101,110,99,121,32,61,32,37,103,10,0 };
    static shortint st0035[33] = { 87,114,111,110,103,32,116,121,112,101,32,
	    111,102,32,112,108,111,116,32,102,111,114,32,109,32,107,101,121,
	    46,32,7,10,0 };
    static shortint st0036[17] = { 101,114,114,111,114,32,105,110,32,100,99,
	    118,102,105,116,10,0 };
    static shortint st0037[28] = { 65,108,114,101,97,100,121,32,117,115,105,
	    110,103,32,114,101,115,105,100,117,97,108,115,46,32,7,10,0 };
    static shortint st0038[3] = { 7,10,0 };
    static shortint st0039[25] = { 63,32,102,111,114,32,104,101,108,112,32,
	    111,114,32,40,104,44,102,44,105,44,107,44,112,0 };
    static shortint st0040[27] = { 44,100,44,117,44,106,44,115,44,116,44,103,
	    44,97,44,109,44,114,44,120,44,113,44,58,41,10,0 };
    static shortint st0001[7] = { 99,117,114,115,111,114,0 };
    static shortint st0002[21] = { 110,111,97,111,36,108,105,98,47,115,99,114,
	    47,112,100,109,46,107,101,121,0 };

    /* System generated locals */
    integer i__1;
    real r__1;
    doublereal d__1;

    /* Local variables */
    static integer i__;
    static doublereal x, y;
    static integer sp;
    static real xx, yy;
    static doublereal rx1, rx2;
    static integer ier, key, wcs;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
#define meml ((integer *)&mem_1)
    static integer sw0001;
#define mems ((shortint *)&mem_1)
#define memr ((real *)&mem_1)
#define memx ((complex *)&mem_1)
    static doublereal xmin, xmax;
    extern /* Subroutine */ int alimd_(), pargd_(), sfree_();
    static integer index;
    extern /* Subroutine */ int smark_(), icgfid_(), gpagee_();
    extern integer pdmdee_();
    extern /* Subroutine */ int pdmala_();
    static integer commad;
    extern /* Subroutine */ int pdmfie_(), dcvfre_();
    extern integer pdmfin_();
    extern /* Subroutine */ int salloc_();
    extern integer clgcur_();
    static doublereal signif;
    extern /* Subroutine */ int pdmcon_();
    static doublereal period;
    extern doublereal dcvevl_(), pdmsif_();
    static integer weighs;
    extern integer pdmune_();
    extern /* Subroutine */ int pdmdpt_(), pdmmip_(), pdmamp_(), pdmphe_(), 
	    rggxmd_(), gflush_(), dcvint_(), sprinf_(), dcvfit_(), eprinf_(), 
	    pdmppt_(), erract_(), icputr_(), xprinf_();
    extern integer strids_();
    static integer sptemp;
    extern /* Subroutine */ int pdmtpt_(), xffluh_();
    extern integer gqvery_();
    extern /* Subroutine */ int xstrct_(), zzepro_(), xstrcy_();

    /* Parameter adjustments */
    --infile__;

    /* Function Body */
    smark_(&sp);
    salloc_(&commad, &c__1023, &c__2);
L110:
    if (! (clgcur_(st0001, &xx, &yy, &wcs, &key, &memc[commad - 1], &c__1023) 
	    != -2)) {
	goto L111;
    }
    x = (doublereal) xx;
    y = (doublereal) yy;
    sw0001 = key;
    goto L120;
L130:
    gpagee_(&memi[*pdmp + 23], st0002, st0003);
    goto L121;
L140:
    pdmcon_(pdmp, &memc[commad - 1], ptype, &infile__[1], &period, flip);
    if (xercom_1.xerflg) {
	goto L100;
    }
    goto L121;
L150:
    pdmdpt_(pdmp, &infile__[1], flip);
    *ptype = 0;
    goto L121;
L160:
    if (! (*ptype == 0)) {
	goto L170;
    }
    alimd_(&mem_1.memd[memi[*pdmp + 27] - 1], &memi[*pdmp + 40], &xmin, &xmax)
	    ;
    r__1 = (real) xmin;
    icputr_(&memi[*pdmp + 19], st0004, &r__1);
    r__1 = (real) xmax;
    icputr_(&memi[*pdmp + 19], st0005, &r__1);
    smark_(&sptemp);
    salloc_(&weighs, &memi[*pdmp + 40], &c__7);
    i__1 = memi[*pdmp + 40];
    for (i__ = 1; i__ <= i__1; ++i__) {
	mem_1.memd[weighs + i__ - 2] = (doublereal) memi[memi[*pdmp + 30] + 
		i__ - 2];
/* L180: */
    }
/* L181: */
    if (! (memi[*pdmp + 40] >= 2)) {
	goto L190;
    }
    icgfid_(&memi[*pdmp + 19], &memi[*pdmp + 23], st0006, &memi[*pdmp + 26], &
	    memi[*pdmp + 21], &mem_1.memd[memi[*pdmp + 27] - 1], &mem_1.memd[
	    memi[*pdmp + 29] - 1], &mem_1.memd[weighs - 1], &memi[*pdmp + 40])
	    ;
L190:
    i__1 = memi[*pdmp + 40];
    for (i__ = 1; i__ <= i__1; ++i__) {
	memi[memi[*pdmp + 30] + i__ - 2] = (integer) mem_1.memd[weighs + i__ 
		- 2];
/* L200: */
    }
/* L201: */
    sfree_(&sptemp);
    pdmdpt_(pdmp, &infile__[1], flip);
    sfree_(&sptemp);
    goto L171;
L170:
    if (! (*ptype == 3)) {
	goto L210;
    }
    pdmfie_(pdmp);
    if (xercom_1.xerflg) {
	goto L100;
    }
    pdmppt_(pdmp, &period, &infile__[1], flip);
    goto L211;
L210:
    xprinf_(st0007);
L211:
L171:
    goto L121;
L220:
    if (! (mem_1.memd[(*pdmp - 1) / 2] < 2.22e-16 && mem_1.memd[(*pdmp + 1) / 
	    2] < 2.22e-16)) {
	goto L230;
    }
    pdmmip_(pdmp);
L230:
    pdmala_(pdmp, &c__2);
    if (xercom_1.xerflg) {
	goto L100;
    }
    pdmtpt_(pdmp, &c__2, &infile__[1]);
    *ptype = 2;
    goto L121;
L240:
    if (! (mem_1.memd[(*pdmp - 1) / 2] < 2.22e-16 && mem_1.memd[(*pdmp + 1) / 
	    2] < 2.22e-16)) {
	goto L250;
    }
    pdmmip_(pdmp);
L250:
    pdmala_(pdmp, &c__1);
    if (xercom_1.xerflg) {
	goto L100;
    }
    pdmtpt_(pdmp, &c__1, &infile__[1]);
    *ptype = 1;
    goto L121;
L260:
    if (! (*ptype == 1)) {
	goto L270;
    }
    pdmamp_(pdmp, &x);
    if (xercom_1.xerflg) {
	goto L100;
    }
    pdmphe_(pdmp, &x, &mem_1.memd[(*pdmp + 17) / 2]);
    if (xercom_1.xerflg) {
	goto L100;
    }
    pdmppt_(pdmp, &x, &infile__[1], flip);
    *ptype = 3;
    period = x;
    goto L271;
L270:
    if (! (*ptype == 2)) {
	goto L280;
    }
    d__1 = 1. / x;
    pdmamp_(pdmp, &d__1);
    if (xercom_1.xerflg) {
	goto L100;
    }
    d__1 = 1. / x;
    pdmphe_(pdmp, &d__1, &mem_1.memd[(*pdmp + 17) / 2]);
    if (xercom_1.xerflg) {
	goto L100;
    }
    d__1 = 1. / x;
    pdmppt_(pdmp, &d__1, &infile__[1], flip);
    *ptype = 3;
    period = 1. / x;
    goto L281;
L280:
    xprinf_(st0008);
L281:
L271:
    goto L121;
L290:
    index = pdmdee_(pdmp, &x, &y, ptype);
    goto L121;
L300:
    index = pdmune_(pdmp, &x, &y, ptype);
    goto L121;
L310:
    if (! (memi[*pdmp + 21] == 0)) {
	goto L320;
    }
    xprinf_(st0009);
    goto L321;
L320:
    if (! (memi[*pdmp + 43] == 1)) {
	goto L330;
    }
    xprinf_(st0010);
    goto L331;
L330:
    i__1 = memi[*pdmp + 40];
    for (i__ = 1; i__ <= i__1; ++i__) {
	mem_1.memd[memi[*pdmp + 29] + i__ - 2] -= dcvevl_(&memi[*pdmp + 21], &
		mem_1.memd[memi[*pdmp + 27] + i__ - 2]);
/* L340: */
    }
/* L341: */
    memi[*pdmp + 43] = 1;
    if (! (*ptype == 0)) {
	goto L350;
    }
    pdmdpt_(pdmp, &infile__[1], flip);
L350:
L331:
L321:
    goto L121;
L360:
    if (! (*ptype == 0)) {
	goto L370;
    }
    if (! (strids_(st0011, &memc[memi[*pdmp + 38] - 1]) > 0)) {
	goto L380;
    }
    memc[memi[*pdmp + 38] - 1] = 0;
L380:
    rx1 = x;
    xprinf_(st0012);
    if (! (clgcur_(st0013, &xx, &yy, &wcs, &key, &memc[commad - 1], &c__1023) 
	    == -2)) {
	goto L390;
    }
    goto L111;
L390:
    rx2 = (doublereal) xx;
    sprinf_(&memc[commad - 1], &c__1023, st0014);
    pargd_(&rx1);
    pargd_(&rx2);
    xstrct_(&memc[commad - 1], &memc[memi[*pdmp + 38] - 1], &c__1023);
    rggxmd_(&memi[*pdmp + 23], &memc[memi[*pdmp + 38] - 1], &mem_1.memd[memi[*
	    pdmp + 27] - 1], &memi[*pdmp + 40], &c__1);
    xprinf_(st0015);
    gflush_(&memi[*pdmp + 23]);
    goto L371;
L370:
    xprinf_(st0016);
L371:
    goto L121;
L400:
    if (! (*ptype == 0)) {
	goto L410;
    }
    rggxmd_(&memi[*pdmp + 23], &memc[memi[*pdmp + 38] - 1], &mem_1.memd[memi[*
	    pdmp + 27] - 1], &memi[*pdmp + 40], &c__0);
L410:
    gflush_(&memi[*pdmp + 23]);
    xstrcy_(st0017, &memc[memi[*pdmp + 38] - 1], &c__1023);
    gflush_(&memi[*pdmp + 23]);
    goto L121;
L420:
    if (! (*ptype == 1)) {
	goto L430;
    }
    signif = pdmsif_(pdmp, &x);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0018);
    pargd_(&signif);
    goto L431;
L430:
    if (! (*ptype == 2)) {
	goto L440;
    }
    d__1 = 1. / x;
    signif = pdmsif_(pdmp, &d__1);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0019);
    pargd_(&signif);
    goto L441;
L440:
    if (! (*ptype == 3)) {
	goto L450;
    }
    signif = pdmsif_(pdmp, &period);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0020);
    pargd_(&signif);
    goto L451;
L450:
    xprinf_(st0021);
L451:
L441:
L431:
    goto L121;
L460:
    if (! (*ptype == 1)) {
	goto L470;
    }
    pdmamp_(pdmp, &x);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0022);
    pargd_(&mem_1.memd[(*pdmp + 15) / 2]);
    pargd_(&mem_1.memd[(*pdmp + 17) / 2]);
    goto L471;
L470:
    if (! (*ptype == 2)) {
	goto L480;
    }
    d__1 = 1. / x;
    pdmamp_(pdmp, &d__1);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0023);
    pargd_(&mem_1.memd[(*pdmp + 15) / 2]);
    pargd_(&mem_1.memd[(*pdmp + 17) / 2]);
    goto L481;
L480:
    if (! (*ptype == 3)) {
	goto L490;
    }
    pdmamp_(pdmp, &period);
    if (xercom_1.xerflg) {
	goto L100;
    }
    xprinf_(st0024);
    pargd_(&mem_1.memd[(*pdmp + 15) / 2]);
    pargd_(&mem_1.memd[(*pdmp + 17) / 2]);
    goto L491;
L490:
    xprinf_(st0025);
L491:
L481:
L471:
    goto L121;
L500:
    if (! (*ptype == 1)) {
	goto L510;
    }
    mem_1.memd[(*pdmp - 1) / 2] = x;
    mem_1.memd[(*pdmp + 3) / 2] = 1. / x;
    xprinf_(st0026);
    pargd_(&mem_1.memd[(*pdmp - 1) / 2]);
    goto L511;
L510:
    if (! (*ptype == 2)) {
	goto L520;
    }
    mem_1.memd[(*pdmp + 5) / 2] = x;
    mem_1.memd[(*pdmp + 1) / 2] = 1. / x;
    xprinf_(st0027);
    pargd_(&mem_1.memd[(*pdmp + 5) / 2]);
    goto L521;
L520:
    xprinf_(st0028);
L521:
L511:
    goto L121;
L530:
    if (! (*ptype == 1)) {
	goto L540;
    }
    mem_1.memd[(*pdmp + 1) / 2] = x;
    mem_1.memd[(*pdmp + 5) / 2] = 1. / x;
    xprinf_(st0029);
    pargd_(&mem_1.memd[(*pdmp + 1) / 2]);
    goto L541;
L540:
    if (! (*ptype == 2)) {
	goto L550;
    }
    mem_1.memd[(*pdmp + 3) / 2] = x;
    mem_1.memd[(*pdmp - 1) / 2] = 1. / x;
    xprinf_(st0030);
    pargd_(&mem_1.memd[(*pdmp + 3) / 2]);
    goto L551;
L550:
    xprinf_(st0031);
L551:
L541:
    goto L121;
L560:
    if (! (*ptype == 2 || *ptype == 1)) {
	goto L570;
    }
    rx1 = x;
    xprinf_(st0032);
    if (! (clgcur_(st0033, &xx, &yy, &wcs, &key, &memc[commad - 1], &c__1023) 
	    == -2)) {
	goto L580;
    }
    goto L111;
L580:
    rx2 = (doublereal) xx;
    index = pdmfin_(pdmp, ptype, &rx1, &rx2, &c__1, &memi[*pdmp + 41]);
    if (xercom_1.xerflg) {
	goto L100;
    }
    mem_1.memd[(*pdmp + 7) / 2] = mem_1.memd[memi[*pdmp + 32] + index - 2];
    xprinf_(st0034);
    pargd_(&mem_1.memd[memi[*pdmp + 32] + index - 2]);
    d__1 = 1. / mem_1.memd[memi[*pdmp + 32] + index - 2];
    pargd_(&d__1);
    goto L571;
L570:
    xprinf_(st0035);
L571:
    goto L121;
L590:
    if (! (*ptype == 0)) {
	goto L600;
    }
    pdmdpt_(pdmp, &infile__[1], flip);
L600:
    if (! (*ptype == 1)) {
	goto L610;
    }
    pdmtpt_(pdmp, &c__1, &infile__[1]);
L610:
    if (! (*ptype == 2)) {
	goto L620;
    }
    pdmtpt_(pdmp, &c__2, &infile__[1]);
L620:
    if (! (*ptype == 3)) {
	goto L630;
    }
    pdmppt_(pdmp, &period, &infile__[1], flip);
L630:
    goto L121;
L640:
    if (! (memi[*pdmp + 47] == 0)) {
	goto L650;
    }
    memi[*pdmp + 47] = 1;
    goto L651;
L650:
    memi[*pdmp + 47] = 0;
L651:
    if (! (*ptype == 3)) {
	goto L660;
    }
    pdmppt_(pdmp, &period, &infile__[1], flip);
L660:
    goto L121;
L670:
    alimd_(&mem_1.memd[memi[*pdmp + 27] - 1], &memi[*pdmp + 40], &xmin, &xmax)
	    ;
    dcvint_(&memi[*pdmp + 21], &c__4, &c__1, &xmin, &xmax);
    smark_(&sptemp);
    salloc_(&weighs, &memi[*pdmp + 40], &c__7);
    i__1 = memi[*pdmp + 40];
    for (i__ = 1; i__ <= i__1; ++i__) {
	mem_1.memd[weighs + i__ - 2] = (doublereal) memi[memi[*pdmp + 30] + 
		i__ - 2];
/* L680: */
    }
/* L681: */
    dcvfit_(&memi[*pdmp + 21], &mem_1.memd[memi[*pdmp + 27] - 1], &mem_1.memd[
	    memi[*pdmp + 29] - 1], &mem_1.memd[weighs - 1], &memi[*pdmp + 40],
	     &c__1, &ier);
    if (! (ier != 0)) {
	goto L690;
    }
    eprinf_(st0036);
    erract_(&c__3);
L690:
    if (! (memi[*pdmp + 43] == 1)) {
	goto L700;
    }
    xprinf_(st0037);
    goto L110;
L700:
    i__1 = memi[*pdmp + 40];
    for (i__ = 1; i__ <= i__1; ++i__) {
	mem_1.memd[memi[*pdmp + 29] + i__ - 2] -= dcvevl_(&memi[*pdmp + 21], &
		mem_1.memd[memi[*pdmp + 27] + i__ - 2]);
/* L710: */
    }
/* L711: */
    memi[*pdmp + 43] = 1;
/* L701: */
    dcvfre_(&memi[*pdmp + 21]);
    sfree_(&sptemp);
    pdmdpt_(pdmp, &infile__[1], flip);
    *ptype = 0;
    goto L121;
L720:
    *flip = ! (*flip);
    goto L121;
L730:
    if (! (gqvery_() == 1)) {
	goto L740;
    }
    goto L111;
L740:
    goto L121;
L750:
    xprinf_(st0038);
    xprinf_(st0039);
    xprinf_(st0040);
    goto L121;
L120:
    if (sw0001 == 44) {
	goto L500;
    }
    if (sw0001 == 46) {
	goto L530;
    }
    if (sw0001 == 58) {
	goto L140;
    }
    if (sw0001 == 63) {
	goto L130;
    }
    if (sw0001 == 97) {
	goto L460;
    }
    if (sw0001 == 100) {
	goto L290;
    }
    if (sw0001 == 101) {
	goto L640;
    }
    if (sw0001 == 102) {
	goto L160;
    }
    if (sw0001 == 103) {
	goto L420;
    }
    if (sw0001 == 104) {
	goto L150;
    }
    if (sw0001 == 105) {
	goto L220;
    }
    if (sw0001 == 106) {
	goto L310;
    }
    if (sw0001 == 107) {
	goto L240;
    }
    if (sw0001 == 109) {
	goto L560;
    }
    if (sw0001 == 112) {
	goto L260;
    }
    if (sw0001 == 113) {
	goto L730;
    }
    if (sw0001 == 114) {
	goto L590;
    }
    if (sw0001 == 115) {
	goto L360;
    }
    if (sw0001 == 116) {
	goto L400;
    }
    if (sw0001 == 117) {
	goto L300;
    }
    if (sw0001 == 120) {
	goto L670;
    }
    if (sw0001 == 122) {
	goto L720;
    }
    goto L750;
L121:
    goto L110;
L111:
    xffluh_(&c__4);
    sfree_(&sp);
L100:
    zzepro_();
    return 0;
} /* pdmcur_ */

#undef memx
#undef memr
#undef mems
#undef meml
#undef memi
#undef memc
#undef memb


