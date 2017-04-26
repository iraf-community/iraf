/* lroff2html.x -- translated by f2c (version 20100827).
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
    integer rightn, leftmn, permrn, permln, inmagg, outmag, soflag, foflag, 
	    justiy, nls, lsindt, shnskp, ihnskp, ihindt, nhnskp, nhlevl[10];
    logical standd;
} lrfcom_;

#define lrfcom_1 lrfcom_

struct {
    logical xerflg, xerpad[84];
} xercom_;

#define xercom_1 xercom_

/* Table of constant values */

static integer c__2046 = 2046;
static integer c__2 = 2;
static integer c__1023 = 1023;
static integer c__255 = 255;
static integer c__256 = 256;
static integer c__9 = 9;
static integer c__0 = 0;
static integer c__10 = 10;
static integer c__1 = 1;
static logical c_true = TRUE_;
static integer c__3 = 3;

/* Subroutine */ int lroffl_(in, out, module, parstr, center, lsblok, sectin)
integer *in, *out;
shortint *module, *parstr, *center, *lsblok, *sectin;
{
    /* Initialized data */

    static shortint st0003[5] = { 60,80,62,10,0 };
    static shortint st0004[7] = { 94,46,104,101,108,112,0 };
    static shortint st0005[8] = { 60,47,80,82,69,62,10,0 };
    static shortint st0006[7] = { 60,80,82,69,62,10,0 };
    static shortint st0007[13] = { 60,47,68,68,62,10,60,47,68,76,62,10,0 };
    static shortint st0008[13] = { 60,47,68,68,62,10,60,47,68,76,62,10,0 };
    static shortint st0009[2] = { 124,0 };
    static shortint st0010[13] = { 60,47,68,68,62,10,60,47,68,76,62,10,0 };
    static shortint st0011[25] = { 60,67,69,78,84,69,82,62,37,115,60,47,67,69,
	    78,84,69,82,62,60,66,82,62,10,0 };
    static shortint st0012[6] = { 60,66,82,62,10,0 };
    static shortint st0013[6] = { 60,66,82,62,10,0 };
    static shortint st0014[6] = { 60,66,82,62,10,0 };
    static shortint st0015[13] = { 60,68,76,62,10,60,68,84,62,60,66,62,0 };
    static shortint st0016[22] = { 60,65,32,78,65,77,69,61,34,108,95,37,115,
	    34,62,37,115,60,47,65,62,0 };
    static shortint st0017[11] = { 60,47,66,62,60,47,68,84,62,10,0 };
    static shortint st0018[46] = { 60,33,32,83,101,99,61,37,115,32,76,101,118,
	    101,108,61,37,100,32,76,97,98,101,108,61,39,37,115,39,32,76,105,
	    110,101,61,39,37,115,39,62,10,60,68,68,62,0 };
    static shortint st0019[5] = { 78,111,110,101,0 };
    static shortint st0020[2] = { 32,0 };
    static shortint st0021[13] = { 60,47,68,68,62,10,60,47,68,76,62,10,0 };
    static shortint st0022[21] = { 60,65,32,72,82,69,70,61,34,37,115,34,62,37,
	    115,60,47,65,62,10,0 };
    static shortint st0023[19] = { 60,65,32,78,65,77,69,61,34,37,115,34,62,60,
	    47,65,62,10,0 };
    static shortint st0024[7] = { 60,80,82,69,62,10,0 };
    static shortint st0025[8] = { 60,47,80,82,69,62,10,0 };
    static shortint st0026[5] = { 39,37,115,39,0 };
    static shortint st0027[7] = { 60,47,85,76,62,10,0 };
    static shortint st0028[22] = { 60,33,32,69,110,100,83,101,99,116,105,111,
	    110,58,32,32,32,37,115,62,10,0 };
    static shortint st0029[32] = { 60,72,50,62,60,65,32,78,65,77,69,61,34,115,
	    95,37,115,34,62,37,115,60,47,65,62,60,47,72,50,62,10,0 };
    static shortint st0030[35] = { 60,72,50,62,60,65,32,78,65,77,69,61,34,115,
	    95,37,115,34,62,37,115,32,37,115,60,47,65,62,60,47,72,50,62,10,0 }
	    ;
    static shortint st0031[24] = { 60,33,32,66,101,103,105,110,83,101,99,116,
	    105,111,110,58,32,39,37,115,39,62,10,0 };
    static shortint st0032[6] = { 60,85,76,62,10,0 };
    static shortint st0033[3] = { 37,115,0 };
    static shortint st0034[30] = { 60,47,85,76,62,10,60,33,32,69,110,100,83,
	    101,99,116,105,111,110,58,32,32,32,32,37,115,62,10,10,0 };
    static shortint st0035[14] = { 60,33,32,67,111,110,116,101,110,116,115,58,
	    32,0 };
    static shortint st0036[4] = { 37,115,32,0 };
    static shortint st0037[5] = { 32,62,10,10,0 };
    static shortint st0038[17] = { 60,47,66,79,68,89,62,10,60,47,72,84,77,76,
	    62,10,0 };
    static shortint st0001[24] = { 60,84,73,84,76,69,62,37,115,60,47,84,73,84,
	    76,69,62,10,60,85,76,62,10,0 };
    static shortint st0002[2] = { 32,0 };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, ip, sp, cmd, arg;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
    static integer name__, ibuf;
#define memi ((integer *)&mem_1)
    static integer nsec;
#define meml ((integer *)&mem_1)
    static integer sw0001, sw0002;
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    static integer font, sptr;
    extern /* Subroutine */ int aclrc_(), pargi_(), sfree_();
    static integer level, unesc;
    extern /* Subroutine */ int smark_();
    static integer indend;
    extern integer lgetag_();
    extern /* Subroutine */ int lhesce_();
    static logical formad;
    extern integer getlie_(), lhfink_(), lhfinn_();
    extern /* Subroutine */ int salloc_(), lhmkne_();
    static integer lastle;
    static logical format, quitae, quitah;
    extern integer nextcd_(), strmah_();
    static integer lslevl;
    extern integer strids_();
    extern /* Subroutine */ int amovki_(), ungete_(), lhprog_(), fprinf_(), 
	    pargsr_(), lhsetl_(), sprinf_(), xffluh_(), zzepro_();
    extern integer xstrln_();
    extern /* Subroutine */ int xstrcy_();

    /* Parameter adjustments */
    --sectin;
    --lsblok;
    --center;
    --parstr;
    --module;

    /* Function Body */
    smark_(&sp);
    salloc_(&ibuf, &c__2046, &c__2);
    salloc_(&unesc, &c__2046, &c__2);
    salloc_(&name__, &c__1023, &c__2);
    salloc_(&level, &c__255, &c__2);
    salloc_(&sptr, &c__256, &c__9);
    aclrc_(&memc[ibuf - 1], &c__2046);
    aclrc_(&memc[name__ - 1], &c__1023);
    aclrc_(&memc[unesc - 1], &c__2046);
    aclrc_(&memc[level - 1], &c__255);
    lastle = 3;
    font = 1;
    indend = 1;
    nsec = 0;
    lslevl = 0;
    format = TRUE_;
    quitae = FALSE_;
    quitah = FALSE_;
    formad = FALSE_;
    amovki_(&c__0, lrfcom_1.nhlevl, &c__10);
L110:
    if (! (getlie_(in, &memc[ibuf - 1]) == -2)) {
	goto L120;
    }
    goto L98;
L120:
    ip = 1;
L130:
    if (! (memc[ibuf + ip - 2] == 32 || memc[ibuf + ip - 2] == 9)) {
	goto L132;
    }
/* L131: */
    ++ip;
    goto L130;
L132:
/* L111: */
    if (! (memc[ibuf + ip - 2] != 10)) {
	goto L110;
    }
/* L112: */
    ungete_(in, &memc[ibuf - 1]);
    if (! (memc[ibuf - 1] == 46)) {
	goto L140;
    }
    formad = TRUE_;
L140:
    if (! (sectin[1] != 0)) {
	goto L150;
    }
    if (! (lhfinn_(in, &formad, &sectin[1]) == -2)) {
	goto L160;
    }
    goto L98;
L160:
    goto L151;
L150:
    if (! (lsblok[1] != 0)) {
	goto L170;
    }
    if (! (lhfink_(in, &formad, &lsblok[1]) == -2)) {
	goto L180;
    }
    goto L98;
L180:
    quitae = TRUE_;
L170:
L151:
    lhprog_(out, &module[1], &parstr[1], &center[1]);
    fprinf_(out, st0001);
    if (! (lsblok[1] != 0)) {
	goto L190;
    }
    pargsr_(&lsblok[1]);
    goto L191;
L190:
    if (! (sectin[1] != 0)) {
	goto L200;
    }
    pargsr_(&sectin[1]);
    goto L201;
L200:
    if (! (module[1] != 0)) {
	goto L210;
    }
    pargsr_(&module[1]);
    goto L211;
L210:
    pargsr_(st0002);
L211:
L201:
L191:
L220:
    if (! (getlie_(in, &memc[ibuf - 1]) != -2)) {
	goto L221;
    }
    memc[ibuf + xstrln_(&memc[ibuf - 1]) - 2] = 0;
    xstrcy_(&memc[ibuf - 1], &memc[unesc - 1], &c__1023);
    lhesce_(&memc[ibuf - 1], &font, &format, &c__0, &c__1023);
    sw0001 = memc[ibuf - 1];
    goto L230;
L240:
    fprinf_(out, st0003);
    goto L231;
L250:
    if (! (strmah_(&memc[ibuf - 1], st0004) > 0)) {
	goto L260;
    }
    goto L220;
L260:
    ip = 1;
    lastle = 3;
    cmd = nextcd_(&memc[ibuf - 1], &ip);
L270:
    if (! (memc[ibuf + ip - 1] == 32 || memc[ibuf + ip - 1] == 9)) {
	goto L271;
    }
    ++ip;
    goto L270;
L271:
    sw0002 = cmd;
    goto L280;
L290:
    fprinf_(out, st0005);
    format = TRUE_;
    goto L281;
L300:
    fprinf_(out, st0006);
    format = FALSE_;
    goto L281;
L310:
    goto L220;
L320:
    goto L220;
L330:
    goto L220;
L340:
    if (! (lslevl > 0)) {
	goto L350;
    }
    fprinf_(out, st0007);
    lslevl = 0;
L350:
    lastle = 1;
    memc[level - 1] = 0;
    goto L220;
L360:
    if (! (lslevl > 0)) {
	goto L370;
    }
    fprinf_(out, st0008);
    lslevl = 0;
L370:
    lastle = 1;
    memc[level - 1] = 0;
    if (! quitah) {
	goto L380;
    }
    if (! (strids_(st0009, &sectin[1]) > 0)) {
	goto L390;
    }
    quitah = FALSE_;
    ungete_(in, &memc[ibuf - 1]);
    if (! (lhfinn_(in, &formad, &sectin[1]) == -2)) {
	goto L400;
    }
    goto L221;
L400:
    goto L391;
L390:
    goto L221;
L391:
L380:
    goto L220;
L410:
    if (! (lslevl > 0)) {
	goto L420;
    }
    fprinf_(out, st0010);
    lslevl = 0;
L420:
    i__1 = lgetag_(&memc[ibuf - 1], &ip, &c__1);
    lhsetl_(&i__1, &memc[level - 1]);
    lastle = 1;
    goto L220;
L430:
    if (! (getlie_(in, &memc[ibuf - 1]) == -2)) {
	goto L440;
    }
    goto L221;
L440:
    lhesce_(&memc[ibuf - 1], &font, &c_true, &c__0, &c__1023);
    fprinf_(out, st0011);
    pargsr_(&memc[ibuf - 1]);
/* L441: */
    goto L281;
L450:
    fprinf_(out, st0012);
    goto L281;
L460:
    arg = lgetag_(&memc[ibuf - 1], &ip, &c__1);
    fprinf_(out, st0013);
    i__ = 1;
L470:
    if (! (i__ < arg)) {
	goto L472;
    }
    fprinf_(out, st0014);
/* L471: */
    ++i__;
    goto L470;
L472:
    goto L281;
L480:
    goto L220;
L490:
    arg = lgetag_(&memc[ibuf - 1], &ip, &c__0);
    if (! (arg == 0)) {
	goto L500;
    }
    ip = 5;
L500:
    xstrcy_(&memc[ibuf + ip - 2], &memc[name__ - 1], &c__1023);
    i__ = 0;
L510:
    if (! (memc[name__ + i__ - 1] >= 65 && memc[name__ + i__ - 1] <= 90 || 
	    memc[name__ + i__ - 1] >= 97 && memc[name__ + i__ - 1] <= 122 || 
	    memc[name__ + i__ - 1] >= 48 && memc[name__ + i__ - 1] <= 57 || 
	    memc[name__ + i__ - 1] == 95)) {
	goto L512;
    }
/* L511: */
    ++i__;
    goto L510;
L512:
    memc[name__ + i__ - 1] = 0;
    memc[ibuf + ip + xstrln_(&memc[ibuf + ip - 1]) - 2] = 0;
    fprinf_(out, st0015);
    fprinf_(out, st0016);
    pargsr_(&memc[name__ - 1]);
    pargsr_(&memc[ibuf + ip - 2]);
    fprinf_(out, st0017);
    lhesce_(&memc[unesc + ip - 2], &font, &c_true, &c__1, &c__1023);
    memc[unesc + xstrln_(&memc[unesc - 1]) - 2] = 0;
    fprinf_(out, st0018);
    if (! (nsec > 0)) {
	goto L520;
    }
    pargsr_(&memc[memi[sptr + nsec - 2] - 1]);
    goto L521;
L520:
    pargsr_(st0019);
L521:
    pargi_(&lslevl);
    pargsr_(&memc[name__ - 1]);
    if (! (memc[unesc + ip - 2] == 10)) {
	goto L530;
    }
    pargsr_(st0020);
    goto L531;
L530:
    pargsr_(&memc[unesc + ip - 2]);
L531:
    ++lslevl;
    goto L281;
L540:
    fprinf_(out, st0021);
    --lslevl;
    if (! quitae) {
	goto L550;
    }
    goto L221;
L550:
    goto L281;
L560:
    memc[ibuf + ip + xstrln_(&memc[ibuf + ip - 1]) - 2] = 0;
    i__ = 0;
L570:
    if (memc[ibuf + ip - 1] == 32 || memc[ibuf + ip - 1] == 9) {
	goto L572;
    }
    memc[name__ + i__ - 1] = memc[ibuf + ip - 1];
    ++i__;
/* L571: */
    ++ip;
    goto L570;
L572:
    memc[name__ + i__ - 1] = 0;
    fprinf_(out, st0022);
    pargsr_(&memc[name__ - 1]);
    pargsr_(&memc[ibuf + ip]);
    goto L281;
L580:
    memc[ibuf + ip + xstrln_(&memc[ibuf + ip - 1]) - 2] = 0;
    fprinf_(out, st0023);
    pargsr_(&memc[ibuf + ip - 1]);
    goto L281;
L590:
    goto L220;
L600:
    goto L220;
L610:
    fprinf_(out, st0024);
    format = FALSE_;
    goto L281;
L620:
    fprinf_(out, st0025);
    format = TRUE_;
    goto L281;
L630:
    goto L221;
L280:
    if (sw0002 < 1 || sw0002 > 21) {
	goto L281;
    }
    switch ((int)sw0002) {
	case 1:  goto L290;
	case 2:  goto L300;
	case 3:  goto L310;
	case 4:  goto L320;
	case 5:  goto L330;
	case 6:  goto L340;
	case 7:  goto L360;
	case 8:  goto L410;
	case 9:  goto L450;
	case 10:  goto L430;
	case 11:  goto L460;
	case 12:  goto L480;
	case 13:  goto L490;
	case 14:  goto L540;
	case 15:  goto L590;
	case 16:  goto L600;
	case 17:  goto L610;
	case 18:  goto L620;
	case 19:  goto L560;
	case 20:  goto L580;
	case 21:  goto L630;
    }
L281:
    goto L231;
L640:
    if (! (lastle == 1)) {
	goto L650;
    }
    salloc_(&memi[sptr + nsec - 1], &c__1023, &c__2);
    aclrc_(&memc[memi[sptr + nsec - 1] - 1], &c__1023);
    memc[ibuf + xstrln_(&memc[ibuf - 1]) - 2] = 0;
    sprinf_(&memc[memi[sptr + nsec - 1] - 1], &c__1023, st0026);
    pargsr_(&memc[ibuf - 1]);
    if (! (indend == 1)) {
	goto L660;
    }
    fprinf_(out, st0027);
L660:
    if (! (nsec > 0)) {
	goto L670;
    }
    fprinf_(out, st0028);
    pargsr_(&memc[memi[sptr + nsec - 2] - 1]);
L670:
    lhmkne_(&memc[ibuf - 1], &memc[name__ - 1]);
    if (! (memc[level - 1] == 0)) {
	goto L680;
    }
    fprinf_(out, st0029);
    pargsr_(&memc[name__ - 1]);
    pargsr_(&memc[ibuf - 1]);
    goto L681;
L680:
    fprinf_(out, st0030);
    pargsr_(&memc[name__ - 1]);
    pargsr_(&memc[level - 1]);
    pargsr_(&memc[ibuf - 1]);
    memc[level - 1] = 0;
L681:
    fprinf_(out, st0031);
    pargsr_(&memc[ibuf - 1]);
    if (! (indend == 1)) {
	goto L690;
    }
    fprinf_(out, st0032);
L690:
    lastle = 2;
    ++nsec;
    if (! (sectin[1] != 0)) {
	goto L700;
    }
    quitah = TRUE_;
L700:
    goto L651;
L650:
/* L99: */
    fprinf_(out, st0033);
    pargsr_(&memc[ibuf - 1]);
    lastle = 3;
L651:
    goto L231;
L230:
    if (sw0001 == 10) {
	goto L240;
    }
    if (sw0001 == 46) {
	goto L250;
    }
    goto L640;
L231:
    aclrc_(&memc[ibuf - 1], &c__2046);
    aclrc_(&memc[unesc - 1], &c__2046);
    aclrc_(&memc[name__ - 1], &c__1023);
    goto L220;
L221:
    if (! (nsec > 0)) {
	goto L710;
    }
    fprinf_(out, st0034);
    pargsr_(&memc[memi[sptr + nsec - 2] - 1]);
L710:
    fprinf_(out, st0035);
    i__ = 0;
L720:
    if (! (i__ < nsec)) {
	goto L722;
    }
    fprinf_(out, st0036);
    pargsr_(&memc[memi[sptr + i__ - 1] - 1]);
/* L721: */
    ++i__;
    goto L720;
L722:
    fprinf_(out, st0037);
    fprinf_(out, st0038);
    xffluh_(out);
L98:
    sfree_(&sp);
/* L100: */
    zzepro_();
    return 0;
} /* lroffl_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


/* Subroutine */ int lhprog_(fd, mod, date, title)
integer *fd;
shortint *mod, *date, *title;
{
    /* Initialized data */

    static shortint st0001[15] = { 60,72,84,77,76,62,10,60,66,79,68,89,62,10,
	    0 };
    static shortint st0002[36] = { 60,84,65,66,76,69,32,87,73,68,84,72,61,34,
	    49,48,48,37,37,34,32,66,79,82,68,69,82,61,48,62,60,84,82,62,10,0 }
	    ;
    static shortint st0003[30] = { 60,84,68,32,65,76,73,71,78,61,76,69,70,84,
	    62,60,70,79,78,84,32,83,73,90,69,61,52,62,10,0 };
    static shortint st0004[10] = { 60,66,62,37,115,60,47,66,62,0 };
    static shortint st0005[15] = { 60,66,62,37,115,32,40,37,115,41,60,47,66,
	    62,0 };
    static shortint st0006[14] = { 60,47,70,79,78,84,62,60,47,84,68,62,10,0 };
    static shortint st0007[32] = { 60,84,68,32,65,76,73,71,78,61,67,69,78,84,
	    69,82,62,60,70,79,78,84,32,83,73,90,69,61,52,62,10,0 };
    static shortint st0008[11] = { 60,66,62,37,115,60,47,66,62,10,0 };
    static shortint st0009[14] = { 60,47,70,79,78,84,62,60,47,84,68,62,10,0 };
    static shortint st0010[31] = { 60,84,68,32,65,76,73,71,78,61,82,73,71,72,
	    84,62,60,70,79,78,84,32,83,73,90,69,61,52,62,10,0 };
    static shortint st0011[10] = { 60,66,62,37,115,60,47,66,62,0 };
    static shortint st0012[15] = { 60,66,62,37,115,32,40,37,115,41,60,47,66,
	    62,0 };
    static shortint st0013[14] = { 60,47,70,79,78,84,62,60,47,84,68,62,10,0 };
    static shortint st0014[18] = { 60,47,84,82,62,60,47,84,65,66,76,69,62,60,
	    80,62,10,0 };

    extern /* Subroutine */ int fprinf_(), pargsr_(), zzepro_();

    /* Parameter adjustments */
    --title;
    --date;
    --mod;

    /* Function Body */
    fprinf_(fd, st0001);
    if (! (date[1] == 0 && title[1] == 0)) {
	goto L110;
    }
    goto L100;
L110:
    fprinf_(fd, st0002);
    fprinf_(fd, st0003);
    if (! (date[1] == 0)) {
	goto L120;
    }
    fprinf_(fd, st0004);
    pargsr_(&mod[1]);
    goto L121;
L120:
    fprinf_(fd, st0005);
    pargsr_(&mod[1]);
    pargsr_(&date[1]);
L121:
    fprinf_(fd, st0006);
    if (! (title[1] != 0)) {
	goto L130;
    }
    fprinf_(fd, st0007);
    fprinf_(fd, st0008);
    pargsr_(&title[1]);
    fprinf_(fd, st0009);
L130:
    fprinf_(fd, st0010);
    if (! (date[1] == 0)) {
	goto L140;
    }
    fprinf_(fd, st0011);
    pargsr_(&mod[1]);
    goto L141;
L140:
    fprinf_(fd, st0012);
    pargsr_(&mod[1]);
    pargsr_(&date[1]);
L141:
    fprinf_(fd, st0013);
    fprinf_(fd, st0014);
L100:
    zzepro_();
    return 0;
} /* lhprog_ */

/* Subroutine */ int lhesce_(str, font, format, speciy, maxch)
shortint *str;
integer *font;
logical *format;
integer *speciy, *maxch;
{
    /* Initialized data */

    static shortint st0013[5] = { 60,47,66,62,0 };
    static shortint st0014[4] = { 60,73,62,0 };
    static shortint st0015[5] = { 60,47,66,62,0 };
    static shortint st0016[5] = { 60,47,73,62,0 };
    static shortint st0017[5] = { 60,47,66,62,0 };
    static shortint st0018[5] = { 60,47,73,62,0 };
    static shortint st0019[5] = { 60,66,82,62,0 };
    static shortint st0020[3] = { 10,0,0 };
    static shortint st0001[4] = { 60,62,38,0 };
    static shortint st0002[5] = { 38,108,116,59,0 };
    static shortint st0003[5] = { 38,103,116,59,0 };
    static shortint st0004[6] = { 38,97,109,112,59,0 };
    static shortint st0005[5] = { 60,84,84,62,0 };
    static shortint st0006[6] = { 60,47,84,84,62,0 };
    static shortint st0007[5] = { 60,84,84,62,0 };
    static shortint st0008[6] = { 60,47,84,84,62,0 };
    static shortint st0009[7] = { 60,47,84,84,62,34,0 };
    static shortint st0010[6] = { 34,60,84,84,62,0 };
    static shortint st0011[5] = { 60,47,73,62,0 };
    static shortint st0012[4] = { 60,66,62,0 };

    /* Local variables */
    static integer i__, ip, sp, buf;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
#define meml ((integer *)&mem_1)
    static integer sw0001;
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    extern /* Subroutine */ int aclrc_(), sfree_(), amovc_(), smark_(), 
	    salloc_();
    static integer keywod;
    extern integer gstrcy_(), stridx_();
    extern /* Subroutine */ int zzepro_();

    /* Parameter adjustments */
    --str;

    /* Function Body */
    smark_(&sp);
    salloc_(&buf, maxch, &c__2);
    salloc_(&keywod, maxch, &c__2);
    aclrc_(&memc[buf - 1], maxch);
    aclrc_(&memc[keywod - 1], maxch);
    ip = buf;
    i__ = 1;
L110:
    if (! (str[i__] != 0 && i__ <= *maxch)) {
	goto L112;
    }
    if (! (*speciy == 1 && stridx_(&str[i__], st0001) == 0)) {
	goto L120;
    }
    goto L90;
L120:
    sw0001 = str[i__];
    goto L130;
L140:
    ip += gstrcy_(st0002, &memc[ip - 1], &c__1023);
    goto L131;
L150:
    ip += gstrcy_(st0003, &memc[ip - 1], &c__1023);
    goto L131;
L160:
    ip += gstrcy_(st0004, &memc[ip - 1], &c__1023);
    goto L131;
L170:
    if (! (str[i__ + 2] == 39)) {
	goto L180;
    }
    ip += gstrcy_(st0005, &memc[ip - 1], &c__1023);
    ip += gstrcy_(&str[i__], &memc[ip - 1], &c__3);
    ip += gstrcy_(st0006, &memc[ip - 1], &c__1023);
    i__ += 2;
    goto L181;
L180:
    goto L90;
L181:
    goto L131;
L190:
    if (! (str[i__ + 2] == 96 || str[i__ + 2] == 39)) {
	goto L200;
    }
    ip += gstrcy_(st0007, &memc[ip - 1], &c__1023);
    ip += gstrcy_(&str[i__], &memc[ip - 1], &c__3);
    ip += gstrcy_(st0008, &memc[ip - 1], &c__1023);
    i__ += 2;
    goto L201;
L200:
    goto L90;
L201:
    goto L131;
L210:
    if (! (*format && str[i__ + 1] != 47 && str[i__ + 2] != 47)) {
	goto L220;
    }
    if (! (*font == 5)) {
	goto L230;
    }
    ip += gstrcy_(st0009, &memc[ip - 1], &c__1023);
    *font = 1;
    goto L231;
L230:
    if (! (*font == 1)) {
	goto L240;
    }
    ip += gstrcy_(st0010, &memc[ip - 1], &c__1023);
    *font = 5;
    goto L241;
L240:
    goto L90;
L241:
L231:
    goto L221;
L220:
    goto L90;
L221:
    goto L131;
L250:
    if (! (str[i__ + 1] == 102)) {
	goto L260;
    }
    if (! (str[i__ + 2] == 66)) {
	goto L270;
    }
    if (! (*font == 3)) {
	goto L280;
    }
    goto L111;
L280:
    if (! (*font == 2)) {
	goto L290;
    }
    ip += gstrcy_(st0011, &memc[ip - 1], &c__1023);
L290:
    ip += gstrcy_(st0012, &memc[ip - 1], &c__1023);
    *font = 3;
    goto L271;
L270:
    if (! (str[i__ + 2] == 73)) {
	goto L300;
    }
    if (! (*font == 2)) {
	goto L310;
    }
    goto L111;
L310:
    if (! (*font == 3)) {
	goto L320;
    }
    ip += gstrcy_(st0013, &memc[ip - 1], &c__1023);
L320:
    ip += gstrcy_(st0014, &memc[ip - 1], &c__1023);
    *font = 2;
    goto L301;
L300:
    if (! (str[i__ + 2] == 82)) {
	goto L330;
    }
    if (! (*font == 3)) {
	goto L340;
    }
    ip += gstrcy_(st0015, &memc[ip - 1], &c__1023);
    goto L341;
L340:
    if (! (*font == 2)) {
	goto L350;
    }
    ip += gstrcy_(st0016, &memc[ip - 1], &c__1023);
L350:
L341:
    *font = 1;
    goto L331;
L330:
    if (! (str[i__ + 2] == 80)) {
	goto L360;
    }
    if (! (*font == 3)) {
	goto L370;
    }
    ip += gstrcy_(st0017, &memc[ip - 1], &c__1023);
    goto L371;
L370:
    if (! (*font == 2)) {
	goto L380;
    }
    ip += gstrcy_(st0018, &memc[ip - 1], &c__1023);
L380:
L371:
    *font = 1;
L360:
L331:
L301:
L271:
    i__ += 2;
    goto L261;
L260:
    if (! (str[i__ + 1] == 10 || str[i__ + 1] == 0)) {
	goto L390;
    }
    memc[ip - 1] = str[i__];
    ++ip;
    ++i__;
    ip += gstrcy_(st0019, &memc[ip - 1], &c__1023);
    goto L391;
L390:
    goto L90;
L391:
L261:
    goto L131;
L400:
L90:
    memc[ip - 1] = str[i__];
    ++ip;
    goto L131;
L130:
    if (sw0001 == 34) {
	goto L210;
    }
    if (sw0001 == 38) {
	goto L160;
    }
    if (sw0001 == 39) {
	goto L170;
    }
    if (sw0001 == 60) {
	goto L140;
    }
    if (sw0001 == 62) {
	goto L150;
    }
    if (sw0001 == 92) {
	goto L250;
    }
    if (sw0001 == 96) {
	goto L190;
    }
    goto L400;
L131:
L111:
    ++i__;
    goto L110;
L112:
    ip += gstrcy_(st0020, &memc[ip - 1], &c__1023);
    amovc_(&memc[buf - 1], &str[1], maxch);
    sfree_(&sp);
/* L100: */
    zzepro_();
    return 0;
} /* lhesce_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


/* Subroutine */ int lhsetl_(n, level)
integer *n;
shortint *level;
{
    /* Initialized data */

    static shortint st0001[4] = { 37,100,46,0 };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int pargi_(), amovki_(), sprinf_(), zzepro_();
    extern integer xstrln_();

    /* Parameter adjustments */
    --level;

    /* Function Body */
    ++lrfcom_1.nhlevl[(60 + (0 + (*n - 1 << 2)) - 60) / 4];
    i__1 = 10 - *n;
    amovki_(&c__0, &lrfcom_1.nhlevl[*n], &i__1);
    level[1] = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sprinf_(&level[xstrln_(&level[1]) + 1], &c__2046, st0001);
	pargi_(&lrfcom_1.nhlevl[i__ - 1]);
/* L110: */
    }
/* L111: */
    if (! (*n > 1 && level[xstrln_(&level[1])] == 46)) {
	goto L120;
    }
    level[xstrln_(&level[1])] = 0;
L120:
/* L100: */
    zzepro_();
    return 0;
} /* lhsetl_ */

integer lhfink_(fd, formad, param)
integer *fd;
logical *formad;
shortint *param;
{
    /* Initialized data */

    static shortint st0001[5] = { 123,37,115,125,0 };
    static shortint st0002[7] = { 94,46,123,108,115,125,0 };
    static shortint st0003[7] = { 94,35,123,37,115,125,0 };

    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer sp, len;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
    static integer lbuf;
#define meml ((integer *)&mem_1)
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    extern /* Subroutine */ int sfree_(), smark_();
    static logical matchd;
    extern integer getlie_();
    extern /* Subroutine */ int salloc_(), ungete_();
    static integer patten;
    extern integer strmah_();
    extern /* Subroutine */ int pargsr_(), sprinf_(), zzepro_();
    extern integer xstrln_();

    /* Parameter adjustments */
    --param;

    /* Function Body */
    smark_(&sp);
    salloc_(&patten, &c__255, &c__2);
    salloc_(&lbuf, &c__1023, &c__2);
    matchd = FALSE_;
    if (! (getlie_(fd, &memc[lbuf - 1]) == -2) && ! xercom_1.xerflg) {
	goto L110;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
    goto L90;
L110:
    if (! (*formad)) {
	goto L120;
    }
    sprinf_(&memc[patten - 1], &c__255, st0001);
    pargsr_(&param[1]);
L130:
    if (! (strmah_(&memc[lbuf - 1], st0002) > 0)) {
	goto L140;
    }
    if (! (strmah_(&memc[lbuf - 1], &memc[patten - 1]) > 0)) {
	goto L150;
    }
    matchd = TRUE_;
    goto L132;
L150:
L140:
/* L131: */
    if (! (getlie_(fd, &memc[lbuf - 1]) == -2) && ! xercom_1.xerflg) {
	goto L130;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
L132:
    goto L121;
L120:
    sprinf_(&memc[patten - 1], &c__255, st0003);
    pargsr_(&param[1]);
L160:
    if (! (strmah_(&memc[lbuf - 1], &memc[patten - 1]) > 0)) {
	goto L170;
    }
    matchd = TRUE_;
    goto L162;
L170:
/* L161: */
    if (! (getlie_(fd, &memc[lbuf - 1]) == -2) && ! xercom_1.xerflg) {
	goto L160;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
L162:
L121:
    ungete_(fd, &memc[lbuf - 1]);
L90:
    len = xstrln_(&memc[lbuf - 1]);
    sfree_(&sp);
    if (! matchd) {
	goto L180;
    }
    ret_val = len;
    goto L100;
L180:
    ret_val = -2;
    goto L100;
/* L181: */
L100:
    zzepro_();
    return ret_val;
} /* lhfink_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


integer lhfinn_(fd, formad, sectis)
integer *fd;
logical *formad;
shortint *sectis;
{
    /* Initialized data */

    static shortint st0001[7] = { 94,46,123,105,104,125,0 };
    static shortint st0002[5] = { 46,105,104,10,0 };

    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    static integer ip, op, sp;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
    static shortint lbuf[1024];
#define meml ((integer *)&mem_1)
    static integer sw0001;
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
    static integer npat;
#define memx ((complex *)&mem_1)
    extern /* Subroutine */ int sfree_(), smark_();
    static logical matchd;
    extern integer getlie_();
    extern logical lhmath_();
    static integer patoff[10];
    extern /* Subroutine */ int salloc_();
    static integer patbuf;
    extern /* Subroutine */ int ungete_();
    extern integer strmah_();
    extern /* Subroutine */ int zzepro_();

    /* Parameter adjustments */
    --sectis;

    /* Function Body */
    smark_(&sp);
    salloc_(&patbuf, &c__1023, &c__2);
    npat = 1;
    op = patbuf;
    patoff[0] = op;
    if (! (getlie_(fd, lbuf) == -2) && ! xercom_1.xerflg) {
	goto L110;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
    goto L91;
L110:
    ip = 1;
L120:
    if (! (sectis[ip] != 0)) {
	goto L122;
    }
    sw0001 = sectis[ip];
    goto L130;
L140:
    memc[op - 1] = 0;
    ++op;
/* Computing MIN */
    i__1 = 10, i__2 = npat + 1;
    npat = min(i__1,i__2);
    patoff[npat - 1] = op;
    goto L131;
L150:
    memc[op - 1] = sectis[ip];
    ++op;
    goto L131;
L130:
    if (sw0001 == 124) {
	goto L140;
    }
    goto L150;
L131:
/* L121: */
    ++ip;
    goto L120;
L122:
    memc[op - 1] = 0;
    matchd = FALSE_;
    if (! (*formad)) {
	goto L160;
    }
L170:
    if (! (strmah_(lbuf, st0001) > 0)) {
	goto L180;
    }
    if (! (getlie_(fd, lbuf) != -2) && ! xercom_1.xerflg) {
	goto L190;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
    matchd = lhmath_(lbuf, patoff, &npat);
    if (! matchd) {
	goto L200;
    }
    goto L172;
L200:
L190:
L180:
/* L171: */
    if (! (getlie_(fd, lbuf) == -2) && ! xercom_1.xerflg) {
	goto L170;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
L172:
    ungete_(fd, lbuf);
    ungete_(fd, st0002);
    goto L161;
L160:
L210:
    matchd = lhmath_(lbuf, patoff, &npat);
    if (! matchd) {
	goto L220;
    }
    goto L212;
L220:
/* L211: */
    if (! (getlie_(fd, lbuf) == -2) && ! xercom_1.xerflg) {
	goto L210;
    }
    if (xercom_1.xerflg) {
	goto L100;
    }
L212:
    ungete_(fd, lbuf);
L161:
L91:
    sfree_(&sp);
    if (! matchd) {
	goto L230;
    }
    ret_val = 0;
    goto L100;
L230:
    ret_val = -2;
    goto L100;
/* L231: */
L100:
    zzepro_();
    return ret_val;
} /* lhfinn_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


logical lhmath_(lbuf, patoff, npat)
shortint *lbuf;
integer *patoff, *npat;
{
    /* Initialized data */

    static shortint st0001[6] = { 94,123,37,115,125,0 };

    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static integer sp, pat;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
#define meml ((integer *)&mem_1)
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    extern /* Subroutine */ int sfree_(), smark_(), salloc_();
    static integer patten;
    extern integer strmah_();
    extern /* Subroutine */ int pargsr_(), sprinf_(), zzepro_();

    /* Parameter adjustments */
    --lbuf;
    --patoff;

    /* Function Body */
    smark_(&sp);
    salloc_(&patten, &c__255, &c__2);
    pat = 1;
L110:
    if (! (pat <= *npat)) {
	goto L112;
    }
    sprinf_(&memc[patten - 1], &c__255, st0001);
    pargsr_(&memc[patoff[pat] - 1]);
    if (! (strmah_(&lbuf[1], &memc[patten - 1]) > 0)) {
	goto L120;
    }
    sfree_(&sp);
    ret_val = TRUE_;
    goto L100;
L120:
/* L111: */
    ++pat;
    goto L110;
L112:
    sfree_(&sp);
    ret_val = FALSE_;
    goto L100;
L100:
    zzepro_();
    return ret_val;
} /* lhmath_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


/* Subroutine */ int lhmkne_(instr, outstr)
shortint *instr, *outstr;
{
    static integer i__;
    extern /* Subroutine */ int zzepro_(), xstrcy_(), strlwr_();

    /* Parameter adjustments */
    --outstr;
    --instr;

    /* Function Body */
    xstrcy_(&instr[1], &outstr[1], &c__1023);
    strlwr_(&outstr[1]);
    i__ = 1;
L110:
    if (! (i__ < 1023)) {
	goto L112;
    }
    if (! (outstr[i__] == 0 || outstr[i__] == 10)) {
	goto L120;
    }
    goto L112;
L120:
    if (outstr[i__] >= 65 && outstr[i__] <= 90 || outstr[i__] >= 97 && outstr[
	    i__] <= 122 || outstr[i__] >= 48 && outstr[i__] <= 57) {
	goto L130;
    }
    outstr[i__] = 95;
L130:
/* L121: */
/* L111: */
    ++i__;
    goto L110;
L112:
/* L100: */
    zzepro_();
    return 0;
} /* lhmkne_ */

