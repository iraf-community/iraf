/* zzdebug.x -- translated by f2c (version 20061008).
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
    logical xerflg, xerpad[84];
} xercom_;

#define xercom_1 xercom_

struct {
    doublereal memd[1];
} mem_;

#define mem_1 mem_

/* Table of constant values */

static integer c__4 = 4;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__1023 = 1023;
static integer c_b46 = 999999999;

integer 
sysruk_ (shortint *task, shortint *cmd, integer *rukarf, integer *rukint)
{
    /* Initialized data */

    static shortint dict[14] = { 115,116,97,99,107,0,114,101,97,108,108,111,
	    99,0 };
    static shortint st0009[29] = { 105,110,118,97,108,105,100,32,115,101,116,
	    32,115,116,97,116,101,109,101,110,116,58,32,39,37,115,39,10,0 };
    static shortint st0010[25] = { 105,110,118,97,108,105,100,32,83,69,84,32,
	    105,110,32,73,82,65,70,32,77,97,105,110,0 };
    static integer dp[3] = { 1,7,0 };
    static integer lmarg = 5;
    static integer maxch = 0;
    static integer ncol = 0;
    static integer rukean = 3;
    static integer ntasks = 0;
    static shortint st0001[9] = { 116,116,121,110,99,111,108,115,0 };
    static shortint st0002[6] = { 99,104,100,105,114,0 };
    static shortint st0003[3] = { 99,100,0 };
    static shortint st0004[6] = { 104,111,109,101,36,0 };
    static shortint st0005[6] = { 72,79,77,69,36,0 };
    static shortint st0006[4] = { 115,101,116,0 };
    static shortint st0007[6] = { 114,101,115,101,116,0 };
    static shortint st0008[2] = { 9,0 };

    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer i__, rmarg;
    extern logical streq_();
    extern /* Subroutine */ integer trealc_();
    extern integer envgei_();
    extern /* Subroutine */ integer xfchdr_(), erract_(), eprinf_(), tstack_()
	    ;
    extern integer envscn_();
    extern /* Subroutine */ integer xffluh_(), pargsr_(), envlit_(), syspac_()
	    , xerpsh_(), strtbl_();
    extern logical xerpop_();
    extern /* Subroutine */ integer zzepro_();

    /* Parameter adjustments */
    --cmd;
    --task;

    /* Function Body */
    if (! (ntasks == 0)) {
	goto L110;
    }
    i__ = 1;
L120:
    if (! (dp[i__ - 1] != 0)) {
	goto L122;
    }
/* L121: */
    ++i__;
    goto L120;
L122:
    ntasks = i__ - 1;
L110:
    if (! (task[1] == 63)) {
	goto L130;
    }
    xerpsh_();
    rmarg = envgei_(st0001);
    if (! xerpop_()) {
	goto L140;
    }
    rmarg = 80;
L140:
    strtbl_(&c__4, dict, dp, &ntasks, &lmarg, &rmarg, &maxch, &ncol);
    ret_val = 0;
    goto L100;
L130:
    if (! (streq_(&task[1], st0002) || streq_(&task[1], st0003))) {
	goto L150;
    }
    xerpsh_();
    if (! (cmd[*rukarf] == 0)) {
	goto L170;
    }
    xerpsh_();
    xfchdr_(st0004);
    if (! xerpop_()) {
	goto L180;
    }
    xfchdr_(st0005);
L180:
    goto L171;
L170:
    xfchdr_(&cmd[*rukarf]);
L171:
/* L162: */
    if (! xerpop_()) {
	goto L160;
    }
    if (! (*rukint == 1)) {
	goto L190;
    }
    erract_(&rukean);
    if (xercom_1.xerflg) {
	goto L100;
    }
    goto L191;
L190:
L191:
L160:
    ret_val = 0;
    goto L100;
L150:
    if (! (streq_(&task[1], st0006) || streq_(&task[1], st0007))) {
	goto L200;
    }
    xerpsh_();
    if (! (cmd[*rukarf] == 0)) {
	goto L220;
    }
    envlit_(&c__4, st0008, &c__1);
    xffluh_(&c__4);
    goto L221;
L220:
    if (! (envscn_(&cmd[1]) <= 0)) {
	goto L230;
    }
    if (! (*rukint == 1)) {
	goto L240;
    }
    eprinf_(st0009);
    pargsr_(&cmd[1]);
    goto L241;
L240:
    goto L91;
L241:
L230:
L221:
/* L212: */
    if (! xerpop_()) {
	goto L210;
    }
    if (! (*rukint == 1)) {
	goto L250;
    }
    erract_(&rukean);
    if (xercom_1.xerflg) {
	goto L100;
    }
    goto L251;
L250:
L91:
    syspac_(&c__0, st0010);
L251:
L210:
    ret_val = 0;
    goto L100;
L200:
/* L151: */
/* L131: */
    if (! streq_(&task[1], &dict[dp[0] - 1])) {
	goto L260;
    }
    tstack_();
    ret_val = 0;
    goto L100;
L260:
    if (! streq_(&task[1], &dict[dp[1] - 1])) {
	goto L270;
    }
    trealc_();
    ret_val = 0;
    goto L100;
L270:
    ret_val = -1;
    goto L100;
L100:
    zzepro_();
    return ret_val;
} /* sysruk_ */

/* Subroutine */ integer 
tstack_ (void)
{
    /* Initialized data */

    static shortint st0001[12] = { 98,117,102,102,101,114,95,115,105,122,101,
	    0 };
    static shortint st0002[28] = { 98,117,102,102,101,114,32,112,111,105,110,
	    116,101,114,61,37,100,44,32,115,105,122,101,61,37,100,10,0 };

    /* Local variables */
    static integer sp;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
#define meml ((integer *)&mem_1)
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    static integer junk;
    extern /* Subroutine */ integer pargi_(), sfree_(), smark_();
    extern integer clglpi_();
    static integer bufsie;
    extern /* Subroutine */ integer salloc_(), xffluh_(), xprinf_(), zzepro_()
	    ;

    smark_(&sp);
L110:
    if (! (clglpi_(st0001, &bufsie) != -2)) {
	goto L111;
    }
    salloc_(&junk, &bufsie, &c__2);
    xprinf_(st0002);
    pargi_(&junk);
    pargi_(&bufsie);
    xffluh_(&c__4);
    goto L110;
L111:
    sfree_(&sp);
/* L100: */
    zzepro_();
    return 0;
} /* tstack_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


/* Subroutine */ integer 
trealc_ (void)
{
    /* Initialized data */

    static shortint st0001[12] = { 97,98,99,100,101,102,103,104,105,106,107,0 
	    };
    static shortint st0002[11] = { 48,49,50,51,52,53,54,55,56,57,0 };
    static shortint st0003[25] = { 97,32,105,115,32,97,116,32,37,100,44,32,
	    115,105,122,101,32,37,100,58,32,37,115,10,0 };
    static shortint st0004[25] = { 98,32,105,115,32,97,116,32,37,100,44,32,
	    115,105,122,101,32,37,100,58,32,37,115,10,0 };
    static shortint st0005[33] = { 45,45,45,45,45,45,45,45,45,45,45,45,45,45,
	    45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,10,0 };
    static shortint st0006[10] = { 97,95,98,117,102,115,105,122,101,0 };
    static shortint st0007[10] = { 98,95,98,117,102,115,105,122,101,0 };
    static shortint st0008[30] = { 97,32,98,117,102,32,37,100,44,32,115,105,
	    122,101,32,37,100,32,45,45,62,32,37,100,58,32,37,115,10,0 };
    static shortint st0009[30] = { 98,32,98,117,102,32,37,100,44,32,115,105,
	    122,101,32,37,100,32,45,45,62,32,37,100,58,32,37,115,10,0 };

    /* Local variables */
    static integer a, b, sza, szb;
#define memb ((logical *)&mem_1)
#define memc ((shortint *)&mem_1)
#define memi ((integer *)&mem_1)
#define meml ((integer *)&mem_1)
#define memr ((real *)&mem_1)
#define mems ((shortint *)&mem_1)
#define memx ((complex *)&mem_1)
    extern /* Subroutine */ integer pargi_();
    extern integer clgeti_();
    extern /* Subroutine */ integer xrealc_(), xmallc_(), eprinf_(), xmfree_()
	    , pargsr_();
    static integer newsza, newszb;
    extern /* Subroutine */ integer zzepro_(), xstrcy_();

    xmallc_(&a, &c__1023, &c__2);
    xstrcy_(st0001, &memc[a - 1], &c_b46);
    sza = 1023;
    xmallc_(&b, &c__1023, &c__2);
    xstrcy_(st0002, &memc[b - 1], &c_b46);
    szb = 1023;
    eprinf_(st0003);
    pargi_(&a);
    pargi_(&sza);
    pargsr_(&memc[a - 1]);
    eprinf_(st0004);
    pargi_(&b);
    pargi_(&szb);
    pargsr_(&memc[b - 1]);
    eprinf_(st0005);
L110:
    newsza = clgeti_(st0006);
    if (! (newsza == 0)) {
	goto L120;
    }
    goto L100;
L120:
    xrealc_(&a, &newsza, &c__2);
    newszb = clgeti_(st0007);
    if (! (newszb == 0)) {
	goto L130;
    }
    goto L100;
L130:
    xrealc_(&b, &newszb, &c__2);
    eprinf_(st0008);
    pargi_(&a);
    pargi_(&sza);
    pargi_(&newsza);
    pargsr_(&memc[a - 1]);
    eprinf_(st0009);
    pargi_(&b);
    pargi_(&szb);
    pargi_(&newszb);
    pargsr_(&memc[b - 1]);
    sza = newsza;
    szb = newszb;
/* L111: */
    goto L110;
/* L112: */
    xmfree_(&a, &c__2);
    xmfree_(&b, &c__2);
L100:
    zzepro_();
    return 0;
} /* trealc_ */

#undef memx
#undef mems
#undef memr
#undef meml
#undef memi
#undef memc
#undef memb


