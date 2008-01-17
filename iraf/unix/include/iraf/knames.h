/*
 * KNAMES.H -- External names of the kernel procedures.  These are defined
 * because the trailing underscore is peculiar to this version of UNIX.
 * On some other system the underscore might not be necessary.  UNIX uses
 * the underscore to avoid name collisions between Fortran names and C/UNIX
 * names.  If your system does not employ such a convention, delete the _ but
 * do not delete the defines - there will probably be name collisions and
 * some of the names will have to be changed.  To change the external name
 * change the define given here.
 */

#ifndef D_knames
#define D_knames

#include <iraf/spptypes.h>

#define	c_main		cmain_
#define	IRAF_MAIN	irafmn_
#define	USHLIB		ushlib_
#define	VSHLIB		vshlib_
#define	VSHEND		vshend_
#define	VLIBINIT	vlibinit_
#define	KI_CONNECT	kicont_
#define	KI_GETHOSTS	kigets_
#define	KI_SEND		kisend_
#define	KI_RECEIVE	kirece_

extern XINT IRAF_MAIN ( XCHAR *, XINT *, XINT *, XINT *, XINT *, XINT *, XINT *, XCHAR *, XINT *, PFI, PFI );
extern XINT c_main ( XINT *, PKCHAR *, PKCHAR * );
/* zshlib.c */
#ifndef F2C_INCLUDE		/* for native C code */
extern unsigned XINT VSHLIB[];	/* shared library descriptor */
extern unsigned XINT USHLIB[];
extern unsigned XINT VSHEND;
#endif

int gfpucw_( XINT * );
int sfpucw_( XINT * );

#define	ZARDBF		zardbf_
#define	ZARDGD		zardgd_
#define	ZARDKS		zardks_
#define	ZARDLP		zardlp_
#define	ZARDND          zardnd_
#define	ZARDPL		zardpl_
#define	ZARDPR		zardpr_
#define	ZARDSF		zardsf_
#define	ZAWRBF		zawrbf_
#define	ZAWRGD		zawrgd_
#define	ZAWRKS		zawrks_
#define	ZAWRLP		zawrlp_
#define	ZAWRND          zawrnd_
#define	ZAWRPL		zawrpl_
#define	ZAWRPR		zawrpr_
#define	ZAWRSF		zawrsf_
#define	ZAWSET		zawset_
#define	ZAWTBF		zawtbf_
#define	ZAWTGD		zawtgd_
#define	ZAWTKS		zawtks_
#define	ZAWTLP		zawtlp_
#define	ZAWTND          zawtnd_
#define	ZAWTPL		zawtpl_
#define	ZAWTPR		zawtpr_
#define	ZAWTSF		zawtsf_
#define	ZCALL0		zcall0_
#define	ZCALL1		zcall1_
#define	ZCALL2		zcall2_
#define	ZCALL3		zcall3_
#define	ZCALL4		zcall4_
#define	ZCALL5		zcall5_
#define	ZCALL6		zcall6_
#define	ZCALL7		zcall7_
#define	ZCALL8		zcall8_
#define	ZCALL9		zcall9_
#define	ZCALLA		zcalla_
#define	ZCLCPR		zclcpr_
#define	ZCLDIR		zcldir_
#define	ZCLDPR		zcldpr_
#define	ZCLSBF		zclsbf_
#define	ZCLSGD		zclsgd_
#define	ZCLSKS		zclsks_
#define	ZCLSLP		zclslp_
#define	ZCLSND          zclsnd_
#define	ZCLSPL		zclspl_
#define	ZCLSSF		zclssf_
#define	ZCLSTX		zclstx_
#define	ZCLSTY		zclsty_
#define	ZDOJMP		zdojmp_
#define	ZDVALL		zdvall_
#define	ZDVOWN		zdvown_
#define	ZFACSS		zfacss_
#define	ZFALOC		zfaloc_
#define	ZFCHDR		zfchdr_
#define	ZFDELE		zfdele_
#define	ZFGCWD		zfgcwd_
#define	ZFINFO		zfinfo_
#define	ZFLSTX		zflstx_
#define	ZFLSTY		zflsty_
#define	ZFMKCP		zfmkcp_
#define	ZFMKDR		zfmkdr_
#define	ZFNBRK		zfnbrk_
#define	ZFPATH		zfpath_
#define	ZFPOLL		zfpoll_
#define	ZFPROT		zfprot_
#define	ZFRNAM		zfrnam_
#define	ZFSUBD		zfsubd_
#define	ZFUNC0		zfunc0_
#define	ZFUNC1		zfunc1_
#define	ZFUNC2		zfunc2_
#define	ZFUNC3		zfunc3_
#define	ZFUNC4		zfunc4_
#define	ZFUNC5		zfunc5_
#define	ZFUNC6		zfunc6_
#define	ZFUNC7		zfunc7_
#define	ZFUNC8		zfunc8_
#define	ZFUNC9		zfunc9_
#define	ZFUNCA		zfunca_
#define	ZFUTIM		zfutim_
#define	ZFXDIR		zfxdir_
#define	ZGCMDL		zgcmdl_
#define	ZGETTT		zgettt_
#define	ZGETTX		zgettx_
#define	ZGETTY		zgetty_
#define	ZGFDIR		zgfdir_
#define	ZGHOST		zghost_
#define	ZGMTCO		zgmtco_
#define	ZGTENV		zgtenv_
#define	ZGTIME		zgtime_
#define	ZGTPID		zgtpid_
#define	ZINTPR		zintpr_
#define	ZLOCPR		zlocpr_
#define	ZLOCVA		zlocva_
#define	ZMALOC		zmaloc_
#define	ZMEMCK		zmemck_
#define	ZMFREE		zmfree_
#define	ZNOTTX		znottx_
#define	ZNOTTY		znotty_
#define	ZOPCPR		zopcpr_
#define	ZOPDIR		zopdir_
#define	ZOPDPR		zopdpr_
#define	ZOPNBF		zopnbf_
#define	ZOPNGD		zopngd_
#define	ZOPNKS		zopnks_
#define	ZOPNLP		zopnlp_
#define	ZOPNND          zopnnd_
#define	ZOPNPL		zopnpl_
#define	ZOPNSF		zopnsf_
#define	ZOPNTX		zopntx_
#define	ZOPNTY		zopnty_
#define	ZOSCMD		zoscmd_
#define	ZPANIC		zpanic_
#define	ZPUTTX		zputtx_
#define	ZPUTTY		zputty_
#define	ZRALOC		zraloc_
#define	ZSEKTX		zsektx_
#define	ZSEKTY		zsekty_
#define	ZSTTBF		zsttbf_
#define	ZSTTGD		zsttgd_
#define	ZSTTKS		zsttks_
#define	ZSTTLP		zsttlp_
#define	ZSTTND          zsttnd_
#define	ZSTTPL		zsttpl_
#define	ZSTTPR		zsttpr_
#define	ZSTTSF		zsttsf_
#define	ZSTTTX		zstttx_
#define	ZSTTTY		zsttty_
#define	ZSVJMP		zsvjmp_
#define	ZTSLEE		ztslee_
#define	ZWMSEC		zwmsec_
#define	ZXGMES		zxgmes_
#define	ZXWHEN		zxwhen_
#define	ZZCLMT		zzclmt_
#define ZZEPRO          zzepro_
#define	ZZOPMT		zzopmt_
#define	ZZRDMT		zzrdmt_
#define	ZZRWMT		zzrwmt_
#define	ZZSETK		zzsetk_
#define	ZZSTMT		zzstmt_
#define	ZZSTOP		zzstop_
#define	ZZSTRT		zzstrt_
#define	ZZWRMT		zzwrmt_
#define	ZZWTMT		zzwtmt_


/* If KNET name mapping is enabled selected machine level kernel names are
 * mapped into procedures in the KI (kernel interface) package.  This is
 * necessary if the high level code is to have networking capabilities.
 * Define NOKNET to disable this mapping, e.g., in the kernel itself.
 */

#ifndef NOKNET

#define	zardnd_	kardnd_
#define	zawrnd_	kawrnd_
#define	zawtnt_	kawtnd_
#define	zclsnd_	kclsnd_
#define	zfchdr_ kfchdr_
#define	zfgcwd_ kfgcwd_
#define	zfpath_ kfpath_
#define	zfsubd_ kfsubd_
#define	zopnnd_	kopnnd_
#define	zsttnd_	ksttnd_
#define zardbf_	kardbf_
#define zardlp_	kardlp_
#define zardpl_	kardpl_
#define zardpr_	kardpr_
#define zardsf_	kardsf_
#define zawrbf_	kawrbf_
#define zawrlp_	kawrlp_
#define zawrpl_	kawrpl_
#define zawrpr_	kawrpr_
#define zawrsf_	kawrsf_
#define zawtbf_	kawtbf_
#define zawtlp_	kawtlp_
#define zawtpl_	kawtpl_
#define zawtpr_	kawtpr_
#define zawtsf_	kawtsf_
#define zclcpr_	kclcpr_
#define zcldir_	kcldir_
#define zcldpr_	kcldpr_
#define zclsbf_	kclsbf_
#define zclslp_	kclslp_
#define zclspl_	kclspl_
#define zclspr_	kclspr_
#define zclssf_	kclssf_
#define zclstx_	kclstx_
#define zclsty_	kclsty_
#define zdvall_	kdvall_
#define zdvown_	kdvown_
#define zfacss_	kfacss_
#define zfaloc_	kfaloc_
#define zfdele_	kfdele_
#define zfinfo_	kfinfo_
#define zflstx_	kflstx_
#define zflsty_	kflsty_
#define zfmkcp_	kfmkcp_
#define zfmkdr_	kfmkdr_
#define zfprot_	kfprot_
#define zfrnam_	kfrnam_
#define zfutim_	kfutim_
#define zgettx_	kgettx_
#define zgetty_	kgetty_
#define zgfdir_	kgfdir_
#define zintpr_	kintpr_
#define znottx_	knottx_
#define znotty_	knotty_
#define zopcpr_	kopcpr_
#define zopdir_	kopdir_
#define zopdpr_	kopdpr_
#define zopnbf_	kopnbf_
#define zopnlp_	kopnlp_
#define zopnpl_	kopnpl_
#define zopnpr_	kopnpr_
#define zopnsf_	kopnsf_
#define zopntx_	kopntx_
#define zopnty_	kopnty_
#define zoscmd_	koscmd_
#define zputtx_	kputtx_
#define zputty_	kputty_
#define zsektx_	ksektx_
#define zsekty_	ksekty_
#define zsttbf_	ksttbf_
#define zsttlp_	ksttlp_
#define zsttpl_	ksttpl_
#define zsttpr_	ksttpr_
#define zsttsf_	ksttsf_
#define zstttx_	kstttx_
#define zsttty_	ksttty_
#define zzclmt_	kzclmt_
#define zzopmt_	kzopmt_
#define zzrdmt_	kzrdmt_
#define zzrwmt_	kzrwmt_
#define zzstmt_	kzstmt_
#define zzwrmt_	kzwrmt_
#define zzwtmt_	kzwtmt_

#endif

extern int ZARDBF ( XINT *, XCHAR *, XINT *, XLONG * );
/* ZARDGD -> gdev/ */
extern int ZARDKS ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDLP ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDND ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDPL ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDPR ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDSF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRBF ( XINT *, XCHAR *, XINT *, XLONG * );
/* ZAWRGD -> gdev/ */
extern int ZAWRKS ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRLP ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRND ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRPL ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRPR ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRSF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWSET ( XINT *, XINT *, XINT *, XINT * );
extern int ZAWTBF ( XINT *, XINT * );
/* ZAWTGD -> gdev/ */
extern int ZAWTKS ( XINT *, XINT * );
extern int ZAWTLP ( XINT *, XINT * );
extern int ZAWTND ( XINT *, XINT * );
extern int ZAWTPL ( XINT *, XINT * );
extern int ZAWTPR ( XINT *, XINT * );
extern int ZAWTSF ( XINT *, XINT * );
extern int ZCALL0 ( XPOINTER * );
extern int ZCALL1 ( XPOINTER *, void * );
extern int ZCALL2 ( XPOINTER *, void *, void * );
extern int ZCALL3 ( XPOINTER *, void *, void *, void * );
extern int ZCALL4 ( XPOINTER *, void *, void *, void *, void * );
extern int ZCALL5 ( XPOINTER *, void *, void *, void *, void *, void * );
extern int ZCALL6 ( XPOINTER *, void *, void *, void *, void *, void *, void * );
extern int ZCALL7 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void * );
extern int ZCALL8 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void * );
extern int ZCALL9 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern int ZCALLA ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern int ZCLCPR ( XINT *, XINT * );
extern int ZCLDIR ( XINT *, XINT * );
extern int ZCLDPR ( XINT *, XINT *, XINT * );
extern int ZCLSBF ( XINT *, XINT * );
/* ZCLSGD -> gdev/ */
extern int ZCLSKS ( XINT *, XINT * );
extern int ZCLSLP ( XINT *, XINT * );
extern int ZCLSND ( XINT *, XINT * );
extern int ZCLSPL ( XINT *, XINT * );
extern int ZCLSSF ( XINT *, XINT * );
extern int ZCLSTX ( XINT *, XINT * );
extern int ZCLSTY ( XINT *, XINT * );
extern void ZDOJMP ( XPOINTER *, XINT * );
extern int ZDVALL ( PKCHAR *, XINT *, XINT * );
extern int ZDVOWN ( PKCHAR *, PKCHAR *, XINT *, XINT * );
extern int ZFACSS ( PKCHAR *, XINT *, XINT *, XINT * );
extern int ZFALOC ( PKCHAR *, XLONG *, XINT * );
extern int ZFCHDR ( PKCHAR *, XINT *);
extern int ZFDELE ( PKCHAR *, XINT * );
extern int ZFGCWD ( PKCHAR *, XINT *, XINT * );
extern int ZFINFO ( PKCHAR *, XLONG *, XINT * );
extern int ZFLSTX ( XINT *, XINT * );
extern int ZFLSTY ( XINT *, XINT * );
extern int ZFMKCP ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFMKDR ( PKCHAR *, XINT * );
extern int ZFNBRK ( XCHAR *, XINT *, XINT * );
extern int ZFPATH ( XCHAR *, XCHAR *, XINT *, XINT * );
extern int ZFPOLL ( XINT *, XINT *, XINT *, XINT *, XINT * );
extern int ZFPROT ( PKCHAR *, XINT *, XINT * );
extern int ZFRNAM ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFSUBD ( XCHAR *, XINT *, XCHAR *, XINT * );
extern XINT ZFUNC0 ( XPOINTER * );
extern XINT ZFUNC1 ( XPOINTER *, void * );
extern XINT ZFUNC2 ( XPOINTER *, void *, void * );
extern XINT ZFUNC3 ( XPOINTER *, void *, void *, void * );
extern XINT ZFUNC4 ( XPOINTER *, void *, void *, void *, void * );
extern XINT ZFUNC5 ( XPOINTER *, void *, void *, void *, void *, void * );
extern XINT ZFUNC6 ( XPOINTER *, void *, void *, void *, void *, void *, void * );
extern XINT ZFUNC7 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void * );
extern XINT ZFUNC8 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XINT ZFUNC9 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XINT ZFUNCA ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern int ZFUTIM ( PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZFXDIR ( XCHAR *, XCHAR *, XINT *, XINT *);
extern int ZGCMDL ( PKCHAR *, XINT *, XINT * );
/* ??? ZGETTT ??? */
extern int ZGETTX ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZGETTY ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZGFDIR ( XINT *, PKCHAR *, XINT *, XINT * );
extern int ZGHOST ( PKCHAR *, XINT * );
extern int ZGMTCO ( XINT * );
extern int ZGTENV ( PKCHAR *, PKCHAR *, XINT *, XINT * );
extern int ZGTIME ( XLONG *, XLONG * );
extern int ZGTPID ( XINT * );
extern int ZINTPR ( XINT *, XINT *, XINT * );
extern int ZLOCPR ( PFU, XINT * );
extern int ZLOCVA ( XCHAR *, XINT * );
extern int ZMALOC ( XINT *, XINT *, XINT * );
/* ??? ZMEMCK ??? */
extern int ZMFREE ( XINT *, XINT * );
extern int ZNOTTX ( XINT *, XLONG * );
extern int ZNOTTY ( XINT *, XLONG * );
extern int ZOPCPR ( PKCHAR *, XINT *, XINT *, XINT * );
extern int ZOPDIR ( PKCHAR *, XINT * );
extern int ZOPDPR ( PKCHAR *, PKCHAR *, PKCHAR *, XINT * );
extern int ZOPNBF ( PKCHAR *, XINT *, XINT * );
/* ZOPNGD -> gdev/ */
extern int ZOPNKS ( PKCHAR *, XINT *, XINT * );
extern int ZOPNLP ( PKCHAR *, XINT *, XINT * );
extern int ZOPNND ( PKCHAR *, XINT *, XINT * );
extern int ZOPNPL ( PKCHAR *, XINT *, XINT * );
extern int ZOPNSF ( PKCHAR *, XINT *, XINT * );
extern int ZOPNTX ( PKCHAR *, XINT *, XINT * );
extern int ZOPNTY ( PKCHAR *, XINT *, XINT * );
extern int ZOSCMD ( PKCHAR *, PKCHAR *, PKCHAR *, PKCHAR *, XINT * );
extern int ZPANIC ( XINT *, PKCHAR * );
extern int ZPUTTX ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZPUTTY ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZRALOC ( XINT *, XINT *, XINT * );
extern int ZSEKTX ( XINT *, XLONG *, XINT * );
extern int ZSEKTY ( XINT *, XLONG *, XINT * );
extern int ZSTTBF ( XINT *, XINT *, XLONG * );
/* ZSTTGD -> gdev/ */
extern int ZSTTKS ( XINT *, XINT *, XLONG * );
extern int ZSTTLP ( XINT *, XINT *, XLONG * );
extern int ZSTTND ( XINT *, XINT *, XLONG * );
extern int ZSTTPL ( XINT *, XINT *, XLONG * );
extern int ZSTTPR ( XINT *, XINT *, XLONG * );
extern int ZSTTSF ( XINT *, XINT *, XLONG * );
extern int ZSTTTX ( XINT *, XINT *, XLONG * );
extern int ZSTTTY ( XINT *, XINT *, XLONG * );
extern int ZSVJMP ( XPOINTER *, XINT * );
/* ??? ZTSLEE ??? */
extern int ZWMSEC ( XINT * );
extern int ZXGMES ( XINT *, PKCHAR *, XINT * );
extern int ZXWHEN ( XINT *, XINT *, XINT * );
extern int ZZCLMT ( XINT *, XINT *, XINT * );
extern int ZZEPRO ( void );
extern int ZZOPMT ( PKCHAR *, XINT *, PKCHAR *, XINT *, XINT *, XINT * );
extern int ZZRDMT ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZZRWMT ( PKCHAR *, PKCHAR *, XINT * );
extern int ZZSETK ( XCHAR *, XCHAR *, XINT *, XINT *, XINT *, XINT * );
extern int ZZSTMT ( XINT *, XINT *, XLONG * );
extern int ZZSTOP ( void );
extern int ZZSTRT ( void );
extern int ZZWRMT ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZZWTMT ( XINT *, XINT *, XINT * );


#ifdef F2C_INCLUDE	/* for C code written by f2c */

#ifndef NOKNET

#undef zardnd_
#undef zawrnd_
#undef zawtnt_
#undef zclsnd_
#undef zfchdr_
#undef zfgcwd_
#undef zfpath_
#undef zfsubd_
#undef zopnnd_
#undef zsttnd_
#undef zardbf_
#undef zardlp_
#undef zardpl_
#undef zardpr_
#undef zardsf_
#undef zawrbf_
#undef zawrlp_
#undef zawrpl_
#undef zawrpr_
#undef zawrsf_
#undef zawtbf_
#undef zawtlp_
#undef zawtpl_
#undef zawtpr_
#undef zawtsf_
#undef zclcpr_
#undef zcldir_
#undef zcldpr_
#undef zclsbf_
#undef zclslp_
#undef zclspl_
#undef zclspr_
#undef zclssf_
#undef zclstx_
#undef zclsty_
#undef zdvall_
#undef zdvown_
#undef zfacss_
#undef zfaloc_
#undef zfdele_
#undef zfinfo_
#undef zflstx_
#undef zflsty_
#undef zfmkcp_
#undef zfmkdr_
#undef zfprot_
#undef zfrnam_
#undef zfutim_
#undef zgettx_
#undef zgetty_
#undef zgfdir_
#undef zintpr_
#undef znottx_
#undef znotty_
#undef zopcpr_
#undef zopdir_
#undef zopdpr_
#undef zopnbf_
#undef zopnlp_
#undef zopnpl_
#undef zopnpr_
#undef zopnsf_
#undef zopntx_
#undef zopnty_
#undef zoscmd_
#undef zputtx_
#undef zputty_
#undef zsektx_
#undef zsekty_
#undef zsttbf_
#undef zsttlp_
#undef zsttpl_
#undef zsttpr_
#undef zsttsf_
#undef zstttx_
#undef zsttty_
#undef zzclmt_
#undef zzopmt_
#undef zzrdmt_
#undef zzrwmt_
#undef zzstmt_
#undef zzwrmt_
#undef zzwtmt_

/* Dummy prototypes for C code written by f2c */
extern int ZARDND ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRND ( XINT *, XCHAR *, XINT *, XLONG * );
/* ??? ZAWTNT ??? */
extern int ZCLSND ( XINT *, XINT * );
extern int ZFCHDR ( PKCHAR *, XINT *);
extern int ZFGCWD ( PKCHAR *, XINT *, XINT * );
extern int ZFPATH ( XCHAR *, XCHAR *, XINT *, XINT * );
extern int ZFSUBD ( XCHAR *, XINT *, XCHAR *, XINT * );
extern int ZOPNND ( PKCHAR *, XINT *, XINT * );
extern int ZSTTND ( XINT *, XINT *, XLONG * );
extern int ZARDBF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDLP ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDPL ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDPR ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZARDSF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRBF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRLP ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRPL ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRPR ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWRSF ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZAWTBF ( XINT *, XINT * );
extern int ZAWTLP ( XINT *, XINT * );
extern int ZAWTPL ( XINT *, XINT * );
extern int ZAWTPR ( XINT *, XINT * );
extern int ZAWTSF ( XINT *, XINT * );
extern int ZCLCPR ( XINT *, XINT * );
extern int ZCLDIR ( XINT *, XINT * );
extern int ZCLDPR ( XINT *, XINT *, XINT * );
extern int ZCLSBF ( XINT *, XINT * );
extern int ZCLSLP ( XINT *, XINT * );
extern int ZCLSPL ( XINT *, XINT * );
/* ??? ZCLSPR ??? */
extern int ZCLSSF ( XINT *, XINT * );
extern int ZCLSTX ( XINT *, XINT * );
extern int ZCLSTY ( XINT *, XINT * );
extern int ZDVALL ( PKCHAR *, XINT *, XINT * );
extern int ZDVOWN ( PKCHAR *, PKCHAR *, XINT *, XINT * );
extern int ZFACSS ( PKCHAR *, XINT *, XINT *, XINT * );
extern int ZFALOC ( PKCHAR *, XLONG *, XINT * );
extern int ZFDELE ( PKCHAR *, XINT * );
extern int ZFINFO ( PKCHAR *, XLONG *, XINT * );
extern int ZFLSTX ( XINT *, XINT * );
extern int ZFLSTY ( XINT *, XINT * );
extern int ZFMKCP ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFMKDR ( PKCHAR *, XINT * );
extern int ZFPROT ( PKCHAR *, XINT *, XINT * );
extern int ZFRNAM ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFUTIM ( PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZGETTX ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZGETTY ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZGFDIR ( XINT *, PKCHAR *, XINT *, XINT * );
extern int ZINTPR ( XINT *, XINT *, XINT * );
extern int ZNOTTX ( XINT *, XLONG * );
extern int ZNOTTY ( XINT *, XLONG * );
extern int ZOPCPR ( PKCHAR *, XINT *, XINT *, XINT * );
extern int ZOPDIR ( PKCHAR *, XINT * );
extern int ZOPDPR ( PKCHAR *, PKCHAR *, PKCHAR *, XINT * );
extern int ZOPNBF ( PKCHAR *, XINT *, XINT * );
extern int ZOPNLP ( PKCHAR *, XINT *, XINT * );
extern int ZOPNPL ( PKCHAR *, XINT *, XINT * );
/* ??? ZOPNPR ??? */
extern int ZOPNSF ( PKCHAR *, XINT *, XINT * );
extern int ZOPNTX ( PKCHAR *, XINT *, XINT * );
extern int ZOPNTY ( PKCHAR *, XINT *, XINT * );
extern int ZOSCMD ( PKCHAR *, PKCHAR *, PKCHAR *, PKCHAR *, XINT * );
extern int ZPUTTX ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZPUTTY ( XINT *, XCHAR *, XINT *, XINT * );
extern int ZSEKTX ( XINT *, XLONG *, XINT * );
extern int ZSEKTY ( XINT *, XLONG *, XINT * );
extern int ZSTTBF ( XINT *, XINT *, XLONG * );
extern int ZSTTLP ( XINT *, XINT *, XLONG * );
extern int ZSTTPL ( XINT *, XINT *, XLONG * );
extern int ZSTTPR ( XINT *, XINT *, XLONG * );
extern int ZSTTSF ( XINT *, XINT *, XLONG * );
extern int ZSTTTX ( XINT *, XINT *, XLONG * );
extern int ZSTTTY ( XINT *, XINT *, XLONG * );
extern int ZZCLMT ( XINT *, XINT *, XINT * );
extern int ZZOPMT ( PKCHAR *, XINT *, PKCHAR *, XINT *, XINT *, XINT * );
extern int ZZRDMT ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZZRWMT ( PKCHAR *, PKCHAR *, XINT * );
extern int ZZSTMT ( XINT *, XINT *, XLONG * );
extern int ZZWRMT ( XINT *, XCHAR *, XINT *, XLONG * );
extern int ZZWTMT ( XINT *, XINT *, XINT * );

#endif

#endif


/* Procedure names of miscellaneous potentially machine dependent Bit and Byte
 * Primitives.
 */
#define	ACLRB		aclrb_
#define	ANDI		andi_
#define	ANDL		andl_
#define	ANDS		ands_
#define	BITMOV		bitmov_
#define	BITPAK		bitpak_
#define	BITUPK		bitupk_
#define	BSWAP2		bswap2_
#define	BSWAP4		bswap4_
#define	BSWAP8		bswap8_
#define	BYTMOV		bytmov_
#define	CHRPAK		chrpak_
#define	CHRUPK		chrupk_
#define	D1MACH		d1mach_
#define	I1MACH		i1mach_
#define I32TO64		i32to4_
#define I64TO32		i64to2_
#define	MIILEN		miilen_
#define	MIIPAK		miipak_
#define	MIIUPK		miiupk_
#define	NOTI		noti_
#define	NOTL		notl_
#define	NOTS		nots_
#define	ORI		ori_
#define	ORL		orl_
#define	ORS		ors_
#define	R1MACH		r1mach_
#define	SHIFTI		shifti_
#define	SHIFTS		shifts_
#define	SHIFTL		shiftl_
#define	STRPAK		strpak_
#define	STRUPK		strupk_
#define	XORI		xori_
#define	XORL		xorl_
#define	XORS		xors_

#define	ACLRC		aclrc_
#define	ACLRD		aclrd_
#define	ACLRI		aclri_
#define	ACLRL		aclrl_
#define	ACLRR		aclrr_
#define	ACLRS		aclrs_
#define	AMOVC		amovc_
#define	AMOVD		amovd_
#define	AMOVI		amovi_
#define	AMOVL		amovl_
#define	AMOVR		amovr_
#define	AMOVS		amovs_


/* Procedure names for the potentially machine dependent VOPS vector
 * primitives.  The ACHT stands for change datatype, the B suffix refers
 * to primitives which deal with unsigned machine bytes, and the U suffix
 * refers to primitives which deal with unsigned short (16 bit) integers.
 */
#define	ACHTBB		achtbb_
#define	ACHTBC		achtbc_
#define	ACHTBD		achtbd_
#define	ACHTBI		achtbi_
#define	ACHTBL		achtbl_
#define	ACHTBR		achtbr_
#define	ACHTBS		achtbs_
#define	ACHTBU		achtbu_
#define	ACHTBX		achtbx_
#define	ACHTCB		achtcb_
#define	ACHTCU		achtcu_
#define	ACHTDB		achtdb_
#define	ACHTDU		achtdu_
#define	ACHTIB		achtib_
#define	ACHTIU		achtiu_
#define	ACHTLB		achtlb_
#define	ACHTLU		achtlu_
#define	ACHTRB		achtrb_
#define	ACHTRU		achtru_
#define	ACHTSB		achtsb_
#define	ACHTSU		achtsu_
#define	ACHTUB		achtub_
#define	ACHTUC		achtuc_
#define	ACHTUD		achtud_
#define	ACHTUI		achtui_
#define	ACHTUL		achtul_
#define	ACHTUR		achtur_
#define	ACHTUS		achtus_
#define	ACHTUU		achtuu_
#define	ACHTUX		achtux_
#define	ACHTXB		achtxb_
#define	ACHTXU		achtxu_


extern int ACLRC ( XCHAR *, XINT * );
extern int ACLRD ( XDOUBLE *, XINT * );
extern int ACLRI ( XINT *, XINT * );
extern int ACLRL ( XLONG *, XINT * );
extern int ACLRR ( XREAL *, XINT * );
extern int ACLRS ( XSHORT *, XINT * );
extern int AMOVC ( XCHAR *, XCHAR *, XINT * );
extern int AMOVD ( XDOUBLE *, XDOUBLE *, XINT * );
extern int AMOVI ( XINT *, XINT *, XINT * );
extern int AMOVL ( XLONG *, XLONG *, XINT * );
extern int AMOVR ( XREAL *, XREAL *, XINT * );
extern int AMOVS ( XSHORT *, XSHORT *, XINT * );

extern int ACHTBB ( XCHAR *, XCHAR *, XINT * );
extern int ACHTBC ( XCHAR *, XCHAR *, XINT * );
extern int ACHTBD ( XCHAR *, XDOUBLE *, XINT * );
extern int ACHTBI ( XCHAR *, XINT *, XINT * );
extern int ACHTBL ( XCHAR *, XLONG *, XINT * );
extern int ACHTBR ( XCHAR *, XREAL *, XINT * );
extern int ACHTBS ( XCHAR *, XSHORT *, XINT * );
extern int ACHTBU ( XCHAR *, XUSHORT *, XINT * );
extern int ACHTBX ( XCHAR *, XCOMPLEX *, XINT * );
extern int ACHTCB ( XCHAR *, XCHAR *, XINT * );
extern int ACHTCU ( XCHAR *, XUSHORT *, XINT * );
extern int ACHTDB ( XDOUBLE *, XCHAR *, XINT * );
extern int ACHTDU ( XDOUBLE *, XUSHORT *, XINT * );
extern int ACHTIB ( XINT *, XCHAR *, XINT * );
extern int ACHTIU ( XINT *, XUSHORT *, XINT * );
extern int ACHTLB ( XLONG *, XCHAR *, XINT * );
extern int ACHTLU ( XLONG *, XUSHORT *, XINT * );
extern int ACHTRB ( XREAL *, XCHAR *, XINT * );
extern int ACHTRU ( XREAL *, XUSHORT *, XINT * );
extern int ACHTSB ( XSHORT *, XCHAR *, XINT * );
extern int ACHTSU ( XSHORT *, XUSHORT *, XINT * );
extern int ACHTUB ( XUSHORT *, XCHAR *, XINT * );
extern int ACHTUC ( XUSHORT *, XCHAR *, XINT * );
extern int ACHTUD ( XUSHORT *, XDOUBLE *, XINT * );
extern int ACHTUI ( XUSHORT *, XINT *, XINT * );
extern int ACHTUL ( XUSHORT *, XLONG *, XINT * );
extern int ACHTUR ( XUSHORT *, XREAL *, XINT * );
extern int ACHTUS ( XUSHORT *, XSHORT *, XINT * );
extern int ACHTUU ( XUSHORT *, XUSHORT *, XINT * );
extern int ACHTUX ( XUSHORT *, XCOMPLEX *, XINT * );
extern int ACHTXB ( XCOMPLEX *, XCHAR *, XINT * );
extern int ACHTXU ( XCOMPLEX *, XUSHORT *, XINT * );
extern int ACLRB ( XCHAR *, XINT * );
extern XINT ANDI ( XINT *, XINT * );
extern XSHORT ANDS ( XSHORT *, XSHORT * );
extern XLONG ANDL ( XLONG *, XLONG * );
extern int BITPAK ( XINT *, XINT *, XINT *, XINT * );
extern XINT BITUPK ( XINT *, XINT *, XINT * );
extern int BSWAP2 ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int BSWAP4 ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int BSWAP8 ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int BYTMOV ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int CHRPAK ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int CHRUPK ( XCHAR *, XINT *, XCHAR *, XINT *, XINT * );
extern int I32TO64 ( XCHAR *, XCHAR *, XINT * );
extern int I64TO32 ( XCHAR *, XCHAR *, XINT * );
extern XINT NOTI ( XINT * );
extern XSHORT NOTS ( XSHORT * );
extern XLONG NOTL ( XLONG * );
extern XINT ORI ( XINT *, XINT * );
extern XSHORT ORS ( XSHORT *, XSHORT * );
extern XLONG ORL ( XLONG *, XLONG * );
extern XINT SHIFTI ( XINT *, XINT * );
extern XSHORT SHIFTS ( XSHORT *, XSHORT * );
extern XLONG SHIFTL ( XLONG *, XLONG * );
extern int STRPAK ( XCHAR *, PKCHAR *, XINT * );
extern int STRUPK ( PKCHAR *, XCHAR *, XINT * );


#endif
