#ifndef	_IRAF_KNAMES_H
#define	_IRAF_KNAMES_H

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

extern XINT IRAF_MAIN ( XCHAR *, XINT *, XINT *, XINT *, XPOINTER *, XINT *, XINT *, XCHAR *, XINT *, PFI, PFI );
extern XINT c_main ( XINT *, PKCHAR *, PKCHAR * );
/* zshlib.c */
#ifndef F2C_INCLUDE		/* for native C code */
extern unsigned XINT VSHLIB[];	/* shared library descriptor */
extern unsigned XINT USHLIB[];
extern unsigned XINT VSHEND;
#endif

#define	IEEVPAKR	ieevpr_
#define IEEVPAKD	ieevpd_
#define	IEEVUPKR	ieevur_
#define	IEEVUPKD	ieevud_
#define	IEEPAKR		ieepar_
#define	IEEPAKD		ieepad_
#define	IEEUPKR		ieeupr_
#define	IEEUPKD		ieeupd_
#define	IEESNANR	ieesnr_
#define	IEESNAND	ieesnd_
#define	IEEGNANR	ieegnr_
#define	IEEGNAND	ieegnd_
#define	IEESTATR	ieestr_
#define	IEESTATD	ieestd_
#define	IEEZSTATR	ieezsr_
#define	IEEZSTATD	ieezsd_
#define	IEEMAPR		ieemar_
#define	IEEMAPD		ieemad_
#define	IEEGMAPR	ieegmr_
#define	IEEGMAPD	ieegmd_
#define	IEESMAPR	ieesmr_
#define	IEESMAPD	ieesmd_
#define	IEEE_SIGMASK	ieeesk_
#define	IEEE_SIGRESTORE	ieeese_

int IEEVPAKR ( XREAL *, void *, XSIZE_T * );
int IEEVPAKD ( XDOUBLE *, void *, XSIZE_T * );
int IEEVUPKR ( void *, XREAL *, XSIZE_T * );
int IEEVUPKD ( void *, XDOUBLE *, XSIZE_T * );
int IEEPAKR ( XREAL * );
int IEEPAKD ( XDOUBLE * );
int IEEUPKR ( XREAL * );
int IEEUPKD ( XDOUBLE * );
int IEESNANR ( XREAL * );
int IEESNAND ( XDOUBLE * );
int IEEGNANR ( XREAL * );
int IEEGNAND ( XDOUBLE * );
int IEESTATR ( XSIZE_T *, XSIZE_T * );
int IEESTATD ( XSIZE_T *, XSIZE_T * );
int IEEZSTATR ( void );
int IEEZSTATD ( void );
int IEEMAPR ( XINT *, XINT * );
int IEEMAPD ( XINT *, XINT * );
int IEEGMAPR ( XINT *, XINT * );
int IEEGMAPD ( XINT *, XINT * );
int IEESMAPR ( XINT *, XINT * );
int IEESMAPD ( XINT *, XINT * );
int IEEE_SIGMASK ( void );
int IEEE_SIGRESTORE ( void );

#define	GFPUCW		gfpucw_
#define	SFPUCW		sfpucw_

#define	GFPUSW		gfpusw_

#define	MXMASK		mxmask_
#define	MXUMSK		mxumsk_

int GFPUCW ( XINT * );
int SFPUCW ( XINT * );

int GFPUSW ( XINT * );

int MXMASK ( void );
int MXUMSK ( void );


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
#define	ZLFNC0		zlfnc0_
#define	ZLFNC1		zlfnc1_
#define	ZLFNC2		zlfnc2_
#define	ZLFNC3		zlfnc3_
#define	ZLFNC4		zlfnc4_
#define	ZLFNC5		zlfnc5_
#define	ZLFNC6		zlfnc6_
#define	ZLFNC7		zlfnc7_
#define	ZLFNC8		zlfnc8_
#define	ZLFNC9		zlfnc9_
#define	ZLFNCA		zlfnca_
#define	ZPFNC0		zpfnc0_
#define	ZPFNC1		zpfnc1_
#define	ZPFNC2		zpfnc2_
#define	ZPFNC3		zpfnc3_
#define	ZPFNC4		zpfnc4_
#define	ZPFNC5		zpfnc5_
#define	ZPFNC6		zpfnc6_
#define	ZPFNC7		zpfnc7_
#define	ZPFNC8		zpfnc8_
#define	ZPFNC9		zpfnc9_
#define	ZPFNCA		zpfnca_
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
#define	ZTTYSZ		zttysz_
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

extern int ZARDBF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
/* ZARDGD -> gdev/ */
extern int ZARDKS ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDLP ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDND ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDPL ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDPR ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDSF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRBF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
/* ZAWRGD -> gdev/ */
extern int ZAWRKS ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRLP ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRND ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRPL ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRPR ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRSF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWSET ( XSIZE_T *, XSIZE_T *, XSIZE_T *, XSIZE_T * );
extern int ZAWTBF ( XINT *, XLONG * );
/* ZAWTGD -> gdev/ */
extern int ZAWTKS ( XINT *, XLONG * );
extern int ZAWTLP ( XINT *, XLONG * );
extern int ZAWTND ( XINT *, XLONG * );
extern int ZAWTPL ( XINT *, XLONG * );
extern int ZAWTPR ( XINT *, XLONG * );
extern int ZAWTSF ( XINT *, XLONG * );
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
extern int ZFLSTX ( XINT *, XLONG * );
extern int ZFLSTY ( XINT *, XLONG * );
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
extern XLONG ZLFNC0 ( XPOINTER * );
extern XLONG ZLFNC1 ( XPOINTER *, void * );
extern XLONG ZLFNC2 ( XPOINTER *, void *, void * );
extern XLONG ZLFNC3 ( XPOINTER *, void *, void *, void * );
extern XLONG ZLFNC4 ( XPOINTER *, void *, void *, void *, void * );
extern XLONG ZLFNC5 ( XPOINTER *, void *, void *, void *, void *, void * );
extern XLONG ZLFNC6 ( XPOINTER *, void *, void *, void *, void *, void *, void * );
extern XLONG ZLFNC7 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void * );
extern XLONG ZLFNC8 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XLONG ZLFNC9 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XLONG ZLFNCA ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNC0 ( XPOINTER * );
extern XPOINTER ZPFNC1 ( XPOINTER *, void * );
extern XPOINTER ZPFNC2 ( XPOINTER *, void *, void * );
extern XPOINTER ZPFNC3 ( XPOINTER *, void *, void *, void * );
extern XPOINTER ZPFNC4 ( XPOINTER *, void *, void *, void *, void * );
extern XPOINTER ZPFNC5 ( XPOINTER *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNC6 ( XPOINTER *, void *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNC7 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNC8 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNC9 ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern XPOINTER ZPFNCA ( XPOINTER *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern int ZFUTIM ( PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZFXDIR ( XCHAR *, XCHAR *, XINT *, XINT *);
extern int ZGCMDL ( PKCHAR *, XINT *, XINT * );
/* ??? ZGETTT ??? */
extern int ZGETTX ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZGETTY ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZGFDIR ( XINT *, PKCHAR *, XINT *, XINT * );
extern int ZGHOST ( PKCHAR *, XINT * );
extern int ZGMTCO ( XINT * );
extern int ZGTENV ( PKCHAR *, PKCHAR *, XINT *, XINT * );
extern int ZGTIME ( XLONG *, XLONG * );
extern int ZGTPID ( XINT * );
extern int ZINTPR ( XINT *, XINT *, XINT * );
extern int ZLOCPR ( PFU, XPOINTER * );
extern int ZLOCVA ( void *, XPOINTER * );
extern int ZMALOC ( XPOINTER *, XSIZE_T *, XINT * );
/* ??? ZMEMCK ??? */
extern int ZMFREE ( XPOINTER *, XINT * );
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
extern int ZPUTTX ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZPUTTY ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZRALOC ( XPOINTER *, XSIZE_T *, XINT * );
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
extern int ZTTYSZ ( XINT *, XINT *, XINT * );
extern int ZWMSEC ( XINT * );
extern int ZXGMES ( XINT *, PKCHAR *, XINT * );
extern int ZXWHEN ( XINT *, XPOINTER *, XPOINTER * );
extern int ZZCLMT ( XINT *, XLONG *, XINT * );
extern int ZZEPRO ( void );
extern int ZZOPMT ( PKCHAR *, XINT *, PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZZRDMT ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZZRWMT ( PKCHAR *, PKCHAR *, XINT * );
extern int ZZSETK ( XCHAR *, XCHAR *, XINT *, XINT *, XINT *, XINT * );
extern int ZZSTMT ( XINT *, XINT *, XLONG * );
extern int ZZSTOP ( void );
extern int ZZSTRT ( void );
extern int ZZWRMT ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZZWTMT ( XINT *, XLONG *, XLONG * );


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
extern int ZARDND ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRND ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
/* ??? ZAWTNT ??? */
extern int ZCLSND ( XINT *, XINT * );
extern int ZFCHDR ( PKCHAR *, XINT *);
extern int ZFGCWD ( PKCHAR *, XINT *, XINT * );
extern int ZFPATH ( XCHAR *, XCHAR *, XINT *, XINT * );
extern int ZFSUBD ( XCHAR *, XINT *, XCHAR *, XINT * );
extern int ZOPNND ( PKCHAR *, XINT *, XINT * );
extern int ZSTTND ( XINT *, XINT *, XLONG * );
extern int ZARDBF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDLP ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDPL ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDPR ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZARDSF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRBF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRLP ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRPL ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRPR ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWRSF ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZAWTBF ( XINT *, XLONG * );
extern int ZAWTLP ( XINT *, XLONG * );
extern int ZAWTPL ( XINT *, XLONG * );
extern int ZAWTPR ( XINT *, XLONG * );
extern int ZAWTSF ( XINT *, XLONG * );
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
extern int ZFLSTX ( XINT *, XLONG * );
extern int ZFLSTY ( XINT *, XLONG * );
extern int ZFMKCP ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFMKDR ( PKCHAR *, XINT * );
extern int ZFPROT ( PKCHAR *, XINT *, XINT * );
extern int ZFRNAM ( PKCHAR *, PKCHAR *, XINT * );
extern int ZFUTIM ( PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZGETTX ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZGETTY ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
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
extern int ZPUTTX ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZPUTTY ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZSEKTX ( XINT *, XLONG *, XINT * );
extern int ZSEKTY ( XINT *, XLONG *, XINT * );
extern int ZSTTBF ( XINT *, XINT *, XLONG * );
extern int ZSTTLP ( XINT *, XINT *, XLONG * );
extern int ZSTTPL ( XINT *, XINT *, XLONG * );
extern int ZSTTPR ( XINT *, XINT *, XLONG * );
extern int ZSTTSF ( XINT *, XINT *, XLONG * );
extern int ZSTTTX ( XINT *, XINT *, XLONG * );
extern int ZSTTTY ( XINT *, XINT *, XLONG * );
extern int ZZCLMT ( XINT *, XLONG *, XINT * );
extern int ZZOPMT ( PKCHAR *, XINT *, PKCHAR *, XLONG *, XLONG *, XINT * );
extern int ZZRDMT ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZZRWMT ( PKCHAR *, PKCHAR *, XINT * );
extern int ZZSTMT ( XINT *, XINT *, XLONG * );
extern int ZZWRMT ( XINT *, XCHAR *, XSIZE_T *, XLONG * );
extern int ZZWTMT ( XINT *, XLONG *, XLONG * );

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
#define	I32TO64		i32to4_
#define	I64TO32		i64to2_
#define	MIILEN		miilen_
#define	MIIPAK		miipak_
#define	MIIUPK		miiupk_
#define	MIIPAK8		miipa8_
#define	MIIPAK16	miipa6_
#define	MIIPAK32	miipa2_
#define	MIIPAK64	miipa4_
#define	MIIPAKR		miipar_
#define	MIIPAKD		miipad_
#define	MIIUPK8		miiup8_
#define	MIIUPK16	miiup6_
#define	MIIUPK32	miiup2_
#define	MIIUPK64	miiup4_
#define	MIIUPKR		miiupr_
#define	MIIUPKD		miiupd_
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
#define	F77PAK		f77pak_
#define	XORI		xori_
#define	XORL		xorl_
#define	XORS		xors_

#define	SMOD		smod_
#define	IMOD		imod_
#define	LMOD		lmod_
#define	PMOD		pmod_
#define	SABS		sabs_
#define	LABS		labs_
#define	PABS		pabs_
#define	AABS		aabs_
#define	SNINT		snint_
#define	SDNINT		sdnint_
#define	ININT		inint_
#define	LNINT		lnint_
#define	LDNINT		ldnint_
#define	SINT		sint_
#define	SDINT		sdint_
#define	IINT		iint_
#define	LINT		lint_
#define	LDINT		ldint_

#define	ACLRC		aclrc_
#define	ACLRD		aclrd_
#define	ACLRI		aclri_
#define	ACLRL		aclrl_
#define	ACLRP		aclrp_
#define	ACLRR		aclrr_
#define	ACLRS		aclrs_
#define	AMOVC		amovc_
#define	AMOVD		amovd_
#define	AMOVI		amovi_
#define	AMOVL		amovl_
#define	AMOVP		amovp_
#define	AMOVR		amovr_
#define	AMOVS		amovs_


/* Procedure names for the potentially machine dependent VOPS vector
 * primitives.  The ACHT stands for change datatype, the B suffix refers
 * to primitives which deal with unsigned machine bytes, and the U suffix
 * refers to primitives which deal with unsigned short (16 bit) integers.
 */
#define	ACHT		acht_
#define	ACHTB		achtb_
#define	ACHTBB		achtbb_
#define	ACHTBC		achtbc_
#define	ACHTBD		achtbd_
#define	ACHTBI		achtbi_
#define	ACHTBL		achtbl_
#define	ACHTBP		achtbp_
#define	ACHTBR		achtbr_
#define	ACHTBS		achtbs_
#define	ACHTBU		achtbu_
#define	ACHTBX		achtbx_
#define	ACHTC		achtc_
#define	ACHTCB		achtcb_
#define	ACHTCC		achtcc_
#define	ACHTCD		achtcd_
#define	ACHTCI		achtci_
#define	ACHTCL		achtcl_
#define	ACHTCP		achtcp_
#define	ACHTCR		achtcr_
#define	ACHTCS		achtcs_
#define	ACHTCU		achtcu_
#define	ACHTCX		achtcx_
#define	ACHTD		achtd_
#define	ACHTDB		achtdb_
#define	ACHTDC		achtdc_
#define	ACHTDD		achtdd_
#define	ACHTDI		achtdi_
#define	ACHTDL		achtdl_
#define	ACHTDP		achtdp_
#define	ACHTDR		achtdr_
#define	ACHTDS		achtds_
#define	ACHTDU		achtdu_
#define	ACHTDX		achtdx_
#define	ACHTI		achti_
#define	ACHTIB		achtib_
#define	ACHTIC		achtic_
#define	ACHTID		achtid_
#define	ACHTII		achtii_
#define	ACHTIL		achtil_
#define	ACHTIP		achtip_
#define	ACHTIR		achtir_
#define	ACHTIS		achtis_
#define	ACHTIU		achtiu_
#define	ACHTIX		achtix_
#define	ACHTL		achtl_
#define	ACHTLB		achtlb_
#define	ACHTLC		achtlc_
#define	ACHTLD		achtld_
#define	ACHTLI		achtli_
#define	ACHTLL		achtll_
#define	ACHTLP		achtlp_
#define	ACHTLR		achtlr_
#define	ACHTLS		achtls_
#define	ACHTLU		achtlu_
#define	ACHTLX		achtlx_
#define	ACHTP		achtp_
#define	ACHTPB		achtpb_
#define	ACHTPC		achtpc_
#define	ACHTPD		achtpd_
#define	ACHTPI		achtpi_
#define	ACHTPL		achtpl_
#define	ACHTPP		achtpp_
#define	ACHTPR		achtpr_
#define	ACHTPS		achtps_
#define	ACHTPU		achtpu_
#define	ACHTPX		achtpx_
#define	ACHTR		achtr_
#define	ACHTRB		achtrb_
#define	ACHTRC		achtrc_
#define	ACHTRD		achtrd_
#define	ACHTRI		achtri_
#define	ACHTRL		achtrl_
#define	ACHTRP		achtrp_
#define	ACHTRR		achtrr_
#define	ACHTRS		achtrs_
#define	ACHTRU		achtru_
#define	ACHTRX		achtrx_
#define	ACHTS		achts_
#define	ACHTSB		achtsb_
#define	ACHTSC		achtsc_
#define	ACHTSD		achtsd_
#define	ACHTSI		achtsi_
#define	ACHTSL		achtsl_
#define	ACHTSP		achtsp_
#define	ACHTSR		achtsr_
#define	ACHTSS		achtss_
#define	ACHTSU		achtsu_
#define	ACHTSX		achtsx_
#define	ACHTU		achtu_
#define	ACHTUB		achtub_
#define	ACHTUC		achtuc_
#define	ACHTUD		achtud_
#define	ACHTUI		achtui_
#define	ACHTUL		achtul_
#define	ACHTUP		achtup_
#define	ACHTUR		achtur_
#define	ACHTUS		achtus_
#define	ACHTUU		achtuu_
#define	ACHTUX		achtux_
#define	ACHTX		achtx_
#define	ACHTXB		achtxb_
#define	ACHTXC		achtxc_
#define	ACHTXD		achtxd_
#define	ACHTXI		achtxi_
#define	ACHTXL		achtxl_
#define	ACHTXP		achtxp_
#define	ACHTXR		achtxr_
#define	ACHTXS		achtxs_
#define	ACHTXU		achtxu_
#define	ACHTXX		achtxx_


extern int ACLRC ( XCHAR *, XSIZE_T * );
extern int ACLRD ( XDOUBLE *, XSIZE_T * );
extern int ACLRI ( XINT *, XSIZE_T * );
extern int ACLRL ( XLONG *, XSIZE_T * );
extern int ACLRP ( XPOINTER *, XSIZE_T * );
extern int ACLRR ( XREAL *, XSIZE_T * );
extern int ACLRS ( XSHORT *, XSIZE_T * );
extern int AMOVC ( XCHAR *, XCHAR *, XSIZE_T * );
extern int AMOVD ( XDOUBLE *, XDOUBLE *, XSIZE_T * );
extern int AMOVI ( XINT *, XINT *, XSIZE_T * );
extern int AMOVL ( XLONG *, XLONG *, XSIZE_T * );
extern int AMOVP ( XPOINTER *, XPOINTER *, XSIZE_T * );
extern int AMOVR ( XREAL *, XREAL *, XSIZE_T * );
extern int AMOVS ( XSHORT *, XSHORT *, XSIZE_T * );

extern int ACHT ( void *, void *, XSIZE_T *, XINT *, XINT * );
extern int ACHTB ( XUBYTE *, void *, XSIZE_T *, XINT * );
extern int ACHTBB ( XUBYTE *, XUBYTE *, XSIZE_T * );
extern int ACHTBC ( XUBYTE *, XCHAR *, XSIZE_T * );
extern int ACHTBD ( XUBYTE *, XDOUBLE *, XSIZE_T * );
extern int ACHTBI ( XUBYTE *, XINT *, XSIZE_T * );
extern int ACHTBL ( XUBYTE *, XLONG *, XSIZE_T * );
extern int ACHTBP ( XUBYTE *, XPOINTER *, XSIZE_T * );
extern int ACHTBR ( XUBYTE *, XREAL *, XSIZE_T * );
extern int ACHTBS ( XUBYTE *, XSHORT *, XSIZE_T * );
extern int ACHTBU ( XUBYTE *, XUSHORT *, XSIZE_T * );
extern int ACHTBX ( XUBYTE *, XCOMPLEX *, XSIZE_T * );
extern int ACHTC ( XCHAR *, void *, XSIZE_T *, XINT * );
extern int ACHTCB ( XCHAR *, XUBYTE *, XSIZE_T * );
extern int ACHTCC ( XCHAR *, XCHAR *, XSIZE_T * );
extern int ACHTCD ( XCHAR *, XDOUBLE *, XSIZE_T * );
extern int ACHTCI ( XCHAR *, XINT *, XSIZE_T * );
extern int ACHTCL ( XCHAR *, XLONG *, XSIZE_T * );
extern int ACHTCP ( XCHAR *, XPOINTER *, XSIZE_T * );
extern int ACHTCR ( XCHAR *, XREAL *, XSIZE_T * );
extern int ACHTCS ( XCHAR *, XSHORT *, XSIZE_T * );
extern int ACHTCU ( XCHAR *, XUSHORT *, XSIZE_T * );
extern int ACHTCX ( XCHAR *, XCOMPLEX *, XSIZE_T * );
extern int ACHTD ( XDOUBLE *, void *, XSIZE_T *, XINT * );
extern int ACHTDB ( XDOUBLE *, XUBYTE *, XSIZE_T * );
extern int ACHTDC ( XDOUBLE *, XCHAR *, XSIZE_T * );
extern int ACHTDD ( XDOUBLE *, XDOUBLE *, XSIZE_T * );
extern int ACHTDI ( XDOUBLE *, XINT *, XSIZE_T * );
extern int ACHTDL ( XDOUBLE *, XLONG *, XSIZE_T * );
extern int ACHTDP ( XDOUBLE *, XPOINTER *, XSIZE_T * );
extern int ACHTDR ( XDOUBLE *, XREAL *, XSIZE_T * );
extern int ACHTDS ( XDOUBLE *, XSHORT *, XSIZE_T * );
extern int ACHTDU ( XDOUBLE *, XUSHORT *, XSIZE_T * );
extern int ACHTDX ( XDOUBLE *, XCOMPLEX *, XSIZE_T * );
extern int ACHTI ( XINT *a, void *, XSIZE_T *, XINT * );
extern int ACHTIB ( XINT *, XUBYTE *, XSIZE_T * );
extern int ACHTIC ( XINT *, XCHAR *, XSIZE_T * );
extern int ACHTID ( XINT *, XDOUBLE *, XSIZE_T * );
extern int ACHTII ( XINT *, XINT *, XSIZE_T * );
extern int ACHTIL ( XINT *, XLONG *, XSIZE_T * );
extern int ACHTIP ( XINT *, XPOINTER *, XSIZE_T * );
extern int ACHTIR ( XINT *, XREAL *, XSIZE_T * );
extern int ACHTIS ( XINT *, XSHORT *, XSIZE_T * );
extern int ACHTIU ( XINT *, XUSHORT *, XSIZE_T * );
extern int ACHTIX ( XINT *, XCOMPLEX *, XSIZE_T * );
extern int ACHTL ( XLONG *, void *, XSIZE_T *, XINT * );
extern int ACHTLB ( XLONG *, XUBYTE *, XSIZE_T * );
extern int ACHTLC ( XLONG *, XCHAR *, XSIZE_T * );
extern int ACHTLD ( XLONG *, XDOUBLE *, XSIZE_T * );
extern int ACHTLI ( XLONG *, XINT *, XSIZE_T * );
extern int ACHTLL ( XLONG *, XLONG *, XSIZE_T * );
extern int ACHTLP ( XLONG *, XPOINTER *, XSIZE_T * );
extern int ACHTLR ( XLONG *, XREAL *, XSIZE_T * );
extern int ACHTLS ( XLONG *, XSHORT *, XSIZE_T * );
extern int ACHTLU ( XLONG *, XUSHORT *, XSIZE_T * );
extern int ACHTLX ( XLONG *, XCOMPLEX *, XSIZE_T * );
extern int ACHTP ( XPOINTER *, void *, XSIZE_T *, XINT * );
extern int ACHTPB ( XPOINTER *, XUBYTE *, XSIZE_T * );
extern int ACHTPC ( XPOINTER *, XCHAR *, XSIZE_T * );
extern int ACHTPD ( XPOINTER *, XDOUBLE *, XSIZE_T * );
extern int ACHTPI ( XPOINTER *, XINT *, XSIZE_T * );
extern int ACHTPL ( XPOINTER *, XLONG *, XSIZE_T * );
extern int ACHTPP ( XPOINTER *, XPOINTER *, XSIZE_T * );
extern int ACHTPR ( XPOINTER *, XREAL *, XSIZE_T * );
extern int ACHTPS ( XPOINTER *, XSHORT *, XSIZE_T * );
extern int ACHTPU ( XPOINTER *, XUSHORT *, XSIZE_T * );
extern int ACHTPX ( XPOINTER *, XCOMPLEX *, XSIZE_T * );
extern int ACHTR ( XREAL *, void *, XSIZE_T *, XINT * );
extern int ACHTRB ( XREAL *, XUBYTE *, XSIZE_T * );
extern int ACHTRC ( XREAL *, XCHAR *, XSIZE_T * );
extern int ACHTRD ( XREAL *, XDOUBLE *, XSIZE_T * );
extern int ACHTRI ( XREAL *, XINT *, XSIZE_T * );
extern int ACHTRL ( XREAL *, XLONG *, XSIZE_T * );
extern int ACHTRP ( XREAL *, XPOINTER *, XSIZE_T * );
extern int ACHTRR ( XREAL *, XREAL *, XSIZE_T * );
extern int ACHTRS ( XREAL *, XSHORT *, XSIZE_T * );
extern int ACHTRU ( XREAL *, XUSHORT *, XSIZE_T * );
extern int ACHTRX ( XREAL *, XCOMPLEX *, XSIZE_T * );
extern int ACHTS ( XSHORT *, void *, XSIZE_T *, XINT * );
extern int ACHTSB ( XSHORT *, XUBYTE *, XSIZE_T * );
extern int ACHTSC ( XSHORT *, XCHAR *, XSIZE_T * );
extern int ACHTSD ( XSHORT *, XDOUBLE *, XSIZE_T * );
extern int ACHTSI ( XSHORT *, XINT *, XSIZE_T * );
extern int ACHTSL ( XSHORT *, XLONG *, XSIZE_T * );
extern int ACHTSP ( XSHORT *, XPOINTER *, XSIZE_T * );
extern int ACHTSR ( XSHORT *, XREAL *, XSIZE_T * );
extern int ACHTSS ( XSHORT *, XSHORT *, XSIZE_T *);
extern int ACHTSU ( XSHORT *, XUSHORT *, XSIZE_T * );
extern int ACHTSX ( XSHORT *, XCOMPLEX *, XSIZE_T * );
extern int ACHTU ( XUSHORT *, void *, XSIZE_T *, XINT * );
extern int ACHTUB ( XUSHORT *, XUBYTE *, XSIZE_T * );
extern int ACHTUC ( XUSHORT *, XCHAR *, XSIZE_T * );
extern int ACHTUD ( XUSHORT *, XDOUBLE *, XSIZE_T * );
extern int ACHTUI ( XUSHORT *, XINT *, XSIZE_T * );
extern int ACHTUL ( XUSHORT *, XLONG *, XSIZE_T * );
extern int ACHTUP ( XUSHORT *, XPOINTER *, XSIZE_T * );
extern int ACHTUR ( XUSHORT *, XREAL *, XSIZE_T * );
extern int ACHTUS ( XUSHORT *, XSHORT *, XSIZE_T * );
extern int ACHTUU ( XUSHORT *, XUSHORT *, XSIZE_T * );
extern int ACHTUX ( XUSHORT *, XCOMPLEX *, XSIZE_T * );
extern int ACHTX ( XCOMPLEX *, void *, XSIZE_T *, XINT * );
extern int ACHTXB ( XCOMPLEX *, XUBYTE *, XSIZE_T * );
extern int ACHTXC ( XCOMPLEX *, XCHAR *, XSIZE_T * );
extern int ACHTXD ( XCOMPLEX *, XDOUBLE *, XSIZE_T * );
extern int ACHTXI ( XCOMPLEX *, XINT *, XSIZE_T * );
extern int ACHTXL ( XCOMPLEX *, XLONG *, XSIZE_T * );
extern int ACHTXP ( XCOMPLEX *, XPOINTER *, XSIZE_T * );
extern int ACHTXR ( XCOMPLEX *, XREAL *, XSIZE_T * );
extern int ACHTXS ( XCOMPLEX *, XSHORT *, XSIZE_T * );
extern int ACHTXU ( XCOMPLEX *, XUSHORT *, XSIZE_T * );
extern int ACHTXX ( XCOMPLEX *, XCOMPLEX *, XSIZE_T * );

extern int ACLRB ( void *, XSIZE_T * );
extern int BSWAP2 ( void *, XSIZE_T *, void *, XSIZE_T *, XSIZE_T * );
extern int BSWAP4 ( void *, XSIZE_T *, void *, XSIZE_T *, XSIZE_T * );
extern int BSWAP8 ( void *, XSIZE_T *, void *, XSIZE_T *, XSIZE_T * );
extern int BYTMOV ( void *, XSIZE_T *, void *, XSIZE_T *, XSIZE_T * );
extern int I32TO64 ( void *, void *, XSIZE_T * );
extern int I64TO32 ( void *, void *, XSIZE_T * );

extern int MIIPAK ( void *, void *, XSIZE_T *, XINT *, XINT * );
extern int MIIUPK ( void *, void *, XSIZE_T *, XINT *, XINT * );
extern int MIIPAK8 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIPAK16 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIPAK32 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIPAK64 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIPAKR ( void *, void *, XSIZE_T *, XINT * );
extern int MIIPAKD ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPK8 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPK16 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPK32 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPK64 ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPKR ( void *, void *, XSIZE_T *, XINT * );
extern int MIIUPKD ( void *, void *, XSIZE_T *, XINT * );

extern int CHRPAK ( XCHAR *, XSIZE_T *, XCHAR *, XSIZE_T *, XSIZE_T * );
extern int CHRUPK ( XCHAR *, XSIZE_T *, XCHAR *, XSIZE_T *, XSIZE_T * );
extern int STRPAK ( XCHAR *, PKCHAR *, XSIZE_T * );
extern int STRUPK ( PKCHAR *, XCHAR *, XSIZE_T * );

extern XINT ANDI ( XINT *, XINT * );
extern XSHORT ANDS ( XSHORT *, XSHORT * );
extern XLONG ANDL ( XLONG *, XLONG * );
extern int BITPAK ( XINT *, XINT *, XINT *, XINT * );
extern XINT BITUPK ( XINT *, XINT *, XINT * );

extern XINT NOTI ( XINT * );
extern XSHORT NOTS ( XSHORT * );
extern XLONG NOTL ( XLONG * );
extern XINT ORI ( XINT *, XINT * );
extern XSHORT ORS ( XSHORT *, XSHORT * );
extern XLONG ORL ( XLONG *, XLONG * );
extern XINT SHIFTI ( XINT *, XINT * );
extern XSHORT SHIFTS ( XSHORT *, XSHORT * );
extern XLONG SHIFTL ( XLONG *, XLONG * );

extern XSHORT SMOD ( XSHORT *, XSHORT * );
extern XINT IMOD ( XINT *, XINT * );
extern XLONG LMOD ( XLONG *, XLONG * );
extern XPOINTER PMOD ( XPOINTER *, XPOINTER * );
extern XSHORT SABS ( XSHORT * );
extern XLONG LABS ( XLONG * );
extern XPOINTER PABS ( XPOINTER * );
extern XREAL AABS ( XREAL * );
extern XSHORT SNINT ( XREAL * );
extern XSHORT SDNINT ( XDOUBLE * );
extern XINT ININT ( XREAL * );
extern XLONG LNINT ( XREAL * );
extern XLONG LDNINT ( XDOUBLE * );
extern XSHORT SINT ( XREAL * );
extern XSHORT SDINT ( XDOUBLE * );
extern XINT IINT ( XREAL * );
extern XLONG LINT ( XREAL * );
extern XLONG LDINT ( XDOUBLE * );

#endif	/* ! _IRAF_KNAMES_H */
