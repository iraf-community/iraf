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

#define	IRAF_MAIN	irafmn_
#define	ZZSETK		zzsetk_
#define	USHLIB		ushlib_
#define	VSHLIB		vshlib_
#define	VSHEND		vshend_
#define	VLIBINIT	vlibinit_
#define	KI_CONNECT	kicont_
#define	KI_GETHOSTS	kigets_
#define	KI_SEND		kisend_
#define	KI_RECEIVE	kirece_

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
#define	ZFLINK		zflink_
#define	ZFMKCP		zfmkcp_
#define	ZFMKDR		zfmkdr_
#define	ZFNBRK		zfnbrk_
#define	ZFODPR		zfodpr_
#define	ZFPATH		zfpath_
#define	ZFPROT		zfprot_
#define ZFPOLL		zfpoll_
#define	ZFREE		zfree_
#define	ZFRNAM		zfrnam_
#define	ZFRMDR		zfrmdr_
#define	ZFSUBD		zfsubd_
#define	ZFULNK		zfulnk_
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
#define ZTSDPR		ztsdpr_
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
#define zfodpr_	kfodpr_
#define zfprot_	kfprot_
#define zfrnam_	kfrnam_
#define zfrmdr_	kfrmdr_
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
#define ztsdpr_	ktsdpr_
#define zzclmt_	kzclmt_
#define zzopmt_	kzopmt_
#define zzrdmt_	kzrdmt_
#define zzrwmt_	kzrwmt_
#define zzstmt_	kzstmt_
#define zzwrmt_	kzwrmt_
#define zzwtmt_	kzwtmt_


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
#define I32TO64		i32to4_
#define I64TO32		i64to2_
#define IPAK32		ipak32_
#define IUPK32		iupk32_
#define IPAK16		ipak16_
#define IUPK16		iupk16_
#define IMUL32		imul32_
#define ISCL32          iscl32_
#define ISCL64          iscl64_
#define	STRSUM		strsum_

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

/*
#define import_kproto
*/

#define	D_knames
