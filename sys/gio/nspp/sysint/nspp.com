# NSPP.COM -- The nspp system plot package common block.

int	mmajx  ,mmajy  ,mminx  ,mminy  ,mxlab  ,mylab
int	mflg   ,mtype  ,mxa    ,mya    ,mxb    ,myb
int	mx     ,my     ,mtypex ,mtypey 
real	xxa    ,yya    , xxb    ,yyb    ,xxc    ,yyc
real	xxd    ,yyd    , xfactr ,yfactr ,xadd   ,yadd
real	xx     ,yy

# XX declared integer some places in nspp code !!!
# on a VAX this works, but what if float not same size as int ???

int	mfmtx[3]       ,mfmty[3]       ,mumx   ,mumy
int	msizx  ,msizy  ,mxdec  ,mydec  ,mxor   ,mop[19]
int	mname[19]      ,mxold  ,myold  ,mxmax  ,mymax
int	mxfac  ,myfac  ,modef  ,mf2er  ,mshftx ,mshfty
int	mmgrx  ,mmgry  ,mmnrx  ,mmnry  ,mfrend ,mfrlst
int	mcrout ,mpair1 ,mpair2 ,msblen ,mflcnt ,mjxmin
int	mjymin ,mjxmax ,mjymax ,mnxsto ,mnysto ,mxxsto
int	mxysto ,mprint ,msybuf[360]    ,mncpw  ,minst
int	mbufa  ,mbuflu ,mfwa[12]       ,mlwa[12]
int	mipair ,mbprs[16]      ,mbufl  ,munit  ,mbswap

real	small

common /sysplt/ mmajx  ,mmajy  ,mminx  ,mminy  ,mxlab  ,mylab,
	mflg   ,mtype  ,mxa    ,mya    ,mxb    ,myb,
	mx     ,my     ,mtypex ,mtypey ,xxa    ,yya,
	xxb    ,yyb    ,xxc    ,yyc    ,xxd    ,yyd,
	xfactr ,yfactr ,xadd   ,yadd   ,xx     ,yy,
	mfmtx  ,mfmty  ,mumx   ,mumy,
	msizx  ,msizy  ,mxdec  ,mydec  ,mxor   ,mop,
	mname  ,mxold  ,myold  ,mxmax  ,mymax,
	mxfac  ,myfac  ,modef  ,mf2er  ,mshftx ,mshfty,
	mmgrx  ,mmgry  ,mmnrx  ,mmnry  ,mfrend ,mfrlst,
	mcrout ,mpair1 ,mpair2 ,msblen ,mflcnt ,mjxmin,
	mjymin ,mjxmax ,mjymax ,mnxsto ,mnysto ,mxxsto,
	mxysto ,mprint ,msybuf ,mncpw  ,minst,
	mbufa  ,mbuflu ,mfwa   ,mlwa,
	mipair ,mbprs  ,mbufl  ,munit  ,mbswap ,small
