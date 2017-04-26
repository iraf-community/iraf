
#  CDL_SPP.H  -- Header file for the CDL SPP interface.  Fortran compilers
#  on various platforms may append one or more trailing underscores to 
#  symbol names, we'll use macros for the interface names and use defines
#  to see what the symbol name is.


define	FB_AUTO		-1		# autoconfig the frame buffer

# Types of greyscale transformations.
define 	CDL_UNITARY       0            	# values map without change
define 	CDL_LINEAR        1            	# linear mapping
define 	CDL_LOG           2            	# logarithmic mapping

# Overlay colors.
define	C_BLACK		202		# static overlay color defs
define	C_WHITE		203
define	C_RED		204
define	C_GREEN		205
define	C_BLUE		206
define  C_YELLOW	207
define	C_CYAN		208
define  C_MAGENTA	209
define  C_CORAL		210
define	C_MAROON	211
define  C_ORANGE	212
define	C_KHAKI		213
define  C_ORCHID	214
define  C_TURQUOISE	215
define  C_VIOLET	216
define	C_WHEAT		217

# Overlay point mark types.
define 	M_FILL             1
define 	M_POINT            2
define 	M_BOX              4
define 	M_PLUS             8
define 	M_CROSS           16
define 	M_DIAMOND         32
define 	M_CIRCLE          64
define 	M_STAR           128
define 	M_HLINE          256
define 	M_VLINE          512
define 	M_HBLINE        1024
define 	M_VBLINE        2048

# Font types.
define F_ROMAN            0
define F_GREEK            1
define F_FUTURA           2
define F_TIMES            3
define F_BOLD             4

# Polyline attribute values.
define L_SOLID            0            
define L_DASHED           1
define L_DOTTED           2
define L_DOTDASH          3
define L_HOLLOW           4
define L_SHADOW           5


# Map the SPP names to the symbols in the library.
define	cdl_open		cdopen_
define	cdl_displayPix		cdsppx_
define	cdl_readCursor		crdcur_
define	cdl_setCursor		cscurs_
define	cdl_clearFrame		cclfrm_
define	cdl_selectFB		cselfb_
define	cdl_close		cclose_
define	cdl_displayIRAF		cdspir_
define	cdl_isIRAF		cisirf_
define	cdl_readIRAF		crdirf_
define	cdl_displayFITS		cdspft_
define	cdl_isFITS		cisfts_
define	cdl_readFITS		crdfts_
define	cdl_compZScale		ccmpzs_
define	cdl_zscaleImage		czscim_
define	cdl_printPix		cprpix_
define	cdl_printPixToFile	cprpfl_
define	cdl_readImage		crdimg_
define	cdl_readFrameBuffer	crdfrb_
define	cdl_readSubRaster	crsubr_
define	cdl_writesubRaster	cwsubr_
define	cdl_setWCS		cstwcs_
define	cdl_setFBConfig		csfbcf_
define	cdl_getFBConfig		cgfbcf_
define	cdl_lookupFBSize	clkfbs_
define	cdl_setFrame		csfram_
define	cdl_setZTrans		csztrn_
define	cdl_setZScale		cszscl_
define	cdl_setSample		cssamp_
define	cdl_setSampleLines	cssaml_
define	cdl_setContrast		cscntr_
define	cdl_setName		csname_
define	cdl_setTitle		cstitl_
define	cdl_getWCS		cgtwcs_
define	cdl_getFrame		cgfram_
define	cdl_getZTrans		cgztrn_
define	cdl_getZScale		cgzscl_
define	cdl_getSample		cgsamp_
define	cdl_getSampleLines	cgsmpl_
define	cdl_getContrast		cgcntr_
define	cdl_getName		cgname_
define	cdl_getTitle		cgtitl_
define	cdl_mapFrame		cmapfr_
define	cdl_markCoordsFile	cmkcfl_
define	cdl_markPoint		cmkpnt_
define	cdl_markPointLabel	cmkpnl_
define	cdl_markLine		cmklin_
define	cdl_markBox		cmkbox_
define	cdl_markPolyline	cmkpln_
define	cdl_markPolygon		cmkpgn_
define	cdl_markCircle		cmkcrc_
define	cdl_markCircAnnuli	cmkcan_
define	cdl_markEllipse		cmkell_
define	cdl_markEllipAnnuli	cmkela_
define	cdl_markText		cmktxt_
define	cdl_setFont		csfont_
define	cdl_setLineWidth	cslwid_
define	cdl_setLineStyle	cslsty_
define	cdl_setTextWidth	cstwid_
define	cdl_deleteMark		cdelmk_
define	cdl_clearOverlay	cclrov_
define	cdl_redrawOverlay	crdrov_
define	cdl_setDebug		cstdbg_
define  cdl_setMapping          cstmap_
define  cdl_getMapping          cgtmap_
define  cdl_queryMap            cqrmap_


