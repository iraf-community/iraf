# ALLSTAR Structure

define	LEN_ALLSTARSTRUCT (100)

define	DP_ISCACHE	Memi[$1]	# is data cached (default no)
define	DP_ALLCACHE	Memi[$1+2]	# is all the data cached ? (yes)
define	DP_CACHE	Memi[$1+3+$2-1] # which data is cached ?
define	DP_SZCACHE	Memi[$1+6]	# current working set size
define	DP_SZOLDCACHE	Memi[$1+7]	# old working set size
define	DP_DATA		Memi[$1+8]	# pointer to data
define	DP_SUBT		Memi[$1+9]	# pointer to subt
define	DP_WEIGHTS	Memi[$1+10]	# pointer to weights

define	DP_SBUF		Memi[$1+13]	# local subt buffer
define	DP_SNX		Memi[$1+14]	# subt x dimension
define	DP_SNY		Memi[$1+15]	# subt y dimension
define	DP_SLX		Memi[$1+16]	# subt lower left x coord
define	DP_SMX		Memi[$1+17]	# subt lower right x coord
define	DP_SLY		Memi[$1+18]	# subt lower left y coord
define	DP_SMY		Memi[$1+19]	# subt lower right y coord
define	DP_SXOFF	Memi[$1+20]	# subt lower left x offset
define	DP_SYOFF	Memi[$1+21]	# subt lower left y offset

define	DP_WBUF		Memi[$1+23]	# local weight buffer
define	DP_WNX		Memi[$1+24]	# weight x dimension
define	DP_WNY		Memi[$1+25]	# weight y dimension
define	DP_WLX		Memi[$1+26]	# weight lower left x coord
define	DP_WMX		Memi[$1+27]	# weight lower right x coord
define	DP_WLY		Memi[$1+28]	# weight lower left y coord
define	DP_WMY		Memi[$1+29]	# weight lower right y coord
define	DP_WXOFF	Memi[$1+30]	# weight lower left x offset
define	DP_WYOFF	Memi[$1+31]	# weight lower left y offset

define	DP_DBUF		Memi[$1+33]	# local weight buffer
define	DP_DNX		Memi[$1+34]	# weight x dimension
define	DP_DNY		Memi[$1+35]	# weight y dimension
define	DP_DLX		Memi[$1+36]	# weight lower left x coord
define	DP_DMX		Memi[$1+37]	# weight lower right x coord
define	DP_DLY		Memi[$1+38]	# weight lower left y coord
define	DP_DMY		Memi[$1+39]	# weight lower right y coord
define	DP_DXOFF	Memi[$1+40]	# weight lower left x offset
define	DP_DYOFF	Memi[$1+41]	# weight lower left y offset

define	DP_ANUMER1	Memi[$1+55]	# pointer to the anumer1 directory
define	DP_ANUMER2	Memi[$1+56]	# pointer to the anumer2 directory
define	DP_ADENOM1	Memi[$1+57]	# pointer to the adenom1 directory
define	DP_ADENOM2	Memi[$1+58]	# pointer to the adenom2 directory
define	DP_ARPIXSQ	Memi[$1+59]	# pointer to the rpixsq directory
define	DP_ASUMWT	Memi[$1+60]	# pointer to the sumwt directory
define	DP_ASKIP	Memi[$1+61]	# pointer to the skip directory
define	DP_ALAST	Memi[$1+62]	# pointer to the last directory
define	DP_AXOLD	Memi[$1+63]	# pointer to the xold array
define	DP_AYOLD	Memi[$1+64]	# pointer to the yold array
define	DP_AXCLAMP	Memi[$1+65]	# pointer to the xclamp array
define	DP_AYCLAMP	Memi[$1+66]	# pointer to the yclamp array
define	DP_AX		Memi[$1+67]	# pointer to the x array
define	DP_AV		Memi[$1+68]	# pointer to the v array
define	DP_AC		Memi[$1+69]	# pointer to the c array
define	DP_ANPIX	Memi[$1+70]	# pointer to the npix array

define	A_SUBT		1
define	A_WEIGHT	2
define	A_DCOPY		3

define	RO32K	32767.49
