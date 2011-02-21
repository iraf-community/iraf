# PSF fitting task definitions

# PSF fitting parameters (# 201 - 300)

define  PNUM	        201	# number of PSF stars
define  CUR_PSF		202	# index of current PSF star
define  CUR_PSFID	203	# id of current PSF star
define  CUR_PSFX	204	# x coordinate of current PSF star
define  CUR_PSFY	205	# y coordinate of current PSF star
define  CUR_PSFSKY	206	# sky value of current PSF star
define  CUR_PSFMAG	207	# magnitude of current PSF star
define  CUR_PSFMIN	208	# min data value of current PSF star
define  CUR_PSFMAX	209	# max data value of current PSF star
define	CUR_PSFGMAX	210	# max good data value of current PSF star
define	PLOTTYPE	211	# the plot type
define	LENUSERAREA	212	# PSF image user area length

# PSF task plot types 

define	PSF_PLOTS	"|mesh|contour|radial|"	# list of plot types
define	PSF_MESHPLOT	1			# mesh plot
define	PSF_CONTOURPLOT	2			# contour plot
define	PSF_RADIALPLOT	3			# radial profile plot

# miscellaneous definitions

define	MIN_LENUSERAREA	50000	# minimum length of the user area
define	PSF_NINCOLS	4	# number of columns in input psf star list
define	PSF_NOUTCOLS	6	# number of columns in output group table
define	MAX_NPSFITER	300	# max number of analytic fit iterations

# PSF fitting colon commands

define	PSF_CMDS   "|psfimage|groupfile|function|varorder|fexpand|psfrad|\
fitrad|nclean|saturated|matchrad|scale|fwhmpsf|datamin|datamax|"

define	PSFCMD_PSFIMAGE		1
define	PSFCMD_GROUPFILE	2
define	PSFCMD_FUNCTION		3
define	PSFCMD_VARORDER		4
define	PSFCMD_FEXPAND		5
define	PSFCMD_PSFRAD		6
define	PSFCMD_FITRAD		7
define	PSFCMD_NCLEAN		8
define	PSFCMD_SATURATED	9
define	PSFCMD_MATCHRAD		10
define	PSFCMD_SCALE		11
define	PSFCMD_FWHMPSF		12
define	PSFCMD_DATAMIN		13
define	PSFCMD_DATAMAX		14

# the PSF task fitting structure

define 	LEN_PSFSTRUCT (50)

# arrays required for fitting analytic psf and the look-up table 

define	DP_LENUSERAREA	Memi[$1]	# size of the output psf user area
define	DP_PC		Memi[$1+1]	# pointer to fitting matrix
define	DP_PV		Memi[$1+3]	# pointer to fitting vector
define	DP_PTMP		Memi[$1+4]	# pointer to temporary vector
define	DP_PZ		Memi[$1+5]	# pointer to parameter changes
define	DP_PCLAMP	Memi[$1+6]	# pointer to clamp vector
define	DP_POLD		Memi[$1+7]	# pointer to previous parameter changes
define	DP_PSIGANA	Memr[P2R($1+8)]	# normalized sigma for analytic fit
define	DP_PSUMANA	Memr[P2R($1+9)]	# number of points in analytic fit

# dimensions and arrays required for psf star list

define	DP_PNUM		Memi[$1+10]	# number of stars in PSF
define	DP_PSAT		Memi[$1+11]	# pointer to PSF star saturation indices
define	DP_PXCEN	Memi[$1+12]	# pointer to the PSF star x coords
define  DP_PYCEN	Memi[$1+13]	# pointer to the PSF star y coords
define	DP_PMAG		Memi[$1+14]	# pointer to the PSF star list mags
define	DP_PH		Memi[$1+15]	# pointer to the PSF star heights
define	DP_PWEIGHT	Memi[$1+16]	# pointer to the PSF star weights
define	DP_PXCLAMP	Memi[$1+17]	# pointer to the PSF star x clamps
define	DP_PYCLAMP	Memi[$1+18]	# pointer to the PSF star y clamps
define	DP_PXOLD	Memi[$1+19]	# pointer to the old PSF star x values
define	DP_PYOLD	Memi[$1+20]	# pointer to the old PSF star y values 

# additional arrays required for fitting the look-up table

define	DP_PSUMN	Memi[$1+21]	# pointer to the number of points
define	DP_PSUMW	Memi[$1+22]	# pointer to the weights corrections
define	DP_PSUMSQ	Memi[$1+23]	# pointer to the resdiduals
define	DP_PSIGMA	Memi[$1+24]	# pointer to the standard deviations
define	DP_PCONST	Memi[$1+25]	# pointer to the const part of psf
define	DP_POLDLUT 	Memi[$1+26]	# pointer to the old lookup table

# description of current psf star

define	DP_CUR_PSF	Memi[$1+27]	# position of current PSF star in file
define	DP_CUR_PSFID	Memi[$1+28]	# id of current PSF star
define	DP_CUR_PSFX	Memr[P2R($1+29)]# x position of current PSF star
define	DP_CUR_PSFY	Memr[P2R($1+30)]# y position of current PSF star
define	DP_CUR_PSFSKY	Memr[P2R($1+31)]# sky for current PSF star
define	DP_CUR_PSFMAG	Memr[P2R($1+32)]# magnitude for current PSF star
define	DP_CUR_PSFMIN	Memr[P2R($1+33)]# minimum data value in PSF subrast
define	DP_CUR_PSFMAX	Memr[P2R($1+34)]# maximum data value in PSF subrast
define	DP_CUR_PSFGMAX	Memr[P2R($1+35)]# maximum good data value  in fitrad

# the psf plotting parameters

define	DP_PLOTTYPE	Memi[$1+36]	# type of PSF plot
define	DP_MANGV	Memr[P2R($1+37)]# vertical angle for surface plot
define	DP_MANGH	Memr[P2R($1+38)]# horizontal angle for surface plot
define  DP_MFLOOR	Memr[P2R($1+39)]# floor value for surface plot
define	DP_MCEILING	Memr[P2R($1+40)]# ceiling for surface plot
define	DP_CFLOOR	Memr[P2R($1+41)]# floor for contour
define  DP_CCEILING	Memr[P2R($1+42)]# ceiling for contour
