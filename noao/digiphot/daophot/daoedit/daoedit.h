# The definitions file for the DAOEDIT task
# At some point this should become paert of daophotdef.h

define	PSET_LIST	"|datapars|centerpars|fitskypars|photpars|daopars|\
findpars|"

define	PSET_DATAPARS	1
define	PSET_CENTERPARS	2
define	PSET_FITSKYPARS	3
define	PSET_PHOTPARS	4
define	PSET_DAOPARS	5
define	PSET_FINDPARS	6

define	CMD_LIST  "|lpar|epar|unlearn|scale|fwhmpsf|emission|sigma|datamin|\
datamax|noise|ccdread|gain|readnoise|epadu|exposure|airmass|filter|obstime|\
itime|xairmass|ifilter|otime|calgorithm|cbox|cthreshold|maxshift|minsnratio|\
cmaxiter|clean|rclean|rclip|kclean|mkcenter|salgorithm|annulus|\
dannulus|skyvalue|smaxiter|sloclip|shiclip|snreject|sloreject|shireject|\
khist|binsize|smooth|rgrow|mksky|weighting|apertures|zmag|mkapert|\
function|varorder|nclean|saturated|matchrad|psfrad|fitrad|recenter|fitsky|\
groupsky|sannulus|wsannulus|flaterr|proferr|maxiter|clipexp|cliprange|\
critoverlap|maxnstar|maxgroup|threshold|nsigma|ratio|theta|sharplo|sharphi|\
roundlo|roundhi|mkdetections|"

define	CMD_LPAR	1
define	CMD_EPAR	2
define	CMD_UNLEARN	3

define	CMD_SCALE	4
define	CMD_FWHMPSF	5
define	CMD_EMISSION	6
define	CMD_SIGMA	7
define	CMD_DATAMIN	8
define	CMD_DATAMAX	9
define	CMD_NOISE	10
define	CMD_CCDREAD	11
define	CMD_GAIN	12
define	CMD_READNOISE	13
define	CMD_EPADU	14
define	CMD_EXPOSURE	15
define	CMD_AIRMASS	16
define	CMD_FILTER	17
define	CMD_OBSTIME	18
define	CMD_ITIME	19
define	CMD_XAIRMASS	20
define	CMD_IFILTER	21
define	CMD_OTIME	22

define	CMD_CALGORITHM	23
define	CMD_CBOX	24
define	CMD_CTHRESHOLD	25
define	CMD_MAXSHIFT	26
define	CMD_MINSNRATIO	27
define	CMD_CMAXITER	28
define	CMD_CLEAN	29
define	CMD_RCLEAN	30
define	CMD_RCLIP	31
define	CMD_KCLEAN	32
define	CMD_MKCENTER	33

define	CMD_SALGORITHM	34
define	CMD_ANNULUS	35
define	CMD_DANNULUS	36
define	CMD_SKYVALUE	37
define	CMD_SMAXITER	38
define	CMD_SLOCLIP	39
define	CMD_SHICLIP	40
define	CMD_SNREJECT	41
define	CMD_SLOREJECT	42
define	CMD_SHIREJECT	43
define	CMD_KHIST 	44
define	CMD_BINSIZE	45
define	CMD_SMOOTH	46
define	CMD_RGROW	47
define	CMD_MKSKY	48

define	CMD_WEIGHTING	49
define	CMD_APERTURES	50
define	CMD_ZMAG	51
define	CMD_MKAPERT	52

define	CMD_FUNCTION	53
define	CMD_VARORDER	54
define	CMD_NCLEAN	55
define	CMD_SATURATED	56
define	CMD_MATCHRAD	57
define	CMD_PSFRAD	58
define	CMD_FITRAD	59
define	CMD_RECENTER	60
define	CMD_FITSKY	61
define	CMD_GROUPSKY	62
define	CMD_SANNULUS	63
define	CMD_WSANNULUS	64
define	CMD_FLATERR	65
define	CMD_PROFERR	66
define	CMD_MAXITER	67
define	CMD_CLIPEXP	68
define	CMD_CLIPRANGE	69
define	CMD_CRITOVERLAP	70
define	CMD_MAXNSTAR	71
define	CMD_MAXGROUP	72

define	CMD_THRESHOLD	73
define	CMD_NSIGMA	74
define	CMD_RATIO	75
define	CMD_THETA	76
define	CMD_SHARPLO	77
define	CMD_SHARPHI	78
define	CMD_ROUNDLO	79
define	CMD_ROUNDHI	80
define	CMD_MKDETECTIONS 81

# define the different WCSS for the radial profile plots

define	WCS_XPIX		1
define	WCS_XSCALE		2

define	WCS_YCOUNT		1
define	WCS_YNORM		2

# miscellaneous definitions

define	MAX_NAPERTS		100
