# PSF Structure

define 	LEN_PSFSTRUCT (16)

# PSF parameters

define	DP_PSFPLOT	Memi[$1]	# pointer to PSF plot structure
define	DP_PSFMATRIX	Memi[$1+1]	# pointer to PSF matrix
define	DP_COLPOINT	Memi[$1+2]	# pointer to the output column pointers
define	DP_PLOOKUP	Memi[$1+3]	# pointer to PSF lookup table
define	DP_SZLOOKUP	Memi[$1+4]	# size of the PSF lookup table
define	DP_PSFNUMB	Memi[$1+5]	# number of stars in PSF
define	DP_CUR_PSF	Memi[$1+6]	# position of current PSF star in file
define	DP_CUR_PSFID	Memi[$1+7]	# id of current PSF star
define	DP_CUR_PSFX	Memr[$1+8]	# x position of current PSF star
define	DP_CUR_PSFY	Memr[$1+9]	# y position of current PSF star
define	DP_CUR_PSFSKY	Memr[$1+10]	# sky for current PSF star
define	DP_CUR_PSFMAG	Memr[$1+11]	# mangitude for current PSF star
define	DP_PSFMIN	Memr[$1+12]	# minimum data value in PSF subrast
define	DP_PSFMAX	Memr[$1+13]	# maximum data value in PSF subrast

# Plot parameters

define	PSF_PLOTS	"|mesh|contour|"

# PSF Plot structure

define 	LEN_PSFPLOT (18)

define	DP_PSFGD	Memi[$1]	# graphics descriptor
define	DP_PSFID	Memi[$1+1]	# image display descriptor
define	DP_PSFPFD	Memi[$1+2]	# plot file descriptor
define	DP_PSFMGD	Memi[$1+3]	# metacode descriptor
define	DP_PLOT_TYPE	Memi[$1+4]	# type of PSF plot
define	DP_MANGV	Memr[$1+5]	# vertical angle for surface plot
define	DP_MANGH	Memr[$1+6]	# horizontal angle for surface plot
define  DP_MFLOOR	Memr[$1+7]	# floor value for surface plot
define	DP_MCEILING	Memr[$1+8]	# ceiling for surface plot
define	DP_CZERO	Memr[$1+9]	# zero level for contour
define	DP_CFLOOR	Memr[$1+10]	# floor for contour
define  DP_CCEIL	Memr[$1+11]	# ceiling for contour

# miscellaneous definitions

define	LEN_USERAREA	28800
define	NAPCOLUMNS	6
define	SZ_PSFMATRIX	3
