# the photometry structure definition


# input photometry list parameters (# 101 - 200)

define	APNUM		101		

# parameters for reading photometry results

define	DP_PAPID 	1	# star id number
define	DP_PAPXCEN	2	# x center
define	DP_PAPYCEN	3	# y center
define	DP_PAPSKY	4	# sky value
define	DP_PAPMAG1	5	# aperture 1 magnitude
define	DP_PAPMERR1	6	# aperture 1 magnitude error
define	DP_PAPGROUP	7	# group number
define	DP_PAPNITER	8	# number of iterations
define	DP_PAPCHI	9	# chi squared of fit
define	DP_PAPSHARP	10	# sharpness characteristic

# some useful definitions

define	NAPRESULT	5
define	NAPGROUP	6
define	NAPPAR		10

# define the column names for reading and writing photometry files

define	ID		"ID"
define	GROUP		"GROUP"
define	XCENTER		"XCENTER"
define	YCENTER		"YCENTER"
define	APMAG		"MAG[1]"
define	MAG		"MAG"
define	APMAGERR	"MERR[1]"
define	MAGERR		"MERR"
define	SKY		"MSKY"
define	NITER		"NITER"
define	CHI		"CHI"
define	SHARP		"SHARPNESS"
define	PIER		"PIER"
define	PERROR		"PERROR"

define	LEN_DPAPSTRUCT	(15)

define	DP_APNUM	Memi[$1]	# number of stars in list
define	DP_APRESULT	Memi[$1+1]	# pointer to fields needed
define	DP_APID		Memi[$1+2]	# pointer to star ids
define	DP_APXCEN	Memi[$1+3]	# pointer to stellar x centers
define	DP_APYCEN	Memi[$1+4]	# pointer to stellar y centers
define	DP_APMAG	Memi[$1+5]	# pointer to magnitudes
define	DP_APERR	Memi[$1+6]	# pointer to magnitude errors
define	DP_APMSKY	Memi[$1+7]	# pointer to sky values
define	DP_APGROUP	Memi[$1+8]	# pointer to group numbers
define	DP_APNITER	Memi[$1+9]	# pointer to number of iterations
define	DP_APCHI	Memi[$1+10]	# pointer to output chi values
define	DP_APSHARP	Memi[$1+11]	# pointer to output sharp values
