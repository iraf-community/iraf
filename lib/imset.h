# IMSET.H -- Definitions for IMIO user settable options.

define	IM_ADVICE	1	# RANDOM or SEQUENTIAL
define	IM_NBUFS	2	# number of input buffers
define	IM_COMPRESS	3	# align lines on device blocks?
define	IM_NBNDRYPIX	4	# width of boundary region
define	IM_TYBNDRY	5	# type of boundary extension
define	IM_FLAGBADPIX	6	# set bad pix to INDEF
define	IM_PIXFD	7	# pixfile fd (special devices)
define	IM_WHEADER	8	# update image header at unmap time
define	IM_BNDRYPIXVAL	9	# for option IM_CONSTANT
define	IM_CANCEL	10	# free any pixel data buffers
define	IM_CLOSEFD	11	# set F_CLOSEFD on pixfile
define	IM_BUFSIZE	12	# recommended FIO buffer size, chars
define	IM_IMAGENAME	13	# name of open image section
define	IM_PLDES	14	# pixel mask descriptor
define	IM_RLIO		15	# enable range list i/o (image masks)

define	IM_PMDES	IM_PLDES

# Types of Boundary Extension

define	BT_CONSTANT	1	# return constant if out of bounds
define	BT_NEAREST	2	# return nearest boundary pixel
define	BT_REFLECT	3	# reflect back into image
define	BT_WRAP		4	# wrap around to other side
define	BT_PROJECT	5	# project about boundary
