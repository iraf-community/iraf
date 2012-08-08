#---------------------------------------------------------------------------
.help cif.h Apr94 source
.ih
NAME
cif.h -- Definitions for the Coordinated Input File object.
.endhelp
#---------------------------------------------------------------------------
#====
# Below are the PUBLIC definitions of the CIF object.  These may be
# used by any external application as desired.
#====

# Generic size of file names/character strings used by CIF.
define	CIF_SZ_FNAME	SZ_PATHNAME

# Possible values for the operation code passed to the 'cif_next' routine.
define	CIF_NEXT_GROUP	1		# Get next, if any groups
define	CIF_NEXT_FILE	2		# Get next primary file

# Status of the secondary files after a 'cif_next" call.
define	CIF_OK		1		# New file which is accessable.
define	CIF_NONE	2		# No accessable file found.
define	CIF_SAME	3		# File name is the same as previous.
define	CIF_EXISTS	4		# Output file exists.

# CIF structure variables: Primary file
define	CIF_p_file_list		CIF_file_list(CIF_p($1))
define	CIF_p_file		CIF_file(CIF_p($1))
define	CIF_p_ext	       	CIF_ext(CIF_p($1))
define	CIF_p_status		CIF_status(CIF_p($1))
define	CIF_p_nloop		CIF_nloop(CIF_p($1))

# CIF structure variables: Input Files
define	CIF_in_file_list	CIF_file_list(CIF_in($1,$2))
define	CIF_in_file		CIF_file(CIF_in($1,$2))
define	CIF_in_ext	       	CIF_ext(CIF_in($1,$2))
define	CIF_in_status		CIF_status(CIF_in($1,$2))
define	CIF_in_nloop		CIF_nloop(CIF_in($1,$2))

# CIF structure variables: Output Files.
define	CIF_out_file_list	CIF_file_list(CIF_out($1,$2))
define	CIF_out_file		CIF_file(CIF_out($1,$2))
define	CIF_out_ext	       	CIF_ext(CIF_out($1,$2))
define	CIF_out_status		CIF_status(CIF_out($1,$2))

#===========================================================================
#===========================================================================
# The Private definitions to be used by the object code alone.  Any use
# of the below macros constitutes an interface violation.
#===========================================================================

# Type of file which the current file name represents.
define	CIF_GENERIC	1
define	CIF_IMAGE	2
define	CIF_DIRECTORY	3
define	CIF_SAME_ROOT	4

#====
# The CIF object structure.
#====
define	CIF_p			Memi[$1]
define	CIF_in_ptr		Memi[$1+1]
define	CIF_n_in		Memi[$1+2]
define	CIF_out_ptr		Memi[$1+3]
define	CIF_n_out		Memi[$1+4]
define	CIF_loop		Memi[$1+5]
define	CIF_SZ			6

define	CIF_in			Memi[CIF_in_ptr($1)+$2-1]
define	CIF_out			Memi[CIF_out_ptr($1)+$2-1]

#====
# CIF File Object Structure
#====
define	CIF_list		Memi[$1]
define	CIF_group		Memi[$1+1]
define	CIF_status		Memi[$1+2]
define	CIF_nloop		Memi[$1+3]
define	CIF_cg			Memi[$1+5]
define	CIF_type		Memi[$1+6]
define	CIF_cbuf		Memi[$1+7]
define	CIF_SZ_FILE		8

define	CIF_file_list		Memc[CIF_cbuf($1)]
define	CIF_file		Memc[CIF_cbuf($1)+CIF_SZ_FNAME+1]
define	CIF_ext			Memc[CIF_cbuf($1)+2*(CIF_SZ_FNAME+1)]
define	CIF_base		Memc[CIF_cbuf($1)+3*(CIF_SZ_FNAME+1)]
define	CIF_SZ_FILE_CBUF	4*(CIF_SZ_FNAME+1)

# Indexed versions of some strings.
define	CIF_basei		Memc[CIF_CBUF($1)+3*(CIF_SZ_FNAME+1)+$2-1]

#---------------------------------------------------------------------------
# End of cif.h
#---------------------------------------------------------------------------
