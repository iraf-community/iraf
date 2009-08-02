#---------------------------------------------------------------------------
.help od.h Feb93 source
.ih
NAME
od.h -- Include parameters for the 1D I/O data system.
.endhelp
#---------------------------------------------------------------------------
#-----
# Below describes the structure and access to the OD descriptor.
define  OD_FD           Memp[$1]             # The image/table descriptor
define  OD_TYPE         Memi[P2I($1+1)]      # TABLE/IMAGE flag.
define  OD_CD_PTR       Memp[$1+2]           # Table column descriptor.
define  OD_CD           Memp[OD_CD_PTR($1)+$2-1]
define  OD_LEN          Memz[P2Z($1+3)]      # Dimension of the data.
define  OD_NGRP         Memi[P2I($1+4)]      # Number of groups in image.
define  OD_GRP          Memi[P2I($1+5)]      # Current open group.
define  OD_NAME_PTR     Memp[$1+6]           # Specified file name.
define  OD_NAME         Memc[OD_NAME_PTR($1)]
define  OD_MW           Memp[$1+7]           # MWCS descriptor.
define  OD_WL           Memp[$1+8]           # World-to-Logical transformation.
define  OD_LW           Memp[$1+9]           # Logical-to-World transformation.
define  OD_WSYS_PTR     Memp[$1+10]          # WCS system type.
define  OD_WSYS         Memc[OD_WSYS_PTR($1)]
define  OD_OLD          Memp[$1+11]          # Template which opened this OD.
define  OD_SZ_OD        12              # Size of structure.

# The flag of what type of file we are dealing with.
define  OD_TABLE         1
define  OD_IMAGE         2
#---------------------------------------------------------------------------
# End of od.h
#---------------------------------------------------------------------------
