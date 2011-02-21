include "fitsio.h"

procedure fspgpj(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpj(ounit,group,fparm,nparm,array,status)
end
