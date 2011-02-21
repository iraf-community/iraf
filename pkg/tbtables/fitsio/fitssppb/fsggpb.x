include "fitsio.h"

procedure fsggpb(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # o array of values
int     status          # o error status

begin

call ftggpb(iunit,group,fparm,nparm,array,status)
end
