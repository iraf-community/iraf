include "fitsio.h"

procedure fspclm(ounit,colnum,frow,felem,nelem,array,status)

# write an array of double precision complex data values to the
# specified column of the table.
# The binary table column being written to must have datatype 'M'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
long	frow            # i first row
long	felem           # i first element in row
long	nelem           # i number of elements
double  array[ARB]      # i array of values
int     status          # o error status

int	i_frow
int	i_felem
int	i_nelem

begin
	i_frow = frow
	i_felem = felem
	i_nelem = nelem
	call ftpclm(ounit,colnum,i_frow,i_felem,i_nelem,array,status)
end
