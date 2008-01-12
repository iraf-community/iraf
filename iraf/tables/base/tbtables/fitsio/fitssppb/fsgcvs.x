include "fitsio.h"

procedure fsgcvs(iunit,colnum,frow,felem,nelem,nulval,array,dim1,anynul,
                status)

# read an array of string values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=' ', in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
char    nulval[SZ_FTNULL] # i value for undefined pixels
%       character fnulva*16
char    array[dim1,ARB]      # o array of values
%       character farray*256
int	dim1		# i size of 1st dimension of 2D character string array
bool    anynul          # o any null values returned?
int     status          # o error status
int	i
int	elem
bool    null

begin

call f77pak(nulval,fnulva,SZ_FTNULL)

anynul=false
elem=felem
do i=1,nelem    {
  call ftgcvs(iunit,colnum,frow,elem,1,fnulva,farray,null,status)
  if (null)
     anynul=true

  call f77upk(farray,array(1,i),dim1)
  elem=elem+1
 }
end
