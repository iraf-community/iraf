# This is the VMS version.  A character string variable is passed
# to an integer array argument using %ref.
#
# FTGCBF -- Read a sequence of characters from a file into the output
# character string buffer.  The sequence may begin on any byte boundary and
# may be any number of bytes long.  An error status is returned if less than
# the requested amount of data is read.

procedure ftgcbf (iunit, convrt, nbytes, array, status)

int     iunit           #I fortran unit number
int     convrt          #I convert to ASCII? (not used in SPP version)
int     nbytes          #I number of bytes to be transferred
%	character*(*)	array
int     status          #U output error status

begin
	# Get the data.  Note that we use %ref.
        call ftgbyt (iunit, nbytes, %ref (array), status)
end
