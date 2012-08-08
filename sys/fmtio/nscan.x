# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NSCAN -- Return the number of tokens successfully converted in the most
# recent scan.

int procedure nscan()

include	"scan.com"

begin
	return (sc_ntokens)
end
