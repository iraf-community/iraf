# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# RESET_SCAN -- Initialize the scan common at the start of a scan.  May also
# be called by the user to rescan a line, following a conversion failure.

procedure reset_scan()

include	"scan.com"

begin
	sc_ip = 1
	sc_ntokens = 0
	sc_stopscan = false
end
