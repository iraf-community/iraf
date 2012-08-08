# PS_SYNCH -- Bring screen up to date
#
# B.Simon	18-Jan-89	Original

procedure ps_synch ()

#--
include	"screen.com"

begin
	call flush (ttyout)
end
