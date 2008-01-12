# PS_WIDTH -- Get width of physical screen
#
# B.Simon	18-Jan-89	Original

int procedure ps_width ()

#--
include	"screen.com"

begin
	return (cols)
end
