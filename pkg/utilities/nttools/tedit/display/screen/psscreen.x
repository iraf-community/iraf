# PS_SCREEN -- Return a pointer to a given character on the screen
#
# B.Simon	28-Sep-90	Original

pointer procedure ps_screen (row, col)

int	row		# i: Screen line
int	col		# i: Screen column
#--
include	"screen.com"

begin
	return (termscr+cols*(row-1)+(col-1))
end
