#{ RECURSION.CL -- Test CL calling recursion.

procedure recursion (level)

int	level

begin
	if (level == 0)
	    i = 0
	else
	    i = level
	recur0 (i)
end
