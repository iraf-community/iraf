#{ RECURS0.CL -- Test CL calling recursion.

procedure recurs0 (level)

int	level

begin
	j = level + 1
	if (level == 0)
	    recursion (j)
	else
	    sfpe ()
end
