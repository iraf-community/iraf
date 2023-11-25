
# DAORAN -- Replacement for the random number generator RAN2 from
#           Numerical Recipes, uses the system urand() instead.

real procedure daoran (idum)
int	idum			# seed for the random number generator
real    urand()
begin
        return (urand (idum))
end
