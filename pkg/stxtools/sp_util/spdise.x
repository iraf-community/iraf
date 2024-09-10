# sp_distanced - Determine the distance between two points.
#
# History
#   4Dec90 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

double procedure sp_distanced( x1, y1, x2, y2 )

double x1, y1, x2, y2

double a, b

begin

  a = x1 - x2
  b = y1 - y2
  return( sqrt( ( a * a ) + ( b * b ) ) )

end
#---------------------------------------------------------------------------
# End of sp_distanced
#---------------------------------------------------------------------------
# sp_distancer - Determine the distance between two points.
#
# History
#   4Dec90 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

real procedure sp_distancer( x1, y1, x2, y2 )

real x1, y1, x2, y2

real a, b

begin

  a = x1 - x2
  b = y1 - y2
  return( sqrt( ( a * a ) + ( b * b ) ) )

end
#---------------------------------------------------------------------------
# End of sp_distancer
#---------------------------------------------------------------------------
