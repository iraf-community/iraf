include <math.h>

# The dimensionality.
define N_DIM 2

# Define some memory management.
define ONER Memr[$1+$2-1]

# sp_rotate - Rotate a vector.
#
# History
#    8Mar91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_rotate( x, y, npts, angle, nx, ny )

real x[npts], y[npts]    # I:  The vectors to rotate.
size_t npts                # I:  The number of points in the vectors.
real angle               # I:  The angle to rotate (radians).
real nx[npts], ny[npts]  # O:  The translated vectors.

# Declarations
size_t	sz_val
pointer center          # To specify the center.
pointer mw              # MWCS structure.
pointer sp              # Stack pointer.

# Function prototypes.
pointer mw_open(), mw_sctran()

include	<nullptr.inc>

begin

  # Suck some memory.
  call smark( sp )
  sz_val = N_DIM
  call salloc( center, sz_val, TY_REAL )

  mw = mw_open( NULLPTR, N_DIM )
  ONER(center,1) = 0.
  ONER(center,2) = 0.
  call mw_rotate( mw, -DEGTORAD( angle ), ONER(center,1), 3b )
  call mw_v2tranr( mw_sctran( mw, "physical", "logical", 3b ),
                  x, y, nx, ny, npts )

  call mw_close( mw )
  call sfree( sp )

end
#---------------------------------------------------------------------------
# End of sp_rotate
#---------------------------------------------------------------------------
