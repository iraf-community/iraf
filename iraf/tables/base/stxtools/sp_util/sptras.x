# Set the dimensionality
define N_DIM 2

# sp_trans - Translate the origin to a new center.
#
# History
#   11Mar91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure sp_trans( x, y, npts, center, nx, ny )

real x[npts], y[npts]    # I:  The x, y vectors to translate.
int  npts                # I:  The number of points in the vectors.
real center[N_DIM]       # I:  The new coordinate center.
real nx[npts], ny[npts]  # O:  The translated vectors.

# Declarations
pointer mw              # MWCS structure.

# Function prototypes.
pointer mw_open(), mw_sctran()

begin

  mw = mw_open( NULL, N_DIM )
  call mw_shift( mw, center, 3b )
  call mw_v2tranr( mw_sctran( mw, "physical", "logical", 3b ),
                   x, y, nx, ny, npts )

  call mw_close( mw )

end
#---------------------------------------------------------------------------
# End of sp_trans
#---------------------------------------------------------------------------
