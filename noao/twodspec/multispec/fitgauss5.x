include <fset.h>
include "ms.h"

# FITGAUSS5 -- Procedures used in fitting the GAUSS5 model.
#
# G5_FIT1 -- Fitting algorithm #1.
# G5_FIT2 -- Fitting algorithm #2.
# G5_FIT -- Fit the selected parameters for the best RMS value.
# SET_VERBOSE -- Verbose output.

########################################################################
.helpsys g5fit1 Jul84 MULTISPEC
.ih
NAME
G5_FIT1 -- Fitting algorithm #1.
.ih
DESCRIPTION
This algorithm fits the selected parameters simultaneously.
The parameters are selected by the parameter array (which of the 5
model parameters in a profile are to be fit) and the spectra range
array defined in fitgauss5.com.  These two arrays are used to generate
the fitparms array with the routine set_fitparams.  The fitparams array
controls the parameters fit by G5_FIT.
.ih
PARAMETERS
The model parameter values which are part of the MULTISPEC data structure,
the data array, the model array, the model profiles, and the profile
ranges must be initialized.  Other parameters for this procedure are
input via the common in file fitgauss5.com.  These include the spectra
to be fit and the parameters array.
.ih
OUTPUT
The returned data are the final parameter values, the model and profiles
arrays and the Y/N function value indicating if the RMS fit has been improved.
.endhelp
########################################################################

int procedure g5_fit1 (ms, data, model, profiles, ranges, lower, len_profile)

pointer	ms				# MULTISPEC database data
real	data[ARB]			# Line of image pixels
real	model[ARB]			# Line of model pixels
real	profiles[ARB]			# Model profiles
real	ranges[ARB]			# Origins of model profiles
real	lower				# Profile origin
int	len_profile			# Length of a model profile

int	improved
real	rms
pointer	sp, fitparams

int	g5_fit()
real	armsrr()

include	"fitgauss5.com"

begin
	# Calculate the initial RMS.  The parameter values are only changed
	# if the new RMS is less than this value.
	rms = armsrr (data, model, MS_LEN(ms, 1))
	call g5_prnt3 (rms)

	# Allocate and set the fitparams array.
	call smark (sp)
	call salloc (fitparams, MS_NSPECTRA(ms) * MS_NGAUSS5, TY_REAL)
	call set_fitparams (spectra, parameters, MS_NSPECTRA(ms), MS_NGAUSS5,
	    Memr[fitparams])

	# Call the fitting program once to simultaneously minimize the RMS.
	improved = g5_fit (ms, data, model, profiles, ranges, Memr[fitparams],
	    lower, len_profile, rms)

	call sfree (sp)
	return (improved)
end

###############################################################################
.helpsys g5fit2 Jul84 MULTISPEC
.ih
NAME
G5_FIT2 -- Fitting algorithm #2.
.ih
DESCRIPTION
This algorithm begins by fitting the parameters I0, X0, and S0
simultaneously.  Note that the values of S1 and S2 are used but are
kept fixed.  Next the parameters S0 and S1 (the shape) are fit simultaneously
keeping I0, X0, and S2 fixed followed by fitting I0 and X0 while
keeping S0, S1, and S2 (the shape) fixed.  If either of these fits
fails to improve the RMS then the algorithm terminates.
Also, if after the two steps (the fit of S0 and S1 followed by the fit
of I0 and X0), the RMS of the fit has not improved by more than the
user specified factor the algorithm also terminates.
.ih
INPUT
The model parameter values which are part of the MULTISPEC data structure,
the data array, the model array, the model profiles, and the profile
ranges must be initialized.  Other parameters for this procedure are
input via the common in file fitgauss5.com.  These include the spectra
to be fit, the parameters array (used as a working array), and the RMS
stopping factor.
.ih
OUTPUT
The returned data are the final parameter values, the model and profiles
arrays and the Y/N function value indicating if the RMS fit has been improved.
.endhelp
##############################################################################

int procedure g5_fit2 (ms, data, model, profiles, ranges, lower, len_profile)

pointer	ms				# MULTISPEC database data
real	data[ARB]			# Line of image pixels
real	model[ARB]			# Line of model pixels
real	profiles[ARB]			# Model profiles
real	ranges[ARB]			# Origins of model profiles
real	lower				# Profile origin
int	len_profile			# Length of a model profile

int	improved, fit
real	rms, rms_old
pointer	sp, fitparams

int	g5_fit()
real	armsrr()

include	"fitgauss5.com"

begin
	# Calculate the initial RMS.  The parameter values are only changed
	# if the new RMS is less than this value.
	rms = armsrr (data, model, MS_LEN(ms, 1))
	call g5_prnt3 (rms)

	# Allocate the fitparams array.
	call smark (sp)
	call salloc (fitparams, MS_NSPECTRA(ms) * MS_NGAUSS5, TY_REAL)

	# Fit the parameters I0, X0, and S0.
	parameters[I0_INDEX] = YES
	parameters[X0_INDEX] = YES
	parameters[S0_INDEX] = YES
	parameters[S1_INDEX] = NO
	parameters[S2_INDEX] = NO
	call set_fitparams (spectra, parameters, MS_NSPECTRA(ms),
	    MS_NGAUSS5, Memr[fitparams])

	# Call the fitting procedure to minimze the RMS.
	improved = g5_fit (ms, data, model, profiles, ranges,
	    Memr[fitparams], lower, len_profile, rms)

	# Two step fitting algorithm consisting of a fit to S0 and S1 followed
	# by a fit to I0 and X0.  This loop terminates when either one
	# of the fits fails to improve the RMS or the RMS has improved
	# by less than factor after the second step (the I0, X0 fit).
	repeat {
	    rms_old = rms

	    # Fit S0 and S1.
	    parameters[I0_INDEX] = NO
	    parameters[X0_INDEX] = NO
	    parameters[S0_INDEX] = YES
	    parameters[S1_INDEX] = YES
	    call set_fitparams (spectra, parameters, MS_NSPECTRA(ms),
		MS_NGAUSS5, Memr[fitparams])
	    fit = g5_fit (ms, data, model, profiles, ranges,
		Memr[fitparams], lower, len_profile, rms)
	    if (fit == NO)
		break
	    improved = YES

	    # Fit I0 and X0.
	    parameters[I0_INDEX] = YES
	    parameters[X0_INDEX] = YES
	    parameters[S0_INDEX] = NO
	    parameters[S1_INDEX] = NO
	    call set_fitparams (spectra, parameters, MS_NSPECTRA(ms),
		MS_NGAUSS5, Memr[fitparams])
	    fit = g5_fit (ms, data, model, profiles, ranges,
		Memr[fitparams], lower, len_profile, rms)
	    if (fit == NO)
	        break

	    if (rms > (1 - factor) * rms_old)
		break
	}

	call sfree (sp)
	return (improved)
end


##############################################################################
.helpsys g5fit Jul84 MULTISPEC
.ih
NAME
G5_FIT -- Basic parameter fitting procedure.
.ih
INPUT
The input data are the data array to be fit and the initial model
parameters (part of the MULTISPEC data structure), the model array
and model profiles (with the profile ranges array) corresponding to the
initial model parameters, and the RMS of the model relative to the data.
The parameters to be fit are selected by the fitparams array.
Parameters controlling the fitting process are input to this procedure
via the common block in the include file fitgauss5.com.  These parameters are
the RMS stopping factor and parameters controlling the smoothing of the
shape parameters.
.ih
OUTPUT
The returned data are the final parameter values, the model and profiles
arrays and the Y/N function value indicating if the RMS fit has been improved.
.ih
DESCRIPTION
The best RMS fit is obtained by iteration.  Correction vectors for the
parameters being fit are obtained by the simultaneous banded matrix
method in the procedure solve.  Heuristic constraints and smoothing
are applied to the solution and then the RMS of the new fit to the
data is calculated.  New parameter corrections are computed until the RMS of
the fit fails to improve by the specified factor.
.endhelp
############################################################################

int procedure g5_fit (ms, data, model, profiles, ranges, fitparams, lower,
    len_profile, rms)

pointer	ms				# MULTISPEC data structure
int	fitparams[ARB]			# Model parameters to be fit
real	data[ARB]			# Data line to be fit
real	model[ARB]			# Model line
real	profiles[ARB]			# Model profiles
real	ranges[ARB]			# Profile ranges
real	lower				# Lower limit of profiles
int	len_profile			# Length of profiles
real	rms				# RMS of fit

int	improved
int	len_line, nspectra, nparams
real	rms_next, rnorm
pointer	sp, last_i0, last_x0, last_s0, last_s1, last_s2
pointer solution, sol_i0, sol_x0, sol_s0, sol_s1, sol_s2

real	armsrr()

include	"fitgauss5.com"

begin
	# Set array lengths.
	len_line = MS_LEN(ms, 1)
	nspectra = MS_NSPECTRA(ms)
	nparams = MS_NGAUSS5

	# Allocate working memory to temporarily save the previous parameter
	# values and to hold the correction vector.
	call smark (sp)
	call salloc (last_i0, nspectra, TY_REAL)
	call salloc (last_x0, nspectra, TY_REAL)
	call salloc (last_s0, nspectra, TY_REAL)
	call salloc (last_s1, nspectra, TY_REAL)
	call salloc (last_s2, nspectra, TY_REAL)
	call salloc (solution, nspectra * nparams, TY_REAL)

	# Offsets in the solution array for the various parameters.
	sol_i0 = solution + (I0_INDEX - 1) * nspectra
	sol_x0 = solution + (X0_INDEX - 1) * nspectra
	sol_s0 = solution + (S0_INDEX - 1) * nspectra
	sol_s1 = solution + (S1_INDEX - 1) * nspectra
	sol_s2 = solution + (S2_INDEX - 1) * nspectra

	improved = NO
	repeat {
	    # Store the last parameter values so that if the parameter values
	    # determined in the next iteration yield a poorer RMS fit to
	    # the data the best fit parameter values can be recovered.

 	    call amovr (PARAMETER(ms,I0,1), Memr[last_i0], nspectra)
 	    call amovr (PARAMETER(ms,X0,1), Memr[last_x0], nspectra)
 	    call amovr (PARAMETER(ms,S0,1), Memr[last_s0], nspectra)
 	    call amovr (PARAMETER(ms,S1,1), Memr[last_s1], nspectra)
 	    call amovr (PARAMETER(ms,S2,1), Memr[last_s2], nspectra)

	    # Determine a correction solution vector for the selected
	    # parameters simultaneously, apply heuristic constraints to the
	    # solution vector, apply the correction vector to the parameter
	    # values, and smooth the shape parameters if requested.

	    # Find a least squares correction vector.
	    call solve (ms, data, model, fitparams, profiles, ranges,
		len_line, len_profile, nspectra, nparams, Memr[solution], rnorm)

	    # Apply constraints to the correction vector.
	    call constrain_gauss5 (ms, Memr[solution], nspectra, nparams)

	    # Add the correction vector to the parameter vector.
	    call aaddr (PARAMETER(ms,I0,1), Memr[sol_i0], PARAMETER(ms,I0,1),
		nspectra)
	    call aaddr (PARAMETER(ms,X0,1), Memr[sol_x0], PARAMETER(ms,X0,1),
		nspectra)
	    call aaddr (PARAMETER(ms,S0,1), Memr[sol_s0], PARAMETER(ms,S0,1),
		nspectra)
	    call aaddr (PARAMETER(ms,S1,1), Memr[sol_s1], PARAMETER(ms,S1,1),
		nspectra)
	    call aaddr (PARAMETER(ms,S2,1), Memr[sol_s2], PARAMETER(ms,S2,1),
		nspectra)

	    # Smooth the shape parameters.
	    if (smooth[S0_INDEX] == YES)
		call ms_smooth (PARAMETER(ms, X0, 1), PARAMETER(ms, S0, 1))
	    if (smooth[S1_INDEX] == YES)
		call ms_smooth (PARAMETER(ms, X0, 1), PARAMETER(ms, S1, 1))
	    if (smooth[S2_INDEX] == YES)
		call ms_smooth (PARAMETER(ms, X0, 1), PARAMETER(ms, S2, 1))

	    # Calculate new model profiles and new model data line.
	    # Determine the RMS fit of the new model to the data.
	    # If the change in the RMS is less than factor times the
	    # previous RMS the interation is terminated else the improvement
	    # in the RMS is recorded and the next iteration is begun.

	    # Set new model profiles.
	    call mod_gauss5 (ms, lower, profiles, ranges, len_profile, nspectra)

	    # Set new model line from the profiles.
	    call set_model (ms, model, profiles, ranges, len_line,
		len_profile, nspectra)

	    # Calculate the RMS of the new model.
	    rms_next = armsrr (data, model, len_line)

	    # Check to see if the RMS is improved enough to continue iteration.
	    if ((rms - rms_next) < factor * rms) {

		# The RMS has not improved enough to continue iteration.

		if (rms_next < rms) {
		    # Keep the latest parameter values, profiles, and model
		    # because the new RMS is lower than the previous RMS.
		    # Record the improvement.
		    rms = rms_next
		    improved = YES
		    call g5_prnt3 (rms)

		} else {
		    # Restore the parameter values, profiles, and model to 
		    # previous values because the new RMS is higher.
 	    	    call amovr (Memr[last_i0], PARAMETER(ms,I0,1), nspectra)
 	    	    call amovr (Memr[last_x0], PARAMETER(ms,X0,1), nspectra)
 	    	    call amovr (Memr[last_s0], PARAMETER(ms,S0,1), nspectra)
 	    	    call amovr (Memr[last_s1], PARAMETER(ms,S1,1), nspectra)
 	    	    call amovr (Memr[last_s2], PARAMETER(ms,S2,1), nspectra)
	    	    call mod_gauss5 (ms, lower, profiles, ranges, len_profile,
			nspectra)
	    	    call set_model (ms, model, profiles, ranges, len_line,
			len_profile, nspectra)
		}

		# Exit the iteration loop.
		break

	    } else {

		# The RMS has improved significantly.  Record the improvement
		# and continue the iteration loop.

	        rms = rms_next
	        improved = YES
		call g5_prnt3 (rms)
	    }
	}

	call sfree (sp)
	return (improved)
end


# G5_SET_VERBOSE -- Output procedures for verbose mode.

procedure g5_set_verbose (verbose)

bool	verbose
bool	flag

# entry g5_prnt1 (image, naverage, track, start)
char	image[1]
int	naverage
bool	track
int	start

# entry g5_prnt2 (line, data, len_data)
int	line, len_data
real	data[1]
real	rms, data_rms

real	armsr()
include	"fitgauss5.com"

begin
	# Toggle verbose output.
	flag = verbose
	if (flag)
	    call fseti (STDOUT, F_FLUSHNL, YES)
	else
	    call fseti (STDOUT, F_FLUSHNL, NO)
	return

entry g5_prnt1 (image, naverage, track, start)

	# Print the values of the various task parameters.

	if (!flag)
	    return

	call printf ("\nMULTISPEC Model Fitting Program\n\n")
	call printf ("Image file being modeled is %s.\n")
	    call pargstr (image)
	call printf ("Average %d lines of the image.\n")
	    call pargi (naverage)
	call printf ("Fitting algorithm %d.\n")
	    call pargi (algorithm)
	if (algorithm == 1) {
	    if (parameters[I0_INDEX] == YES)
	        call printf ("Fit intensity scales.\n")
	    if (parameters[X0_INDEX] == YES)
	        call printf ("Fit spectra positions.\n")
	    if (parameters[S0_INDEX] == YES)
	        call printf ("Fit spectra widths.\n")
	    if (parameters[S1_INDEX] == YES)
	        call printf ("Fit model parameter s1.\n")
	    if (parameters[S2_INDEX] == YES)
	        call printf ("Fit model parameter s2.\n")
	}
	if (track) {
	    call printf ("Track model from line %d.\n")
		call pargi (start)
	}
	call printf (
	    "Iterate model until the fit RMS decreases by less than %g %%.\n\n")
	    call pargr (factor * 100)

	return

entry g5_prnt2 (line, data, len_data)

	# Print the image line being fit and the data RMS.
	if (flag) {
	    call printf ("Fit line %d:\n")
	        call pargi (line)
	    data_rms = armsr (data, len_data)
	    call printf ("  Data RMS = %g\n")
	        call pargr (data_rms)
	}
	return

entry g5_prnt3 (rms)

	# Print the RMS of the fit and the ratio to the data RMS.
	if (flag) {
	    call printf ("  Fit RMS = %g  Fit RMS / Data RMS = %g\n")
	        call pargr (rms)
	        call pargr (rms / data_rms)
	}
end
