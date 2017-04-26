# Actions of the LEXNUM finite state automaton.

define	Acc	ACCEPT			# special actions
define	Rvt	REVERT

define	o_o	LEX_OCTAL		# reductions
define	d_d	LEX_DECIMAL
define	x_x	LEX_HEX
define	r_r	LEX_REAL
define	n__	LEX_NONNUM		# (other actions are new states)

# cc:		+-   0-7  8-9  ACF  ED   :    .    X    B    other

data	action /UNM, ODH, DHR, n__, n__, QRN, QRF, n__, n__, n__,	# start
		Rvt, ODH, DHR, Rvt, Rvt, Rvt, QRF, Rvt, Rvt, Rvt,	# UNM
		d_d, Acc, DHR, HEX, QHX, QRN, RFR, x_x, OHN, d_d,	# ODH
		d_d, Acc, Acc, HEX, QHX, QRN, RFR, x_x, HEX, d_d,	# DHR
		Rvt, RFR, RFR, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt,	# QRF
		Rvt, Acc, Acc, Acc, Acc, Rvt, Rvt, x_x, Acc, Rvt,	# HEX
		QRX, HRX, HRX, HEX, HEX, Rvt, Rvt, x_x, HEX, Rvt,	# QHX
		Rvt, RNM, RNM, Rvt, Rvt, Acc, Rvt, Rvt, Rvt, Rvt,	# QRN
		o_o, HEX, HEX, HEX, HEX, o_o, o_o, x_x, HEX, o_o,	# OHN
		r_r, Acc, Acc, r_r, RRX, r_r, r_r, r_r, r_r, r_r,	# RFR
		QRX, REX, REX, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt,	# RRX
		Rvt, REX, REX, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt, Rvt,	# QRX
		r_r, Acc, Acc, HEX, HEX, r_r, r_r, x_x, r_r, r_r,	# HRX
		r_r, Acc, Acc, r_r, QRX, Acc, QRF, r_r, r_r, r_r,	# RNM
		r_r, Acc, Acc, r_r, r_r, r_r, r_r, r_r, r_r, r_r/	# REX
