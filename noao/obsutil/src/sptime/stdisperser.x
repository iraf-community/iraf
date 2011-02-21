include	"sptime.h"

# These routines interface between any type of disperser and the
# disperser type specific routines (such as those for gratings).


# ST_DISPERSER -- Initialize disperser data.

procedure st_disperser (st, name, index)

pointer st		#I SPECTIME pointer
char	name[ARB]	#I Table name
int	index		#I Grating index

int	order, oref, stgeti(), tabgeti(), strdic()
real	f, phi, g, blaze, wb, db, ref, stgetr(), tabgetr(), gr_getr()
pointer sp, str, fname, gr, gr_open()
bool	streq()
errchk	gr_open, tab_getr, gr_getr, st_gtable1

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_LINE, TY_CHAR)

	ST_GR(st,index) = NULL
	ST_DISPTYPE(st,index) = 0

	# Get disperser type.
	if (streq (name, "disperser")) {
	    call stgstr (st, "disptype", "disperser", "", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS) {
		iferr (call tabgstr (ST_TAB(st), "disperser", "spectrograph",
		    "type", Memc[str], SZ_LINE))
		    call strcpy ("generic", Memc[str], SZ_LINE)
	    }
	} else if (streq (name, "xdisperser")) {
	    call stgstr (st, "xdisptype", "xdisperser", "", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS) {
		iferr (call tabgstr (ST_TAB(st), "xdisperser", "spectrograph",
		    "type", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	    }
	} else
	    Memc[str] = EOS
	ST_DISPTYPE(st,index) = strdic (Memc[str],Memc[str],SZ_LINE,DISPTYPES)

	switch (ST_DISPTYPE(st,index)) {
	case GRATING:
	    f = ST_CAMFL(st) * 1000
	    switch (index) {
	    case 1:
		g = stgetr (st, "gmm", name, INDEFR)
		blaze = stgetr (st, "blaze", name, INDEFR)
		oref = stgeti (st, "oref", name, 1)
		wb = stgetr (st, "wavelength", name, INDEFR)
		db = stgetr (st, "dispersion", name, INDEFR)
		ref = stgetr (st, "eff", name, INDEFR)
	    case 2:
		g = stgetr (st, "xgmm", name, INDEFR)
		blaze = stgetr (st, "xblaze", name, INDEFR)
		oref = stgeti (st, "xoref", name, 1)
		wb = stgetr (st, "xwavelength", name, INDEFR)
		db = stgetr (st, "xdispersion", name, INDEFR)
		ref = stgetr (st, "xeff", name, INDEFR)

		# Check old names.
		if (IS_INDEFR(g)) {
		    iferr (g = tabgetr (ST_TAB(st), name, "spectrograph",
			"gmm"))
		    g = INDEFR
		}
	    }

	    # Old names.
	    if (IS_INDEFR(g)) {
		iferr (g = tabgetr (ST_TAB(st), name, "spectrograph",
		    "gmm"))
		    g = INDEFR
	    }
	    if (IS_INDEFR(blaze)) {
		iferr (blaze = tabgetr (ST_TAB(st), name, "spectrograph",
		    "blaze"))
		    blaze = INDEFR
	    }
	    if (IS_INDEFI(oref)) {
		iferr (oref = tabgeti (ST_TAB(st), name, "spectrograph",
		    "oref"))
		    oref = 1
	    }
	    if (IS_INDEFR(wb)) {
		iferr (wb = tabgetr (ST_TAB(st), name, "spectrograph",
		    "wavelength"))
		    wb = INDEFR
	    }
	    if (IS_INDEFR(db)) {
		iferr (db = tabgetr (ST_TAB(st), name, "spectrograph",
		    "dispersion"))
		    db = INDEFR
	    }
	    if (IS_INDEFR(ref)) {
		iferr (ref = tabgetr (ST_TAB(st), name, "spectrograph",
		    "reflectance"))
		    ref = 1.
	    }

	    phi = ST_INOUTA(st,index)
#	    if (!IS_INDEFR(db))
#		db = db / f

	    iferr (gr = gr_open (ST_CW(st), ST_ORDER(st,index), ref, wb, db,
		oref, f, g, blaze, 1., phi, INDEF, INDEF, 1, YES)) {
		g = 300.
		blaze = 6
	        gr = gr_open (ST_CW(st), ST_ORDER(st,index), ref, wb, db,
		    oref, f, g, blaze, 1., phi, INDEF, INDEF, 1, YES)
	    }
	    ST_GR(st,index) = gr

	    if (IS_INDEF(ST_CW(st)))
		ST_CW(st,index) = gr_getr (gr, "wavelength")
	    if (IS_INDEFI(ST_ORDER(st,index)))
		ST_ORDER(st,index) = nint (gr_getr (gr, "order"))
	    ST_DISP(st,index) = gr_getr (gr, "dblaze") * oref * f

	    # Look for explicit blaze functions.
	    do order = ST_ORDER(st,index)-1, ST_ORDER(st,index)+1 {
		call sprintf (Memc[str], SZ_LINE, "eff%d")
		    call pargi (order)
		ifnoerr (call tabgstr (ST_TAB(st), name, "spectrograph",
		    Memc[str], Memc[fname], SZ_LINE)) {
		    if (streq (name, "disperser"))
			call st_gtable1 (st, Memc[str], Memc[fname])
		    else if (streq (name, "xdisperser")) {
			call sprintf (Memc[str], SZ_LINE, "xeff%d")
			    call pargi (order)
			call st_gtable1 (st, Memc[str], Memc[fname])
		    }
		}
	    }
	case GRISM:
	    f = ST_CAMFL(st) * 1000
	    switch (index) {
	    case 1:
		g = stgetr (st, "gmm", name, INDEFR)
		blaze = stgetr (st, "blaze", name, INDEFR)
		ref = stgetr (st, "eff", name, 1.)
		db = stgetr (st, "indexref", name, INDEFR)
		if (!IS_INDEFI(ST_ORDER(st,index)) && ST_ORDER(st,index)!=1) {
		    call sprintf (Memc[str], SZ_LINE, "index%d")
			call pargi (ST_ORDER(st,index))
		    iferr (wb = tabgetr (ST_TAB(st), name, "spectrograph",
			Memc[str]))
			wb = db
		    db = wb
		}
	    case 2:
		g = stgetr (st, "xgmm", name, INDEFR)
		blaze = stgetr (st, "xblaze", name, INDEFR)
		ref = stgetr (st, "xeff", name, 1.)
		db = stgetr (st, "xindexref", name, INDEFR)
		if (!IS_INDEFI(ST_ORDER(st,index)) && ST_ORDER(st,index)!=1) {
		    call sprintf (Memc[str], SZ_LINE, "index%d")
			call pargi (ST_ORDER(st,index))
		    iferr (wb = tabgetr (ST_TAB(st), name, "spectrograph",
			Memc[str]))
			wb = db
		    db = wb
		}
	    }

	    # Old names.
	    if (IS_INDEFR(g)) {
		iferr (g = tabgetr (ST_TAB(st), name, "spectrograph",
		    "gmm"))
		    g = INDEFR
	    }
	    if (IS_INDEFR(blaze)) {
		iferr (blaze = tabgetr (ST_TAB(st), name, "spectrograph",
		    "prism"))
		    blaze = INDEFR
	    }
	    if (IS_INDEFR(ref)) {
		iferr (ref = tabgetr (ST_TAB(st), name, "spectrograph",
		    "transmission"))
		    ref = 1
	    }
	    if (IS_INDEFR(db)) {
		if (!IS_INDEFI(ST_ORDER(st,index))) {
		    call sprintf (Memc[str], SZ_LINE, "index%d")
			call pargi (ST_ORDER(st,index))
		    iferr (db = tabgetr (ST_TAB(st), name, "spectrograph",
			Memc[str]))
			db = tabgetr (ST_TAB(st), name, "spectrograph",
			    "index1")
		} else
		    db = tabgetr (ST_TAB(st), name, "spectrograph", "index1")
	    }
	    oref = 1

	    iferr (gr = gr_open (ST_CW(st), ST_ORDER(st,index), ref, INDEF,
		INDEF, oref, f, g, blaze, db, 0., blaze, blaze, 1, YES)) {
		g = 300.
		blaze = 6.
		gr = gr_open (ST_CW(st), ST_ORDER(st,index), ref, INDEF,
		    INDEF, oref, f, g, blaze, db, 0., blaze, blaze, 1, YES)
	    }

	    ST_GR(st,index) = gr

	    if (IS_INDEF(ST_CW(st)))
		ST_CW(st,index) = gr_getr (gr, "wavelength")
	    if (IS_INDEFI(ST_ORDER(st,index)))
		ST_ORDER(st,index) = nint (gr_getr (gr, "order"))
	    ST_DISP(st,index) = gr_getr (gr, "dblaze") * oref * f

	    # Look for explicit blaze functions.
	    do order = ST_ORDER(st,index)-1, ST_ORDER(st,index)+1 {
		call sprintf (Memc[str], SZ_LINE, "eff%d")
		    call pargi (order)
		ifnoerr (call tabgstr (ST_TAB(st), name, "spectrograph",
		    Memc[str], Memc[fname], SZ_LINE)) {
		    if (streq (name, "disperser"))
			call st_gtable1 (st, Memc[str], Memc[fname])
		    else if (streq (name, "xdisperser")) {
			call sprintf (Memc[str], SZ_LINE, "xeff%d")
			    call pargi (order)
			call st_gtable1 (st, Memc[str], Memc[fname])
		    }
		}
	    }
	case GENERIC:
	    f = ST_CAMFL(st) * 1000
	    g = INDEFR
	    blaze = INDEFR
	    oref = 1

	    switch (index) {
	    case 1:
		g = INDEFR
		blaze = INDEFR
		oref = 1
		wb = stgetr (st, "wavelength", name, INDEFR)
		db = stgetr (st, "dispersion", name, INDEFR)
		ref = stgetr (st, "eff", name, INDEFR)
	    case 2:
		g = INDEFR
		blaze = INDEFR
		oref = 1
		wb = stgetr (st, "xwavelength", name, INDEFR)
		db = stgetr (st, "xdispersion", name, INDEFR)
		ref = stgetr (st, "xeff", name, INDEFR)
	    }

	    if (IS_INDEFR(wb)) {
		iferr (wb = tabgetr (ST_TAB(st), name, "spectrograph",
		    "wavelength"))
		    wb = INDEFR
	    }
	    if (IS_INDEFR(db)) {
		iferr (db = tabgetr (ST_TAB(st), name, "spectrograph",
		    "dispersion"))
		    db = INDEFR
	    }
	    if (IS_INDEFR(ref)) {
		iferr (ref = tabgetr (ST_TAB(st), name, "spectrograph",
		    "reflectance"))
		    ref = 1.
	    }

	    phi = ST_INOUTA(st,index)

	    gr = gr_open (ST_CW(st), ST_ORDER(st,index), ref, wb, db,
		oref, f, g, blaze, 1., phi, INDEF, INDEF, 1, NO)
	    ST_GR(st,index) = gr

	    if (IS_INDEF(ST_CW(st)))
		ST_CW(st,index) = gr_getr (gr, "wavelength")
	    if (IS_INDEFI(ST_ORDER(st,index)))
		ST_ORDER(st,index) = nint (gr_getr (gr, "order"))
	    ST_DISP(st,index) = gr_getr (gr, "dblaze") * oref * f

	    # Look for explicit blaze functions.
	    do order = ST_ORDER(st,index)-1, ST_ORDER(st,index)+1 {
		call sprintf (Memc[str], SZ_LINE, "eff%d")
		    call pargi (order)
		ifnoerr (call tabgstr (ST_TAB(st), name, "spectrograph",
		    Memc[str], Memc[fname], SZ_LINE)) {
		    if (streq (name, "disperser"))
			call st_gtable1 (st, Memc[str], Memc[fname])
		    else if (streq (name, "xdisperser")) {
			call sprintf (Memc[str], SZ_LINE, "xeff%d")
			    call pargi (order)
			call st_gtable1 (st, Memc[str], Memc[fname])
		    }
		}
	    }
	}

	call sfree (sp)
end


# ST_DISPEFF -- Return disperser efficiency.

real procedure st_dispeff (st, name, wave, order)

pointer st		#I SPECTIME pointer
char	name[ARB]	#I Table name
real	wave		#I Wavelength
int	order		#I Order
real	eff		#O Efficiency

real	w
pointer	sp, tab, str

int	tabgeti()
real	tabinterp1(), tabgetr(), gr_eff()
bool	tabexists(), streq()
errchk	tabinterp1, gr_eff

begin
	tab = ST_TAB(st)
	eff = INDEF

	if (streq (name, "disperser") && ST_DISPTYPE(st,1) == 0)
	    return (eff)
	if (streq (name, "xdisperser") && ST_DISPTYPE(st,2) == 0)
	    return (eff)

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	if (streq (name, "disperser")) {
	    call sprintf (Memc[str], SZ_FNAME, "eff%d")
		call pargi (order)
	    if (!tabexists (tab, Memc[str])) {
		if (order == 1 && tabexists (tab, name)) {
		    if (tabgeti (tab, name, "", "table.ndim") != 0)
			call strcpy (name, Memc[str], SZ_FNAME)
		}
	    }
	    if (tabexists (tab, Memc[str])) {
		eff = tabinterp1 (tab, Memc[str], wave)
		w = max (tabgetr (tab, Memc[str], "", "table.xmin"), wave)
		w = min (tabgetr (tab, Memc[str], "", "table.xmax"), w)
		if (w != wave) {
		    eff = tabinterp1 (tab, Memc[str], w) /
		        gr_eff (ST_GR(st,1), w, order)
		    eff = eff * gr_eff (ST_GR(st,1), wave, order)
		}
	    } else
		eff = gr_eff (ST_GR(st,1), wave, order)
	} else if (streq (name, "xdisperser")) {
	    call sprintf (Memc[str], SZ_FNAME, "xeff%d")
		call pargi (order)
	    if (!tabexists (tab, Memc[str])) {
		if (order == 1 && tabexists (tab, name)) {
		    if (tabgeti (tab, name, "", "table.ndim") != 0)
			call strcpy (name, Memc[str], SZ_FNAME)
		}
	    }
	    if (tabexists (tab, Memc[str])) {
		eff = tabinterp1 (tab, Memc[str], wave)
		w = max (tabgetr (tab, Memc[str], "", "table.xmin"), wave)
		w = min (tabgetr (tab, Memc[str], "", "table.xmax"), w)
		if (w != wave) {
		    eff = tabinterp1 (tab, Memc[str], w) /
		        gr_eff (ST_GR(st,2), w, order)
		    eff = eff * gr_eff (ST_GR(st,2), wave, order)
		}
	    } else
		eff = gr_eff (ST_GR(st,2), wave, order)
	}

	if (IS_INDEF(eff))
	    eff = 0.

	call sfree (sp)
	return (eff)
end


# ST_X2W -- Return wavelength at given position on detector.

real procedure st_x2w (st, index, x)

pointer st		#I SPECTIME pointer
int	index		#I Grating index
real	x		#I Detector position (mm from center)
real	w		#O Wavelength (Angstroms)

real	gr_x2w()

begin
	switch (ST_DISPTYPE(st,index)) {
	case GRATING:
	    w = gr_x2w (ST_GR(st,index), x, ST_ORDER(st,index)) 
	case GRISM:
	    w = gr_x2w (ST_GR(st,index), x, ST_ORDER(st,index)) 
	case GENERIC:
	    w = gr_x2w (ST_GR(st,index), x, ST_ORDER(st,index)) 
	}

	return (w)
end


# ST_W2X -- Return wavelength at given position on detector.

real procedure st_w2x (st, index, w)

pointer st		#I SPECTIME pointer
int	index		#I Grating index
real	w		#I Wavelength (Angstroms)
real	x		#O Detector position (mm from center)

real	gr_w2x()

begin
	switch (ST_DISPTYPE(st,index)) {
	case GRATING:
	    x = gr_w2x (ST_GR(st,index), w, ST_ORDER(st,index)) 
	case GRISM:
	    x = gr_w2x (ST_GR(st,index), w, ST_ORDER(st,index)) 
	case GENERIC:
	    x = gr_w2x (ST_GR(st,index), w, ST_ORDER(st,index)) 
	}

	return (x)
end


# ST_W2DW -- Return dispersion on detector at given wavelength.

real procedure st_w2dw (st, index, w)

pointer st		#I SPECTIME pointer
int	index		#I Grating index
real	w		#I Wavelength (Angstroms)
real	d		#I Dispersion (Angstroms/mm)

real	gr_w2dw()

begin
	switch (ST_DISPTYPE(st,index)) {
	case GRATING:
	    d = gr_w2dw (ST_GR(st,index), w, ST_ORDER(st,index)) 
	case GRISM:
	    d = gr_w2dw (ST_GR(st,index), w, ST_ORDER(st,index)) 
	case GENERIC:
	    d = gr_w2dw (ST_GR(st,index), w, ST_ORDER(st,index)) 
	}

	return (d)
end
