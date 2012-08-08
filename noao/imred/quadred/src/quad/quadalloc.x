include <imhdr.h>
include "quadgeom.h"

# QUADOPEN -- Allocate space for quadgeom structure
# Note: The various arrays are dimensioned as QG_MAXAMPS+1 and are ZERO indexed.

procedure quadalloc (qg)

pointer	qg			#O Pointer to opened quadgeom structure

begin

	call malloc (qg, QG_LENSTRUCT, TY_STRUCT)

	# Zero readout counters
	QG_NAMPS(qg)  = 0
	QG_NAMPSX(qg) = 0
	QG_NAMPSY(qg) = 0

	# Allocate and zero arrays.
	call calloc (QG_AMPIDPTR(qg), QG_MAXAMPS+1, TY_INT)
	call calloc (QG_AMPTYPTR(qg), QG_MAXAMPS+1, TY_INT)

	call calloc (QG_NXPTR(qg),    QG_MAXAMPS+1, TY_INT)
	call calloc (QG_NYPTR(qg),    QG_MAXAMPS+1, TY_INT)

	call calloc (QG_DX1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_DX2PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_DY1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_DY2PTR(qg),   QG_MAXAMPS+1, TY_INT)

	call calloc (QG_TX1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_TX2PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_TY1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_TY2PTR(qg),   QG_MAXAMPS+1, TY_INT)

	call calloc (QG_BX1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_BX2PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_BY1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_BY2PTR(qg),   QG_MAXAMPS+1, TY_INT)
 
	call calloc (QG_CX1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_CX2PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_CY1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_CY2PTR(qg),   QG_MAXAMPS+1, TY_INT)

	call calloc (QG_AX1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_AX2PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_AY1PTR(qg),   QG_MAXAMPS+1, TY_INT)
	call calloc (QG_AY2PTR(qg),   QG_MAXAMPS+1, TY_INT)

	call calloc (QG_PHPTR(qg),    QG_MAXAMPS+1, TY_INT)

end

# QUADFREE -- Free quadgeom structure

procedure quadfree (qg)

pointer	qg			#O Pointer to open quadgeom structure

begin

	if (qg != NULL) {
	   
	    call mfree (QG_AMPIDPTR(qg), TY_INT)
	    call mfree (QG_AMPTYPTR(qg), TY_INT)

	    call mfree (QG_NXPTR(qg),    TY_INT)
	    call mfree (QG_NYPTR(qg),    TY_INT)

	    call mfree (QG_DX1PTR(qg),   TY_INT)
	    call mfree (QG_DX2PTR(qg),   TY_INT)
	    call mfree (QG_DY1PTR(qg),   TY_INT)
	    call mfree (QG_DY2PTR(qg),   TY_INT)

	    call mfree (QG_TX1PTR(qg),   TY_INT)
	    call mfree (QG_TX2PTR(qg),   TY_INT)
	    call mfree (QG_TY1PTR(qg),   TY_INT)
	    call mfree (QG_TY2PTR(qg),   TY_INT)

	    call mfree (QG_BX1PTR(qg),   TY_INT)
	    call mfree (QG_BX2PTR(qg),   TY_INT)
	    call mfree (QG_BY1PTR(qg),   TY_INT)
	    call mfree (QG_BY2PTR(qg),   TY_INT)

	    call mfree (QG_CX1PTR(qg),   TY_INT)
	    call mfree (QG_CX2PTR(qg),   TY_INT)
	    call mfree (QG_CY1PTR(qg),   TY_INT)
	    call mfree (QG_CY2PTR(qg),   TY_INT)

	    call mfree (QG_AX1PTR(qg),   TY_INT)
	    call mfree (QG_AX2PTR(qg),   TY_INT)
	    call mfree (QG_AY1PTR(qg),   TY_INT)
	    call mfree (QG_AY2PTR(qg),   TY_INT)

	    call mfree (QG_PHPTR(qg),    TY_INT)

	    call mfree (qg, TY_STRUCT)
	}
end

# QUADDUMP -- Print contents of quadgeom structure on STDERR
procedure quaddump (qg)

pointer	qg			#O Pointer to open quadgeom structure

int	amp

begin

	call eprintf ("Active amps: %d (%d in x, %d in y)\n")
	    call pargi (QG_NAMPS(qg))
	    call pargi (QG_NAMPSX(qg))
	    call pargi (QG_NAMPSY(qg))

	do amp = 0, QG_NAMPS(qg) {
	    switch (amp) {
	    case 0:
		call eprintf ("Entire image\n")
	    default:
		call eprintf ("Amp %s")
		    call pargstr (Memc[QG_AMPID(qg, amp)])

		if (QG_PHANTOM (qg, amp) == YES)
		    call eprintf (" [Phantom]")

		call eprintf ("\n")
	    }

	    call eprintf ("\tnx = %d \tny = %d \n")
		call pargi (QG_NX(qg, amp))
		call pargi (QG_NY(qg, amp))

	    call eprintf ("\tdx1 = %d \tdx2 = %d \tdy1 = %d \tdy2 = %d\n")
		call pargi (QG_DX1(qg, amp))
		call pargi (QG_DX2(qg, amp))
		call pargi (QG_DY1(qg, amp))
		call pargi (QG_DY2(qg, amp))

	    call eprintf ("\ttx1 = %d \ttx2 = %d \tty1 = %d \tty2 = %d\n")
		call pargi (QG_TX1(qg, amp))
		call pargi (QG_TX2(qg, amp))
		call pargi (QG_TY1(qg, amp))
		call pargi (QG_TY2(qg, amp))

	    call eprintf ("\tbx1 = %d \tbx2 = %d \tby1 = %d \tby2 = %d\n")
		call pargi (QG_BX1(qg, amp))
		call pargi (QG_BX2(qg, amp))
		call pargi (QG_BY1(qg, amp))
		call pargi (QG_BY2(qg, amp))

	    call eprintf ("\tcx1 = %d \tcx2 = %d \tcy1 = %d \tcy2 = %d\n")
		call pargi (QG_CX1(qg, amp))
		call pargi (QG_CX2(qg, amp))
		call pargi (QG_CY1(qg, amp))
		call pargi (QG_CY2(qg, amp))

	    call eprintf ("\tax1 = %d \tax2 = %d \tay1 = %d \tay2 = %d\n")
		call pargi (QG_AX1(qg, amp))
		call pargi (QG_AX2(qg, amp))
		call pargi (QG_AY1(qg, amp))
		call pargi (QG_AY2(qg, amp))
	}
end
