# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Test procedures for the NMEMIO interface.
#

include <mach.h>


define	MT_HEAP		0			# test heap memory
define	MT_STACK	1			# test stack memory


task memtest 	= t_memtest,
     stack   	= t_stack, 
     realloc	= t_realloc


#  MEMTEST -- Task to test new memio interface.

procedure t_memtest ()

int	model, nerr
pointer str, ptr

bool	clgetb()

begin
	if (clgetb ("stack"))
	    model = MT_STACK
	else
	    model = MT_HEAP

	# Check we can allocate a large array.
	if (model == MT_HEAP) {
	    call eprintf ("\nBegin large heap malloc tests ....\n\n")
	    call malloc (str, 256000, TY_STRUCT)
	    call mfree (str, TY_STRUCT)
	    call eprintf ("Done\n\n")
	
	    # Print the memory layout.

	        call mt_print (TY_CHAR)

	        call mt_print (TY_SHORT)

	        call mt_print (TY_INT)

	        call mt_print (TY_BOOL)

	        call mt_print (TY_LONG)

	        call mt_print (TY_REAL)

	        call mt_print (TY_DOUBLE)

	        call mt_print (TY_COMPLEX)

	    call mt_print (TY_STRUCT)
	    call mt_print (TY_POINTER)
	}

	# Test Mem common assignments
	call eprintf ("\nBegin assignment tests ....\n\n");
	call mt_auto_b ("bool   ", model)
	call mt_auto_c ("char   ", model)
	call mt_auto_s ("short  ", model)
	call mt_auto_i ("int    ", model)
	call mt_auto_l ("long   ", model)
	call mt_auto_r ("real   ", model)
	call mt_auto_d ("double ", model)
	call mt_auto_x ("complex", model)
	call eprintf ("\nEnd assignment tests ....\n\n");


	# Test string memory
	call eprintf ("Begin Memc test\t\t");
	call calloc (str, SZ_LINE, TY_CHAR)
	call aclrc (Memc[str], SZ_LINE)
	call strcpy ("test string", Memc[str], SZ_LINE)
	call eprintf ("str = '%s'  ch[2] = '%c' (should be 's')\n")
	    call pargstr (Memc[str])
	    call pargc (Memc[str+2])
	call mfree (str, TY_CHAR)


	# Test the struct memory
	call eprintf ("\n\n")
	call eprintf ("Begin struct test\n")
	call mt_struct (model)
	call eprintf ("Done\n")


	# Test memory overflow and then underflow detection.
	call eprintf ("\n\n")
	call eprintf ("Testing overflow:\t")
	nerr = 0

	    iferr ( call mt_overflow (TY_CHAR) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_SHORT) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_INT) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_BOOL) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_LONG) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_REAL) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_DOUBLE) ) 
		nerr = nerr + 1;

	    iferr ( call mt_overflow (TY_COMPLEX) ) 
		nerr = nerr + 1;

	iferr ( call mt_overflow (TY_STRUCT) ) 
	    nerr = nerr + 1;
	iferr ( call mt_overflow (TY_POINTER) ) 
	    nerr = nerr + 1;
	call eprintf ("No. errors detected = %d  of 10\t\tDone\n")
	    call pargi (nerr)
	

	call eprintf ("Testing underflow:\t")
	nerr = 0

	    iferr ( call mt_underflow (TY_CHAR) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_SHORT) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_INT) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_BOOL) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_LONG) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_REAL) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_DOUBLE) ) 
		nerr = nerr + 1;

	    iferr ( call mt_underflow (TY_COMPLEX) ) 
		nerr = nerr + 1;

	iferr ( call mt_underflow (TY_STRUCT) ) 
	    nerr = nerr + 1;
	iferr ( call mt_underflow (TY_POINTER) ) 
	    nerr = nerr + 1;
	call eprintf ("No. errors detected = %d  of 10\t\tDone\n")
	    call pargi (nerr)


	# Note this test will leak 1024 bytes because of the error recovery.
	call eprintf ("Testing invalid free:\t")
	call calloc (ptr, 256, TY_REAL)
	iferr ( call mfree (ptr, TY_INT) )
	    call eprintf ("Detected\t\t\t\t")
	else
	    call eprintf ("Undetected\t\t\t\t")
	call eprintf ("Done\n")

	call eprintf ("Testing double free:\t")
	call calloc (ptr, 256, TY_INT)
	call mfree (ptr, TY_INT)
	iferr ( call mfree (ptr, TY_INT) )
	    call eprintf ("Detected\t\t\t\t")
	else
	    call eprintf ("Undetected\t\t\t\t")
	call eprintf ("Done\n")

	call eprintf ("Testing NULL free:\t")
	iferr ( call mfree (NULL, TY_INT) )
	    call eprintf ("Detected\t\t\t\t")
	else
	    call eprintf ("Undetected\t\t\t\t")
	call eprintf ("Done\n")

	call eprintf ("Testing recovered free:\n")
	call calloc (str, SZ_LINE, TY_CHAR)
	call eprintf ("Done\n")

	call eprintf ("\n\nEnd of NMEMIO tests\n")
end



# Test the SALLOC routine, which allocates storage on the stack.

procedure t_stack ()

int	bufsize
pointer	sp, junk
int	clglpi()

begin
	call smark (sp)

	while (clglpi ("buffer_size", bufsize) != EOF) {
	    call salloc (junk, bufsize, TY_CHAR)
	    call printf ("buffer pointer=%d, size=%d\n")
		call pargi (junk)
		call pargi (bufsize)
	    call flush (STDOUT)
	}

	call sfree (sp)
end


# Test the REALLOC procedure, used to change the size of a buffer.
# Work with two buffers, so that memory can be fragmented, forcing buffers
# to move.

procedure t_realloc()

pointer	a, b
int	i, sza, new_sza, szb, new_szb

begin
	sza = SZ_FNAME
	szb = SZ_LINE

	call malloc (a, sza, TY_CHAR)
	call malloc (b, szb, TY_CHAR)
	call strcpy ("abcdefghijk", Memc[a], ARB)
	call strcpy ("0123456789", Memc[b], ARB)

	call eprintf ("a is at %d, size %d: %s\n")
	    call pargi (a)
	    call pargi (sza)
	    call pargstr (Memc[a])
	call eprintf ("b is at %d, size %d: %s\n")
	    call pargi (b)
	    call pargi (szb)
	    call pargstr (Memc[b])
	call eprintf ("-------------------------------\n")

	for (i=1; i <= 10; i=i+1) {
	    if (i < 5) {
	        new_sza = sza + 512 ; new_szb = szb + 256
	    } else {
	        new_sza = sza + 256 ; new_szb = szb + 512
	    }
	    call realloc (a, new_sza, TY_CHAR)
	    call realloc (b, new_szb, TY_CHAR)

	    call eprintf ("%2d: a buf %d, size %d --> %d: %s\n")
		call pargi (i)
		call pargi (a)
		call pargi (sza)
		call pargi (new_sza)
		call pargstr (Memc[a])
	    call eprintf ("%2d: b buf %d, size %d --> %d: %s\n")
		call pargi (i)
		call pargi (b)
		call pargi (szb)
		call pargi (new_szb)
		call pargstr (Memc[b])

	    sza = new_sza
	    szb = new_szb
	}

	call mfree (a, TY_CHAR)
	call mfree (b, TY_CHAR)
end



define	SZ_TEST		640
define  F_I1		Memi[$1]
define  F_I2		Memi[$1+1]
define  F_L1		Meml[$1+2]
define  F_L2		Meml[$1+3]
define  F_R1		Memr[$1+4]
define  F_R2		Memr[$1+5]
define  F_D1		Memd[P2D($1+8)]
define  F_D2		Memd[P2D($1+10)]
define  F_I3		Memi[$1+12]
define  F_I4		Memi[$1+13]
define  F_S1		Mems[P2S($1+14)]
define  F_S2		Mems[P2S($1+15)]


procedure mt_struct (model)

int	model

pointer	sp, str
real	x, y, z
double  d1, d2, d3

int	locva()

begin
	if (model == MT_HEAP) {
	    call malloc (str, SZ_TEST, TY_STRUCT)
	} else {
	    call smark (sp)
	    call salloc (str, SZ_TEST, TY_STRUCT)
	}


	F_I1(str) = 1
	F_I2(str) = 2
	F_L1(str) = 3
	F_L2(str) = 4
	F_R1(str) = 5.0
	F_R2(str) = 6.0
	F_D1(str) = 7.0
	F_D2(str) = 8.0
	F_I3(str) = 9
	F_I4(str) = 10
	F_S1(str) = 11
	F_S2(str) = 12

	x 	   = 2.717	;   d1 	   = F_R1(str)
	y 	   = 2.717	;   d2 	   = 3.14159d0	;
	z 	   = double(x)  ;   d3 	   = double(3.14159)

	call eprintf ("\nd1=%.6g d2=%.6g d3=%.6g    x=%.6g y=%.6g z=%.6g)\n\n")
	    call pargd (d1)  ; call pargd (d2) ; call pargd (d3)
	    call pargr (x)   ; call pargr (y)  ; call pargr (z)

	call eprintf ("Done Setting values ....\n\ntest = %d  %d  %d\n\n")
	    call pargi (str) 
	    call pargi (locva(str))
	    call pargi (locva(F_I1(str)))

	# call mdump (str, 64)

	call eprintf ("I1 = %4d  I2 = %4d   \t%d %d\n")
	    call pargi (F_I1(str))        ; call pargi (F_I2(str))
	    call pargi (locva(F_I1(str))) ; call pargi (locva(F_I2(str)))

	call eprintf ("L1 = %4d  L2 = %4d   \t%d %d\n")
	    call pargl (F_L1(str))        ; call pargl (F_L2(str))
	    call pargi (locva(F_L1(str))) ; call pargi (locva(F_L2(str)))

	call eprintf ("R1 = %4.1f  R2 = %4.1f   \t%d %d\n")
	    call pargr (F_R1(str))	  ; call pargr (F_R2(str))
	    call pargi (locva(F_R1(str))) ; call pargi (locva(F_R2(str)))

	call eprintf ("D1 = %4.1f  D2 = %4.1f   \t%d %d\n")
	    call pargd (F_D1(str))	  ; call pargd (F_D2(str))
	    call pargi (locva(F_D1(str))) ; call pargi (locva(F_D2(str)))

	call eprintf ("I3 = %4d  I4 = %4d   \t%d %d\n")
	    call pargi (F_I3(str))        ; call pargi (F_I4(str))
	    call pargi (locva(F_I3(str))) ; call pargi (locva(F_I4(str)))

	call eprintf ("S1 = %4d  S2 = %4d   \t%d %d\n")
	    call pargs (F_S1(str))        ; call pargs (F_S2(str))
	    call pargi (locva(F_S1(str))) ; call pargi (locva(F_S2(str)))


        if (model == MT_HEAP)
    	    call mfree (str, TY_STRUCT)
        else
    	    call sfree (sp)
end


define	NVALS		3

procedure mt_print (dtype)

int	dtype

int	i, locva(), coerce()
real	x
double	xx
pointer	p, bp, lwl

begin
    call calloc (p, NVALS, dtype)
    bp = coerce (p, dtype, TY_INT)

    # Set the values.
    for (i=0; i < NVALS; i=i+1) {
	x = i 	; xx = i
	switch (dtype) {
	case TY_BOOL:	  Memb[p+i] = TRUE
	case TY_CHAR:	  Memc[p+i] = 'a' + i
	case TY_SHORT:	  Mems[p+i] = i
	case TY_INT:	  Memi[p+i] = i
	case TY_LONG:	  Meml[p+i] = i
	case TY_REAL:	  Memr[p+i] = x
	case TY_DOUBLE:	  Memd[p+i] = xx
	case TY_COMPLEX:  Memx[p+i] = cmplx(x,-x)

	case TY_STRUCT:   Memi[p+i] = i
	case TY_POINTER:  Memi[p+i] = i
	}
    }

    # Print the ptr header.
    call eprintf ("\n")
    call eprintf ("         p = 0x%-15x  %-16d\t%d\n")
	call pargi (p)  ;  call pargi (p)  ;   call pargi (locva(Memi[bp]))
    call eprintf ("       fwa = 0x%-15x  %-16d\t%d\n")
	call pargi (bp-5)	; call pargi (Memi[bp-5])
	call pargi (locva (Memi[bp-5]))
    call eprintf ("       lwl = 0x%-15x  %-16d\t%d\n")
	call pargi (bp-4)	; call pargi (Memi[bp-4])
	call pargi (locva (Memi[bp-4]))
    call eprintf ("     dtype = 0x%-15x  %-16d\t%d\n")
	call pargi (bp-3)	; call mptype (dtype)
	call pargi (locva (Memi[bp-3]))
    call eprintf ("     nelem = 0x%-15x  %-16d\t%d\n")
	call pargi (bp-2)	; call pargi (Memi[bp-2])
	call pargi (locva (Memi[bp-2]))
    call eprintf ("L sentinal = 0x%-15x  %-16d\t%d\n")
	call pargi (bp-1)	; call pargi (Memi[bp-1])
	call pargi (locva (Memi[bp-1]))


    # Print the values.
    call eprintf ("      data =    ")
    for (i=0; i < NVALS; i=i+1) {
	switch (dtype) {
	case TY_BOOL:
	    call eprintf (" %3b\t\t\t\t\t%-15d")
		call pargb (Memb[p+i])
		call pargi (locva(Memb[p+i]))
	case TY_CHAR:
	    call eprintf (" %3c\t\t\t\t\t%-15d")
		call pargc (Memc[p+i])
		call pargi (locva(Memc[p+i]))
	case TY_SHORT:
	    call eprintf (" %3d\t\t\t\t\t%-15d")
		call pargs (Mems[p+i])
		call pargi (locva(Mems[p+i]))
	case TY_INT:
	    call eprintf (" %3d\t\t\t\t\t%-15d")
		call pargi (Memi[p+i])
		call pargi (locva(Memi[p+i]))
	case TY_LONG:
	    call eprintf (" %3d\t\t\t\t\t%-15d")
		call pargl (Meml[p+i])
		call pargi (locva(Meml[p+i]))
	case TY_REAL:
	    call eprintf (" %3g\t\t\t\t\t%-15d")
		call pargr (Memr[p+i])
		call pargi (locva(Memr[p+i]))
	case TY_DOUBLE:
	    call eprintf (" %3g\t\t\t\t\t%-15d")
		call pargd (Memd[p+i])
		call pargi (locva(Memd[p+i]))
	case TY_COMPLEX:
	    call eprintf (" %3x\t\t\t\t\t%-15d")
		call pargx (Memx[p+i])
		call pargi (locva(Memx[p+i]))
	case TY_STRUCT:
	    call eprintf (" %3d\t\t\t\t\t%-15d")
		call pargi (Memi[p+i])
		call pargi (locva(Memi[p+i]))
	case TY_POINTER:
	    call eprintf (" %3d\t\t\t\t\t%-15d")
		call pargi (Memi[p+i])
		call pargi (locva(Memi[p+i]))
	}
	call eprintf ("\n")
	if (i < (NVALS-1))
	    call eprintf ("\t\t")
    }

    lwl = Memi[bp-4]
    call eprintf ("U sentinal = 0x%-15x  %-15d\t\t%d\n\n")
	call pargi (lwl)	; call pargi (Memi[lwl])
	call pargi (locva (Memi[lwl]))

    call mfree (p, dtype)
end


procedure mt_overflow (dtype)

int	dtype

int	i
real	x
double	xx
pointer	p

begin
    call calloc (p, NVALS, dtype)

    # Set the values.
    for (i=0; i < NVALS + 4; i=i+1) {
	x = i 	; xx = i
	switch (dtype) {
	case TY_BOOL:	  Memb[p+i] = TRUE
	case TY_CHAR:	  Memc[p+i] = 'a' + i
	case TY_SHORT:	  Mems[p+i] = i
	case TY_INT:	  Memi[p+i] = i
	case TY_LONG:	  Meml[p+i] = i
	case TY_REAL:	  Memr[p+i] = x
	case TY_DOUBLE:	  Memd[p+i] = xx
	case TY_COMPLEX:  Memx[p+i] = cmplx(x,-x)
	case TY_STRUCT:   Memi[p+i] = i
	case TY_POINTER:  Memi[p+i] = i
	}
    }

    call mfree (p, dtype)
end


procedure mt_underflow (dtype)

int	dtype

int	i
real	x
double	xx
pointer	p

begin
    call calloc (p, NVALS, dtype)

    # Set the values.
    for (i=0; i < NVALS; i=i+1) {
	x = i 	; xx = i
	switch (dtype) {
	case TY_BOOL:	  Memb[p+i] = TRUE		; Memb[p-1] = FALSE
	case TY_CHAR:	  Memc[p+i] = 'a' + i		; Memc[p-1] = '0'
	case TY_SHORT:	  Mems[p+i] = i			; Mems[p-1] = 999
	case TY_INT:	  Memi[p+i] = i			; Memi[p-1] = 999
	case TY_LONG:	  Meml[p+i] = i			; Meml[p-1] = 999
	case TY_REAL:	  Memr[p+i] = x			; Memr[p-1] = 999
	case TY_DOUBLE:	  Memd[p+i] = xx		; Memd[p-1] = 999
	case TY_COMPLEX:  Memx[p+i] = cmplx(x,-x)	; Memx[p-1] = 999
	case TY_STRUCT:   Memi[p+i] = i			; Memi[p-1] = 999
	case TY_POINTER:  Memi[p+i] = i			; Memi[p-1] = 999
	}
    }

    call mfree (p, dtype)
end


procedure mptype (dtype)
int	dtype
begin
    switch (dtype) {
    case TY_BOOL:	call pargstr ("TY_BOOL   ")
    case TY_CHAR:	call pargstr ("TY_CHAR   ")
    case TY_SHORT:	call pargstr ("TY_SHORT  ")
    case TY_INT:	call pargstr ("TY_INT    ")
    case TY_LONG:	call pargstr ("TY_LONG   ")
    case TY_REAL:	call pargstr ("TY_REAL   ")
    case TY_DOUBLE:	call pargstr ("TY_DOUBLE ")
    case TY_COMPLEX:    call pargstr ("TY_COMPLEX")
    case TY_STRUCT:     call pargstr ("TY_STRUCT ")
    case TY_POINTER:    call pargstr ("TY_POINTER")
    }
end



# Generic Mem_ test assignment.

define	NAVALS		4



procedure mt_auto_b (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_BOOL)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_BOOL)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Memb[ip+i] = TRUE
	call eprintf ("[ %b %b %b %b ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargb (Memb[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_BOOL)
    else
    	call sfree (sp)
end



procedure mt_auto_c (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_CHAR)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_CHAR)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Memc[ip+i] = 'a' + i
	call eprintf ("[ %-3c %-3c %-3c %-3c ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargc (Memc[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_CHAR)
    else
    	call sfree (sp)
end



procedure mt_auto_s (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_SHORT)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_SHORT)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Mems[ip+i] = i
	call eprintf ("[ %-3d %-3d %-3d %-3d ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargs (Mems[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_SHORT)
    else
    	call sfree (sp)
end



procedure mt_auto_i (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_INT)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_INT)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Memi[ip+i] = i
	call eprintf ("[ %-3d %-3d %-3d %-3d ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargi (Memi[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_INT)
    else
    	call sfree (sp)
end



procedure mt_auto_l (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_LONG)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_LONG)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Meml[ip+i] = i
	call eprintf ("[ %-3d %-3d %-3d %-3d ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargl (Meml[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_LONG)
    else
    	call sfree (sp)
end



procedure mt_auto_r (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_REAL)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_REAL)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Memr[ip+i] = i
	call eprintf ("[ %-3g %-3g %-3g %-3g ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargr (Memr[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_REAL)
    else
    	call sfree (sp)
end



procedure mt_auto_d (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_DOUBLE)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_DOUBLE)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1)
	    Memd[ip+i] = i
	call eprintf ("[ %-3g %-3g %-3g %-3g ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargd (Memd[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_DOUBLE)
    else
    	call sfree (sp)
end



procedure mt_auto_x (ty, model)

char	ty[ARB]
int	model

int	i
real	x
pointer	sp, ip

begin
    call eprintf ("  %s\t    ")
	call pargstr (ty)

    if (model == MT_HEAP) {
    	call malloc (ip, NAVALS, TY_COMPLEX)
    } else {
    	call smark (sp)
    	call salloc (ip, NAVALS, TY_COMPLEX)
    }


    call eprintf ("0x%-15x %-15d\t   ")
	call pargi(ip)
	call pargi(ip)

    x = 0.0
        for (i=0; i < NAVALS; i=i+1) {
	    x = i
	    Memx[ip+i] = cmplx(x,0.1)
	}
	call eprintf ("[ %x  %x  %x  %x  ]\n")
        for (i=0; i < NAVALS; i=i+1)
	    call pargx (Memx[ip+i])


    if (model == MT_HEAP)
    	call mfree (ip, TY_COMPLEX)
    else
    	call sfree (sp)
end


