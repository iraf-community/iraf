
                module          zsvjmp
*
*  int status, jmpbuf[14];
*       ...
*       if (zsvjmp_ (&jmpbuf, &status) != 0);
*               printf ("we were called through zdojmp\n");
*       else
*               printf ("we were calling zsvjmp\n");
*
 
*  Save the callers context for a later restore in zdojmp (&jmpbuf, &status)
*
*  Strategy:  Do not mess with the stack frame at all (NONSTANDARD); i.e., retain
*  the callers stack frame.  Save everything, including the status address to which
*  we will later return a status.  We are called with a stack that looks like
*
*                          SP offset
*       |----------------|
* SP -> | return address |  0
*       |----------------|
*       | jmpbuf address |  4
*       |----------------|
*       | status address |  8
*       |----------------|
 
                data
                entry           zsvjmp_
zsvjmp_:        equ             *
                jmp.l           zsvjmp_pure
 
                text
zsvjmp_pure:    procedure       "zsvjmp",NONSTANDARD    no code, nothing
                movea.l         (4.w,a7),a0             address to save to
                movea.l         (8.w,a7),a1             status address
                move.l          d2,(a0)+                save ...
                move.l          d3,(a0)+                .
                move.l          d4,(a0)+                .
                move.l          d5,(a0)+                all ...
                move.l          d6,(a0)+                .
                move.l          d7,(a0)+                .
                movea.l         a2,(a0)+                the ...
                movea.l         a3,(a0)+                .
                movea.l         a4,(a0)+                registers.
                movea.l         a5,(a0)+                callers data base
                movea.l         a6,(a0)+                callers stack base
                movea.l         a7,(a0)+                callers stack ptr
                move.l          (a7),(a0)+              save return address
                move.l          a1,(a0)+                save status address
                move.l          #0,(a1)                 set return status
                rts                                     we never linked, so ...
*
*                          SP offset
*       |----------------|
*       | return address |  0
*       |----------------|
*       | jmpbuf address |  4
*       |----------------|
*       | status address |  8
*       |----------------|
 
                data
                entry           zdojmp_
zdojmp_:        equ             *
                jmp.l           zdojmp_pure
 
                text
zdojmp_pure:    procedure       "zdojmp",NONSTANDARD    don't mess, please
                movea.l         (4.w,a7),a0             jmpbuf address
                movea.l         (8.w,a7),a1             return status address
                move.l          (a1),d0                 get return status
                move.l          (a0)+,d2
                move.l          (a0)+,d3
                move.l          (a0)+,d4
                move.l          (a0)+,d5
                move.l          (a0)+,d6
                move.l          (a0)+,d7
                movea.l         (a0)+,a2
                movea.l         (a0)+,a3
                movea.l         (a0)+,a4
                movea.l         (a0)+,a5                orig data base
                movea.l         (a0)+,a6                orig stack base
                movea.l         (a0)+,a7                orig stack ptr
 
                move.l          (a0)+,(a7)              return adress
                move.l          (a0)+,a1                orig status address
                move.l          d0,(a1)                 set orig return status
                rts                                     we never linked
 
                end
 
