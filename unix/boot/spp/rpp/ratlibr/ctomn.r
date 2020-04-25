include	defs

# ctomn --- translate ASCII control character to mnemonic string

   integer function ctomn (c, rep)
   character c, rep (4)

   integer i
   integer length

   character mntext (136)     # 4 chars/mnemonic; 32 control chars + SP + DEL
   data mntext / _
      BIGN, BIGU, BIGL, EOS,
      BIGS, BIGO, BIGH, EOS,
      BIGS, BIGT, BIGX, EOS,
      BIGE, BIGT, BIGX, EOS,
      BIGE, BIGO, BIGT, EOS,
      BIGE, BIGN, BIGQ, EOS,
      BIGA, BIGC, BIGK, EOS,
      BIGB, BIGE, BIGL, EOS,
      BIGB, BIGS, EOS,	EOS,
      BIGH, BIGT, EOS,	EOS,
      BIGL, BIGF, EOS,	EOS,
      BIGV, BIGT, EOS,	EOS,
      BIGF, BIGF, EOS,	EOS,
      BIGC, BIGR, EOS,	EOS,
      BIGS, BIGO, EOS,	EOS,
      BIGS, BIGI, EOS,	EOS,
      BIGD, BIGL, BIGE, EOS,
      BIGD, BIGC, DIG1, EOS,
      BIGD, BIGC, DIG2, EOS,
      BIGD, BIGC, DIG3, EOS,
      BIGD, BIGC, DIG4, EOS,
      BIGN, BIGA, BIGK, EOS,
      BIGS, BIGY, BIGN, EOS,
      BIGE, BIGT, BIGB, EOS,
      BIGC, BIGA, BIGN, EOS,
      BIGE, BIGM, EOS,	EOS,
      BIGS, BIGU, BIGB, EOS,
      BIGE, BIGS, BIGC, EOS,
      BIGF, BIGS, EOS,	EOS,
      BIGG, BIGS, EOS,	EOS,
      BIGR, BIGS, EOS,	EOS,
      BIGU, BIGS, EOS,	EOS,
      BIGS, BIGP, EOS,	EOS,
      BIGD, BIGE, BIGL, EOS/

   i = mod (max(c,0), 128)
   if (0 <= i & i <= 32)     # non-printing character or space
      call scopy (mntext, 4 * i + 1, rep, 1)
   else if (i == 127)	      # rubout (DEL)
      call scopy (mntext, 133, rep, 1)
   else {		      # printing character
      rep (1) = c
      rep (2) = EOS
      }

   return (length (rep))
   end
