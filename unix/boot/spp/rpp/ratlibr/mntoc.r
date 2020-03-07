include	defs

# mntoc --- translate ASCII mnemonic into a character

   character function mntoc (buf, p, defalt)
   character buf (ARB), defalt
   integer p

   integer i, tp
   integer equal

   character c, tmp (MAXLINE)

   character text (170)
   data text / _
      ACK, LETA, LETC, LETK, EOS,
      BEL, LETB, LETE, LETL, EOS,
      BS,  LETB, LETS, EOS,  EOS,
      CAN, LETC, LETA, LETN, EOS,
      CR,  LETC, LETR, EOS,  EOS,
      DC1, LETD, LETC, DIG1, EOS,
      DC2, LETD, LETC, DIG2, EOS,
      DC3, LETD, LETC, DIG3, EOS,
      DC4, LETD, LETC, DIG4, EOS,
      DEL, LETD, LETE, LETL, EOS,
      DLE, LETD, LETL, LETE, EOS,
      EM,  LETE, LETM, EOS,  EOS,
      ENQ, LETE, LETN, LETQ, EOS,
      EOT, LETE, LETO, LETT, EOS,
      ESC, LETE, LETS, LETC, EOS,
      ETB, LETE, LETT, LETB, EOS,
      ETX, LETE, LETT, LETX, EOS,
      FF,  LETF, LETF, EOS,  EOS,
      FS,  LETF, LETS, EOS,  EOS,
      GS,  LETG, LETS, EOS,  EOS,
      HT,  LETH, LETT, EOS,  EOS,
      LF,  LETL, LETF, EOS,  EOS,
      NAK, LETN, LETA, LETK, EOS,
      NUL, LETN, LETU, LETL, EOS,
      RS,  LETR, LETS, EOS,  EOS,
      SI,  LETS, LETI, EOS,  EOS,
      SO,  LETS, LETO, EOS,  EOS,
      SOH, LETS, LETO, LETH, EOS,
      SP,  LETS, LETP, EOS,  EOS,
      STX, LETS, LETT, LETX, EOS,
      SUB, LETS, LETU, LETB, EOS,
      SYN, LETS, LETY, LETN, EOS,
      US,  LETU, LETS, EOS,  EOS,
      VT,  LETV, LETT, EOS,  EOS/

   tp = 1
   repeat {
      tmp (tp) = buf (p)
      tp = tp + 1
      p = p + 1
      } until (! ((BIGA <= buf(p) & buf(p) <= BIGZ)
                | (LETA <= buf(p) & buf(p) <= LETZ)
                | (DIG0 <= buf(p) & buf(p) <= DIG9))
		  | tp >= MAXLINE)
   tmp (tp) = EOS

   if (tp == 2)
      c = tmp (1)
   else {
      call lower (tmp)
      for (i = 1; i < 170; i = i + 5)  # should use binary search here
	 if (equal (tmp, text (i + 1)) == YES)
	    break
      if (i < 170)
	 c = text (i)
      else
	 c = defalt
      }

   return (c)
   end
