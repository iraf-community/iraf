include	defs

# type - determine type of character

   character function type (c)

   character c

   if ((LETA <= c & c <= LETZ) | (BIGA <= c & c <= BIGZ))
      type = LETTER
   else if (DIG0 <= c & c <= DIG9)
      type = DIGIT
   else
      type = c

   # The original version used a table look-up; you'll have to
   # use that method if you have subverted the convention to
   # use ASCII characters internally:
   # integer index
   # character digits(11), lowalf(27), upalf(27)
   # data digits(1) /DIG0/
   # data digits(2) /DIG1/
   # data digits(3) /DIG2/
   # data digits(4) /DIG3/
   # data digits(5) /DIG4/
   # data digits(6) /DIG5/
   # data digits(7) /DIG6/
   # data digits(8) /DIG7/
   # data digits(9) /DIG8/
   # data digits(10) /DIG9/
   # data digits(11) /EOS/
   #
   # data lowalf(1) /LETA/
   # data lowalf(2) /LETB/
   # data lowalf(3) /LETC/
   # data lowalf(4) /LETD/
   # data lowalf(5) /LETE/
   # data lowalf(6) /LETF/
   # data lowalf(7) /LETG/
   # data lowalf(8) /LETH/
   # data lowalf(9) /LETI/
   # data lowalf(10) /LETJ/
   # data lowalf(11) /LETK/
   # data lowalf(12) /LETL/
   # data lowalf(13) /LETM/
   # data lowalf(14) /LETN/
   # data lowalf(15) /LETO/
   # data lowalf(16) /LETP/
   # data lowalf(17) /LETQ/
   # data lowalf(18) /LETR/
   # data lowalf(19) /LETS/
   # data lowalf(20) /LETT/
   # data lowalf(21) /LETU/
   # data lowalf(22) /LETV/
   # data lowalf(23) /LETW/
   # data lowalf(24) /LETX/
   # data lowalf(25) /LETY/
   # data lowalf(26) /LETZ/
   # data lowalf(27) /EOS/
   #
   # data upalf(1) /BIGA/
   # data upalf(2) /BIGB/
   # data upalf(3) /BIGC/
   # data upalf(4) /BIGD/
   # data upalf(5) /BIGE/
   # data upalf(6) /BIGF/
   # data upalf(7) /BIGG/
   # data upalf(8) /BIGH/
   # data upalf(9) /BIGI/
   # data upalf(10) /BIGJ/
   # data upalf(11) /BIGK/
   # data upalf(12) /BIGL/
   # data upalf(13) /BIGM/
   # data upalf(14) /BIGN/
   # data upalf(15) /BIGO/
   # data upalf(16) /BIGP/
   # data upalf(17) /BIGQ/
   # data upalf(18) /BIGR/
   # data upalf(19) /BIGS/
   # data upalf(20) /BIGT/
   # data upalf(21) /BIGU/
   # data upalf(23) /BIGW/
   # data upalf(24) /BIGX/
   # data upalf(25) /BIGY/
   # data upalf(26) /BIGZ/
   # data upalf(27) /EOS/
   #
   # if (index(lowalf, c) > 0)
   #	    type = LETTER
   # else if (index(upalf,c) >0)
   #	    type = LETTER
   # else if (index(digits,c) > 0)
   #	    type = DIGIT
   # else
   #	    type = c


   return
   end
