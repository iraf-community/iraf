C------------------------------------------------------------------------------
        subroutine ftxmsg(action,text)

C       get, put, or clear the error message stack 

        integer action
        character*(*) text

        integer nbuff,i
        parameter (nbuff=50)
        character*80 txbuff(nbuff)
        save txbuff
        data txbuff/nbuff * ' '/

        if (action .eq. -1)then

C           get error message from top of stack and shift the stack up one
            text=txbuff(1)
            do 10 i=1,nbuff-1
                txbuff(i) = txbuff(i+1)
 10         continue
            txbuff(nbuff)=' '

        else if (action .eq. 1)then

C           put error message onto stack.
            do 20 i=1,nbuff
                if (txbuff(i) .eq. ' ')then
                   txbuff(i)=text
                   return
                end if
20          continue
C           stack is full so discard oldest message
            do 25 i=1,nbuff-1
                txbuff(i) = txbuff(i+1)
25          continue
            txbuff(nbuff)=text

        else if (action .eq. 0)then

C           clear the error message stack
            do 30 i=1,nbuff
                txbuff(i) = ' '
30          continue

        end if
        end
