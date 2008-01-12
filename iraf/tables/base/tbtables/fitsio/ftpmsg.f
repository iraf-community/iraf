C------------------------------------------------------------------------------
        subroutine ftpmsg(text)

C       put error message onto stack.
        character*(*) text
        call ftxmsg(1,text)
        end
