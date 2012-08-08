C------------------------------------------------------------------------------
        subroutine ftgmsg(text)

C       get error message from top of stack and shift the stack up one message
        character*(*) text
        call ftxmsg(-1,text)
        end
