#-h-  puttok			  198  local   12/01/80  15:54:34
# puttok-put token into eval stack
   include  defs

    subroutine puttok (str)
    character str (MAXTOK)

    integer i

    for (i = 1; str (i) != EOS; i = i + 1)
       call putchr (str (i))
    return
    end
