      subroutine trmout (name)
      integer name (100)
      integer tname(9)
      data tname(1)/47/,tname(2)/100/,tname(3)/101/,tname(4)/118/,tname(
     *5)/47/,tname(6)/116/,tname(7)/116/,tname(8)/121/,tname(9)/-2/
      call scopy (tname, 1, name, 1)
      return
      end
