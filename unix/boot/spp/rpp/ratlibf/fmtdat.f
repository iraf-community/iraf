      SUBROUTINE FMTDAT(DATE, TIME, NOW, FORM)
      INTEGER DATE(100), TIME(100)
      INTEGER NOW(7), FORM
      DATE(1) = NOW(2) / 10 + 48
      DATE(2) = MOD(NOW(2), 10) + 48
      DATE(3) = 47
      DATE(4) = NOW(3) / 10 + 48
      DATE(5) = MOD(NOW(3), 10) + 48
      DATE(6) = 47
      DATE(7) = MOD(NOW(1), 100) / 10 + 48
      DATE(8) = MOD(NOW(1), 10) + 48
      DATE(9) = -2
      TIME(1) = NOW(4) / 10 + 48
      TIME(2) = MOD(NOW(4), 10) + 48
      TIME(3) = 58
      TIME(4) = NOW(5) / 10 + 48
      TIME(5) = MOD(NOW(5), 10) + 48
      TIME(6) = 58
      TIME(7) = NOW(6) / 10 + 48
      TIME(8) = MOD(NOW(6), 10) + 48
      TIME(9) = -2
      RETURN
      END
