C-------------------------------------------------------------FIXNAME---

      SUBROUTINE FIXNAME(BUFF)
C
C-------------------------------------------------------------FIXNAME---
C
      PARAMETER( NCHA = 38 )
      CHARACTER BUFF*(*)
      CHARACTER TMPBUFF*8192
      CHARACTER*1 CAP( NCHA ), SMA( NCHA )
C
      DATA CAP/ '-', 'A', 'B', 'C', 'D', 'E', 'F', 'G','H', 'I', 'J',
     &'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
     &'W', 'X','Y', 'Z','0','1','2','3','4','5','6','7','8','9','.'/
      DATA SMA/ '-', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
     &'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
     &'w', 'x', 'y', 'z','0','1','2','3','4','5','6','7','8','9','.'/
C
      LB = LEN(BUFF)
C
      TMPBUFF = BUFF
C
      KK = 0
      DO K = 1, LB
         BUFF(K:K) = ' '
         DO J = 1, NCHA
            IF(TMPBUFF(K:K) .EQ. SMA(J) .OR.
     &         TMPBUFF(K:K) .EQ. CAP(J)) THEN
                KK = KK + 1
                BUFF(KK:KK) = CAP(J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
