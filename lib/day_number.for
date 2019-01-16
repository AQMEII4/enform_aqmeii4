C
C----------------------------------------------------------DAY_NUMBER---
C
      INTEGER FUNCTION DAY_NUMBER (YEAR,MONTH,DAY)
C
C --- PURPOSE: Computes a DAY_NUMBER. One of the more useful applications
C              for this routine is to compute the number of days between
C              two dates.
C              Oct. 15, 1582 will return a DAY_NUMBER of unity.
C
C --- AUTHOR:  
C
C --- INPUT:   YEAR, MONTH, DAY
C
C --- OUTPUT:  DAY_NUMBER
C
C --- CALLS:   LEAP_YEAR
C
C --- VERSION: 
C
C----------------------------------------------------------DAY_NUMBER---
C
      IMPLICIT NONE

      INTEGER CDPY(0:11)
      INTEGER NDAY, YEAR, MONTH, DAY, I
      LOGICAL LEAP_YEAR
C
C --- Cumulative days per year beginning on the first day of the month.
C
      DATA (CDPY(I),I=0,11) /0,31,59,90,120,151,181,212,243,273,304,334/
C
C --- The integer 577747 is simply an offset so that Oct 15, 1582 returns
C     a day number of one.
C
      NDAY = (YEAR-1)*365.25 + CDPY(MONTH-1) + DAY - 577747
C 
      IF (LEAP_YEAR(YEAR)) THEN
C
          IF (MONTH .GT. 2) THEN
C
C --- Add one day for the current leap year.
C
          NDAY = NDAY + 1
C
        ENDIF
      ENDIF
C
      DAY_NUMBER = NDAY
C
      RETURN
      END

