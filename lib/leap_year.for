C
C-----------------------------------------------------------LEAP_YEAR---
C
      LOGICAL FUNCTION LEAP_YEAR (YEAR)
C
C --- PURPOSE: Check for a leap year.  LEAP_YEAR is returned as true if
C              YEAR is a leap year, and false otherwise.
C              Leap years are years that are evenly divisible by 4, 
C              except years that are evenly divisible by 100 must be 
C              divisible by 400.  
C              NOTE: Gregorian calendar adopted Oct. 15, 1582.
C
C
C --- INPUT:   YEAR
C
C --- OUTPUT:  .TRUE. or .FALSE.
C
C --- CALLS:   none
C
C --- VERSION: May 20, 1995
C
C-----------------------------------------------------------LEAP_YEAR---
C
      IMPLICIT NONE
C
      INTEGER YEAR
C

      IF ((YEAR-(YEAR/400)*400) .EQ. 0) THEN
          LEAP_YEAR = .TRUE.
      ELSEIF ((YEAR-(YEAR/100)*100).EQ.0) THEN
          LEAP_YEAR  = .FALSE.
      ELSEIF ((YEAR-(YEAR/4)*4).EQ.0) THEN
C       Leap year.
          LEAP_YEAR  = .TRUE.
      ELSE
          LEAP_YEAR = .FALSE.
      ENDIF
C
      RETURN
      END
