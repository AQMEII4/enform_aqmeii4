C
C------------------------------------------------------------GET_DATE---
C
      SUBROUTINE GET_DATE( DAY_NUMBER, YEAR, MONTH, DAY )
C
C --- PURPOSE: Computes the day (YEAR, MONTH, DAY) from the Julian
C              day DAY_NUMBER.
C              A DAY_NUMBER of unity will return Oct. 15, 1582.
C
C --- AUTHOR:  R.Bianconi - ENVIROWARE SRL
C
C --- INPUT:   DAY_NUMBER
C
C --- OUTPUT:  YEAR, MONTH, DAY
C
C --- CALLS:   LEAP_YEAR
C
C --- VERSION: 2005-12-01 RBe: Corretto bug che si creava nel mese di
C                              dicembre (vedi vecchie linee di codice).
C                              Il contatore k diventava 11, k+1=12, e la
C                              routine puntava su CDPY(12), che e'
C                              dimensionato fino a 11 ...
C              2008-10-10 RBe: Corretto bug che si cerificava l'ultimo
C                              giorno (31 dic) di ogni anno bisestile;
C                              il giorno veniva calcolato come -334
C                              anziche' 31.
C
C------------------------------------------------------------GET_DATE---
C
      INTEGER CDPY(0:11)
      INTEGER YEAR, MONTH, DAY, DAY_NUMBER, DN
      LOGICAL LEAP_YEAR, LOOP
      REAL RYEAR
      REAL EPS
      PARAMETER( EPS = 1.0E-6 )
C
C --- Cumulative days per year beginning on the first day of the month.
C
C was DATA (CDPY(I),I=0,11) /0,31,59,90,120,151,181,212,243,273,304,334/
C
      CDPY( 0) =   0
      CDPY( 1) =  31
      CDPY( 2) =  59
      CDPY( 3) =  90
      CDPY( 4) = 120
      CDPY( 5) = 151
      CDPY( 6) = 181
      CDPY( 7) = 212
      CDPY( 8) = 243
      CDPY( 9) = 273
      CDPY(10) = 304
      CDPY(11) = 334
C
      LOOP = .TRUE.
C
      DN = DAY_NUMBER
C
C --- The integer 577747 is simply an offset so that Oct 15, 1582 returns
C     a day number of one.
C
C     RBe 2008-10-10 -->
C      YEAR = (DN + 577747) / 365.25 + 1
      RYEAR = REAL(DN + 577747) / 365.25
      YEAR = INT(RYEAR)
      IF( (RYEAR-YEAR) .GT. EPS ) YEAR = YEAR + 1
C     <-- RBe 2008-10-10
C
      IF (LEAP_YEAR(YEAR)) THEN
C
C --- Add one day for the current leap year.
C
          DO I = 2, 11
             CDPY(I) = CDPY(I) + 1
          END DO
C
      ENDIF
C
      DAYS = (DN + 577747) - INT ((YEAR-1)*365.25)
C
      K = 0
      DO WHILE (LOOP)

        IF( K .GT. 10 ) THEN
          LOOP = .FALSE.
        ELSE
          IF( (DAYS .GT. CDPY(K) .AND. DAYS .LE. CDPY(K+1)) )
     &        LOOP = .FALSE.
        ENDIF

C RBe 20051201 Vecchie righe errate
C         IF( (DAYS .GT. CDPY(K) .AND. DAYS .LE. CDPY(K+1)) .OR.
C     &       K .GT. 10 ) LOOP = .FALSE.
C
         K = K + 1
      END DO
C
      MONTH = K
      DAY = DAYS - CDPY(K-1)
C
      RETURN
      END

