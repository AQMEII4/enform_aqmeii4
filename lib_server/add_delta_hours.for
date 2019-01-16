C
C-----------------------------------------------------ADD_DELTA_HOURS---
C
      SUBROUTINE ADD_DELTA_HOURS( IYRI, IMOI, IDYI, IHRI, IDH, ICO,
     &                            IYRF, IMOF, IDYF, IHRF )
C
C     (C) Copyright ENVIROWARE srl
C                   via Dante, 142
C                   20863 Concorezzo (MB) - Italy
C                   Tel. +39.039.6203636
C                   Fax. +39.039.6042946
C                   http://www.enviroware.com
C                   mail: info@enviroware.com
C
C --- PURPOSE:  Incrementa di IDH ore una data
C
C --- INPUT:    IYRI - Anno iniziale (YYYY)
C               IMOI - Mese iniziale (MM)
C               IDYI - Giorno iniziale (DD)
C               IHRI - Ora iniziale (HH)
C               IDH  - Delta ore di incremento
C               ICO  - Flag numerica per convenzione sulle ore
C                      0 - Da 0 a 23
C                      1 - Da 1 a 24
C
C --- OUTPUT:   IYRF - Anno finale (YYYY)
C               IMOF - Mese finale (MM)
C               IDYF - Giorno finale (DD)
C               IHRF - Ora finale (HH)
C
C --- USE:      DAYNUM
C               GET_DATE
C
C --- AUTHOR:   R. Bellasio, 2005-01-26
C
C --- MODIFIED: 2005-01-26
C-----------------------------------------------------ADD_DELTA_HOURS---
C
      INTEGER*4 DAY_NUMBER, IJI, IJF

      IF( ICO .EQ. 0 ) THEN 
        ILASTH = 23
      ELSEIF( ICO .EQ. 1 ) THEN 
        ILASTH = 24
      ELSE
        GOTO 910
      ENDIF
C --- Questo risparmia tempo (?) forse
      IF( IDH .EQ. 0 ) THEN
          IYRF = IYRI
          IMOF = IMOI
          IDYF = IDYI
          IHRF = IHRI
          RETURN
      ENDIF
      
C --- Numero giorni di incremento
      IDJ = INT( IDH / 24 )

C --- Numero ore rimanenti (tolti i giorni)
      IDHH = IDH - IDJ * 24

C --- Giorno Giuliano iniziale
      IJI = DAY_NUMBER( IYRI, IMOI, IDYI )

C --- Giorno Giuliano finale
      IJF = IJI + IDJ

C --- Verifica se ore rimanenti cambiano giorno
      IHRF = IHRI + IDHH
      IF( IHRF .GT. ILASTH ) THEN
          IJF = IJF + 1
          IHRF = IHRF - 24
      ENDIF

C --- Torna al Gregoriano
      CALL GET_DATE( IJF, IYRF, IMOF, IDYF )

      RETURN

 910  CONTINUE
      WRITE(6,*) 'Error in ADD_DELTA_HOURS'
      WRITE(6,*) 'Unknown flag for hour range in one day.'
      WRITE(6,*) 'Known flags are:'
      WRITE(6,*) '0 - From 0 to 23'
      WRITE(6,*) '1 - From 1 to 24'
      STOP

      END
