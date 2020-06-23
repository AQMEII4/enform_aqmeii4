C--------------------------------------------------------------------------------------
C Start AQR_HIRES_PROCESSING block - vers 20150101
C--------------------------------------------------------------------------------------

C --- Prepare .ens output filename

      OUTFILE = ' '
      WRITE(OUTFILE( 1: 5),'(I5.5)'      ) USER_MODEL_NUMBER
      WRITE(OUTFILE( 6: 6),'(A)'         ) '-'
      WRITE(OUTFILE( 7:10),'(I4.4)'      ) SOURCE_SEQUENCE_NUMBER
      WRITE(OUTFILE(11:11),'(A)'         ) '-'
      WRITE(OUTFILE(12:14),'(I3.3)'      ) SOURCE_CASE_NUMBER
      WRITE(OUTFILE(15:15),'(A)'         ) '-'
      WRITE(OUTFILE(16:17),'(I2.2)'      ) ICODE
      WRITE(OUTFILE(18:18),'(A)'         ) '-'
      WRITE(OUTFILE(19:20),'(I2.2)'      ) UI
      WRITE(OUTFILE(21:21),'(A)'         ) '-'
      WRITE(OUTFILE(22:33),'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)
      WRITE(OUTFILE(34:37),'(A4)'        ) '.ens'
      IF (LDEBUG) THEN
          WRITE(6,*) '.ens output file ---> ',  OUTFILE
      ENDIF

C --- Write output file headers
      
      OPEN (LOUT, FILE=OUTFILE)
      WRITE(LOUT,'(I5.5)') USER_MODEL_NUMBER
      WRITE(LOUT,'(A7)') SOURCE_RANDOM_KEY
      WRITE(LOUT,'(I4.4)') SOURCE_SEQUENCE_NUMBER
      WRITE(LOUT,'(I3.3)') SOURCE_CASE_NUMBER
      WRITE(LOUT,'(A20)') USER_VAR
      WRITE(LOUT,'(I2)') UI
      WRITE(LOUT,'(E11.4)') VAL_MISSING
      PREC = VAR_ITEMS_PRECISION(ICODE,UI)
      IF (PREC. LE. 0) GOTO 720 
      WRITE(LOUT,'(E11.4)') PREC
      WRITE(LOUT,'(I6.6)') NT
      WRITE(LOUT,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)

C --- Compute output dates

      DELTA_DD = 0
      DELTA_HH = 0
      DELTA_MN = 0
      DELTA_SS = 0

      CALL GENERATE_DATES_HIRES(SOURCE_FIRST_OUTPUT,OUTPUT_DATES,
     &                    DELTA_DD,
     &                    DELTA_HH,
     &                    DELTA_MN,
     &                    DELTA_SS,
     &                    DELTA_CC,
     &                    NT)

C --- Read and store values
      IF (LDEBUG) THEN
          WRITE(6,*) ' '
          WRITE(6,*) 'Reading model output data....'
          WRITE(6,*) ' '
      ENDIF
 
C --- Initialize IRINDEX for possible first receptor all missing (RBI 20130715)
      IRINDEX = 0

C --- Loop over receptors
      DO IR = 1, SOURCE_NDISCREC
         READ(LUN,*,ERR=940) IRINDEX0, RNZ
         IF( SOURCE_RECEPTOR_INDEX(IR) .NE. IRINDEX0 ) GOTO 950
C ---    RNZ is the number of vertical measurements and a flag for missing
C        values for all the times at current receptor

         ! Search max value in input - RBI 20130101
         VMAX_INFO = VAL_MISSING

         ALL_MISSING = .FALSE.
         IF( RNZ .EQ. VAL_MISSING ) THEN
C ---      All values are missing
           NZ = 1
           DO IT = 1, NT
             VAL(IT,NZ) = VAL_MISSING
           ENDDO
           ALL_MISSING = .TRUE.

         ELSE
C ---      Vertical levels 
           NZ = NINT(RNZ)

C ---      Coordinates
           READ(LUN,*,ERR=940) IRINDEX, XREC, YREC, ( ZREC(K), K=1,NZ )
           IF( IRINDEX .NE. IRINDEX0 ) GOTO 960

C ---      Time series
           DO IT = 1, NT
              READ(LUN,'(A)',ERR=940) DUMMY
              DO WHILE( DUMMY(1:1) .EQ. ' ' )
                DUMMY = DUMMY(2:)
              ENDDO
              OPEN(LSCR,STATUS='SCRATCH')
              WRITE(LSCR,'(A)') DUMMY(17:)
              REWIND(LSCR)
              READ(LSCR,*) IRINDEX, ( VAL(IT,K), K=1,NZ )
              CLOSE(LSCR)

              
              READ(DUMMY(1:16),'(I4.4,6I2.2)',ERR=920)
     &                          (READ_DATE(K),K=1,7)

              IF( IRINDEX .NE. IRINDEX0 ) GOTO 960

              IF (LDEBUG_VERBOSE) THEN
                WRITE(6,*) 'Reading date ',(READ_DATE(K),K=1,7)
              ENDIF

C ---         Verify the correct order of dates (dates will be not
C             written within the output file, they will be recreated on
C             the server side)
              DO K = 1, 7
                IF (READ_DATE(K) .NE. OUTPUT_DATES(K,IT)) GOTO 760
              ENDDO

           ENDDO

         ENDIF

C ---    Process data
         IF( ALL_MISSING ) THEN
           IRINDEX = IRINDEX + 1   ! Since it is not read from file
C ---      The whole time series is missing for current receptor
           WRITE(LOUT,*) IRINDEX, RNZ
         ELSE
           WRITE(LOUT,*) IRINDEX, NZ
           DO IT = 1, NT
C ---        Do not write date/time, it will be recreated on server side
             DO K = 1, NZ
               VALUES(K) = VAL(IT,K)

               IF (VALUES(K) .GT. VMAX_INFO) THEN
                   VMAX_INFO = VALUES(K)
               ENDIF
             ENDDO
             CALL ENCODEG(VALUES,VAL_MISSING,PREC,NZ,STRING)
             WRITE(LOUT,'(A)') STRING(1:LSTRIM(STRING))
           ENDDO
         ENDIF
         IF (LDEBUG) THEN
             WRITE(6,*) 'Encoding receptor ---> ', IRINDEX
             WRITE(6,*) 'Maximum value found in file: ',VMAX_INFO
             WRITE(6,*) ' '
         ENDIF

      ENDDO

      CLOSE (LOUT)
