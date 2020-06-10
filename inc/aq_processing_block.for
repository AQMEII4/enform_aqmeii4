C--------------------------------------------------------------------------------------
C Start PROCESSING block - vers 20130101 - R.Bianconi
C                          vers 20190101 - C.Hogrefe: added MD option
C--------------------------------------------------------------------------------------
C

C --- Prepare .ens output filename 
C     and .stat filename RBI-20130101

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

C            IF (LDEBUG) THEN
C               WRITE(6,*) 'ICODE ',ICODE
C               WRITE(6,*) 'UI ',UI
C               WRITE(6,*) 'Precision ',VAR_ITEMS_PRECISION(ICODE,UI)
C            ENDIF

      IF (PREC. LE. 0) GOTO 720 
      WRITE(LOUT,'(E11.4)') PREC
      WRITE(LOUT,'(I4.4)') NT
      WRITE(LOUT,'(I4.4,4I2.2)') (USER_METEO_DATE(K),K=1,5)

      ! Search max value in input - RBI 20130101
      VMAX_INFO = VAL_MISSING
      IF (LDEBUG) THEN
          DO IT = 1, NT
             DO J = NY, 1, -1
                DO I = 1, NX
                   IF (VALUES_GR(I,J,IT) .GT. VMAX_INFO) THEN
                       VMAX_INFO = VALUES_GR(I,J,IT)
                   ENDIF
                ENDDO
             ENDDO
          ENDDO
          WRITE(6,*) ' '
          WRITE(6,*) 'Maximum value found in file: ',VMAX_INFO
          WRITE(6,*) ' '
      ENDIF

C --- Process data and optional statistics
      IF (LDEBUG) THEN
        WRITE(6,*) 'Number of time outputs ---> ', NT
      ENDIF

      DO IPOST = 1, SOURCE_ITEM_NPOSTS(ICODE,UI)

         IF (LDEBUG) THEN
             WRITE(6,*) 'Current postprocessing ---> ',  IPOST
         ENDIF

         IAVGH = 0
         ICOUNT = 0
         ITOUT = 0

         IF (LDEBUG) THEN
             WRITE(6,*) 'Current averaging ---> ',  
     &                  AVERAGING(ICODE,UI,IPOST)
         ENDIF

         IF (AVERAGING(ICODE,UI,IPOST) .EQ. 'N') THEN

             WRITE(LOUT,'(I4.4)') NT

C --- No averaging required

             DO IT = 1, NT
                DO J = NY, 1, -1
                   DO I = 1, NX
                      VALUES(I) = VALUES_GR(I,J,IT)
                   ENDDO
                   CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                   WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))
                ENDDO
             ENDDO

             IF (LDEBUG) THEN
                 WRITE(6,*) 'Current statistics ---> NONE'  
             ENDIF

         ELSE
                
C --- Averaging requested
             IF (LDEBUG) THEN
                 WRITE(6,*) 'Current statistics ---> ',
     &           STATISTICS(ICODE,UI,IPOST)
             ENDIF


             IF (AVERAGING(ICODE,UI,IPOST) .EQ. 'D') THEN
                
                 CALL FIND_NTO(NT,'D',NTO)

             ELSEIF (AVERAGING(ICODE,UI,IPOST) .EQ. 'M') THEN

                 CALL FIND_NTO(NT,'M',NTO)

             ELSEIF (AVERAGING(ICODE,UI,IPOST) .EQ. 'Q') THEN

                 CALL FIND_NTO(NT,'Q',NTO)

             ELSEIF (AVERAGING(ICODE,UI,IPOST) .EQ. 'P') THEN

                 IAVGH = NT

             ELSEIF (AVERAGING(ICODE,UI,IPOST) .EQ. 'MD') THEN
                   
                 CALL FIND_NTO(NT,'MD',NTO)

             ELSE    

C             --- Get the length of the averaging period

                 OPEN(IUNIT,STATUS='SCRATCH')
                 WRITE(IUNIT,'(A)') AVERAGING(ICODE,UI,IPOST)
                 REWIND(IUNIT)
                 READ(IUNIT,*) IAVGH
                 CLOSE(IUNIT)

             ENDIF


C             ---  Find the number of time outputs, NTOUT, and the position
C                  MVAL in sorted arrays for MAX and PCT

             IF (AVERAGING(ICODE,UI,IPOST) .EQ. 'D' .OR.
     &           AVERAGING(ICODE,UI,IPOST) .EQ. 'M' .OR.
C     &           AVERAGING(ICODE,UI,IPOST) .EQ. 'MD' .OR.
     &           AVERAGING(ICODE,UI,IPOST) .EQ. 'Q') THEN

                 IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'MAX') THEN
                     MVAL =  NTO !this doesn't work for MD
                     NTOUT = 1
                 ELSEIF (STATISTICS(ICODE,UI,IPOST) .EQ. 'PCT') THEN
                     MVAL = NINT( 
     &               PERCENTILE(ICODE,UI,IPOST) * NTO * 0.01 )
                     NTOUT = 1
                 ELSE
                     NTOUT = NTO
                 ENDIF
             ELSEIF (AVERAGING(ICODE,UI,IPOST) .EQ. 'MD')  THEN 
                     NTOUT = NTO !always use all MD time steps, even if using PCT
             ELSE !period averaging, which uses IAVGH instead of NTO
                 IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'MAX') THEN
                     MVAL = INT(0.5 +  NT / IAVGH )
                     NTOUT = 1
                 ELSEIF (STATISTICS(ICODE,UI,IPOST) .EQ. 'PCT') THEN
                     NTOUT = INT(0.5 +  NT / IAVGH )
                     MVAL = NINT( 
     &               PERCENTILE(ICODE,UI,IPOST) * NTOUT * 0.01 )
                     NTOUT = 1
                 ELSE
                     NTOUT = INT(0.5 +  NT / IAVGH )
                 ENDIF
             ENDIF

             WRITE(LOUT,'(I4.4)') NTOUT
             IF (LDEBUG) THEN
                WRITE(6,*) 'Number of time outputs after stats ---> ',
     &                  NTOUT
             ENDIF

C ---------------------------------------------------------------
C ---        MAX AND PCT
C ---------------------------------------------------------------

             IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'MAX' .OR.
     &           STATISTICS(ICODE,UI,IPOST) .EQ. 'PCT') THEN

                 IF (AVERAGING(ICODE,UI,IPOST) .EQ. 'D' .OR.
     &               AVERAGING(ICODE,UI,IPOST) .EQ. 'M' .OR.
     &               AVERAGING(ICODE,UI,IPOST) .EQ. 'Q') THEN

                     AVGTYPE = AVERAGING(ICODE,UI,IPOST)
                     
                     CALL FIX24(1,OUTPUT_DATES_24)
                     IF (AVGTYPE .EQ. 'D') THEN
                         IFIRST = OUTPUT_DATES_24(3)
                     ELSEIF (AVGTYPE .EQ. 'M') THEN
                         IFIRST = OUTPUT_DATES_24(2)
                     ELSEIF (AVGTYPE .EQ. 'Q') THEN
                         IFIRST = IQUARTER(OUTPUT_DATES_24(2))
                     ENDIF

                     VMAX_INFO = VAL_MISSING
                     DO J = NY, 1, -1

                        DO I = 1, NX

                           SOMMA(I) = 0.
                           ITOUT = 0
                           ICOUNT = 0
                           ICURR = IFIRST

                           LMISS =.FALSE.
                           DO IT = 1, NT

                 ! RBI 20130101  - set to missing the statistics
                 ! if the model has a non valid value
                 IF (ABS(VALUES_GR(I,J,IT) - VAL_MISSING).LT.EPS) THEN 
                     LMISS = .TRUE.
                 ENDIF

                              CALL FIX24(IT,OUTPUT_DATES_24)
                              IF (AVGTYPE .EQ. 'D') THEN
                                  IVAL = OUTPUT_DATES_24(3)
                              ELSEIF (AVGTYPE .EQ. 'M') THEN
                                  IVAL = OUTPUT_DATES_24(2)
                              ELSEIF (AVGTYPE .EQ. 'Q') THEN
                                  IVAL = IQUARTER(OUTPUT_DATES_24(2))
                              ENDIF
                              IF (IVAL .EQ. ICURR) THEN
                                SOMMA(I) = SOMMA(I) + VALUES_GR(I,J,IT)
                                ICOUNT = ICOUNT + 1
                              ELSE
                                  ITOUT = ITOUT + 1
                                  VALUES_OUT(I,ITOUT) = SOMMA(I)/ICOUNT
                                  SOMMA(I) = VALUES_GR(I,J,IT)
                                  ICOUNT = 1
                                  ICURR = IVAL
                              ENDIF

                              IF (IT .EQ. NT) THEN
                                  ITOUT = ITOUT + 1
                                  VALUES_OUT(I,ITOUT) = SOMMA(I)/ICOUNT
                              ENDIF

                           ENDDO
    
! RBI 20130101 OUT         VMAX = 0.
!                          DO IT = 1, ITOUT
!                             ARR(IT) = VALUES_OUT(I,IT)
!                             IF (ARR(IT) .GT. VMAX) THEN
!                                 VMAX = ARR(IT)
!                             ENDIF
!                          ENDDO
!                          IF (VMAX .GT. 0) THEN
!                              CALL SORT( ARR, ITOUT, WARR )
!                              VALUES(I) = WARR(MVAL)
!                          ELSE
!                              VALUES(I) = 0.
!                          ENDIF

! RBI 20130101 IN
                           IF (LMISS) THEN
                               VALUES(I) = VAL_MISSING
                           ELSE 
                               CALL SORT( ARR, ITOUT, WARR ) !not sure this works because ARR appears to be empty
                               VALUES(I) = WARR(MVAL)
                           ENDIF

                        ENDDO

C                       --- Now encode and write
                        CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                        WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))

                        IF (VMAX .GT. VMAX_INFO) THEN !20130101
                            VMAX_INFO = VMAX
                        ENDIF

                     ENDDO
                     IF (LDEBUG) THEN
                      WRITE(6,*) ' '
                      WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                      WRITE(6,*) ' '
                     ENDIF              



              ELSEIF ( AVERAGING(ICODE,UI,IPOST) .EQ. 'MD'  ) THEN  !CHO 20200518

                  AVGTYPE = AVERAGING(ICODE,UI,IPOST)

                  CALL FIX24(1,OUTPUT_DATES_24)
                  IF (AVGTYPE .EQ. 'MD') THEN
                    ICURRMD(1) = (OUTPUT_DATES_24(2) * 100) +
     &              OUTPUT_DATES_24(4)      
                  ENDIF
                     
                  IT = 0
                  NTO = 1
                  ITBEGMD = NT
                  ITENDMD = 0
 
                  DO WHILE (IT .LT. NT)
 
                   IT = IT + 1
      
                   CALL FIX24(IT,OUTPUT_DATES_24)
                   IF (AVGTYPE .EQ. 'MD') THEN
                    IVAL = (OUTPUT_DATES_24(2)* 100) +
     &              OUTPUT_DATES_24(4)
                   ENDIF
       
      
               LMATCHMD = .FALSE.
               DO N = 1, NTO
                    IF (IVAL .EQ. ICURRMD(N)) THEN
             LMATCHMD = .TRUE.
             IF ( IT .LT. ITBEGMD(N) ) ITBEGMD(N) = IT
             IF ( IT .GT. ITENDMD(N) ) ITENDMD(N) = IT
            ENDIF
       
               ENDDO
           
               IF (.NOT. LMATCHMD) THEN
                    NTO = NTO + 1
                    ICURRMD(NTO) = IVAL
            ITBEGMD(NTO) = IT
            ITENDMD(NTO) = IT
                   ENDIF
                      
                  ENDDO
          
c                 DO N = 1, NTO
c           WRITE(*,*) N, ICURRMD(N), ITBEGMD(N), ITENDMD(N)
c          ENDDO 

                     VMAX_INFO = VAL_MISSING        ! RBI 20130101
                     DO ITO = 1, NTO
 
                        DO J = NY, 1, -1

                           DO I = 1, NX
                              SOMMA(I) = 0.

                              LMISS = .FALSE.
                  
                              ICOUNT = 0
                              DO ITT = ITBEGMD(ITO), ITENDMD(ITO), 24

                 ! RBI 20130101  - set to missing the statitics
                 ! if the model has a non valid value
                 IF (ABS(VALUES_GR(I,J,ITT) - VAL_MISSING).LT.EPS) THEN 
                     LMISS = .TRUE.
                 ENDIF
c                                  SOMMA(I) = SOMMA(I)+VALUES_GR(I,J,ITT)
                                  ICOUNT = ICOUNT + 1
				  
                                  ARR(ICOUNT) = VALUES_GR(I,J,ITT)
                  

                              ENDDO

                              MVAL = NINT( PERCENTILE(ICODE,UI,IPOST) 
     &                               * ICOUNT * 0.01 )
			      
                              CALL SORT( ARR, ICOUNT, WARR ) 
                              VALUES(I) = WARR(MVAL)
			       
                          
                              IF (LMISS) THEN ! RBI 20130101
                                  VALUES(I) = VAL_MISSING
                              ENDIF

                              IF (VALUES(I) .GT. VMAX_INFO) THEN !20130101
                                  VMAX_INFO = VALUES(I)
                              ENDIF

                           ENDDO

C                           --- Now encode and write
                        CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                           WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))
               
                           IF (LDEBUG) THEN
                IF (J. EQ. NY/2) THEN   !CHO 20190101
                 WRITE(6,*) 'PCT DIEL MMHH, START STEP , END STEP  ,'//
     &                                 'NUMBER DAYS, SAMPLE VALUE'
                 WRITE(6,*) ICURRMD(ITO), ITBEGMD(ITO), ITENDMD(ITO), 
     &                                 ICOUNT,VALUES(NX/2)
                ENDIF
               ENDIF

                        ENDDO
                     ENDDO

                    IF (LDEBUG) THEN
                     WRITE(6,*) ' '
                     WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                     WRITE(6,*) ' '
                    ENDIF


                 ELSE !not D, M, or Q or MD time period averaging for max / pct, i.e. use IAVGH for periods

                     VMAX_INFO = VAL_MISSING
                     DO J = NY, 1, -1

                        DO I = 1, NX

                           SOMMA(I) = 0.
                           ICOUNT = 0
                           ITOUT = 0

                           LMISS =.FALSE.
                           DO IT = 1, NT

                 ! RBI 20130101  - set to missing the statistics
                 ! if the model has a non valid value
                 IF (ABS(VALUES_GR(I,J,IT) - VAL_MISSING).LT.EPS) THEN 
                     LMISS = .TRUE.
                 ENDIF

                              ICOUNT = ICOUNT + 1
                             SOMMA(I) = SOMMA(I) + VALUES_GR(I,J,IT)
  
                              IF( ICOUNT .EQ. IAVGH ) THEN
                                  ITOUT = ITOUT + 1
                             VALUES_OUT(I,ITOUT) = SOMMA(I)/FLOAT(IAVGH)
                                  SOMMA(I) = 0.
                                  ICOUNT = 0
                              ENDIF

                           ENDDO

! RBI 20130101 OUT         VMAX = 0.
!                          DO IT = 1, ITOUT
!                             ARR(IT) = VALUES_OUT(I,IT)
!                             IF (ARR(IT) .GT. VMAX) THEN
!                                 VMAX = ARR(IT)
!                             ENDIF
!                          ENDDO
!                          IF (VMAX .GT. 0) THEN
!                              CALL SORT( ARR, ITOUT, WARR )
!                              VALUES(I) = WARR(MVAL)
!                          ELSE
!                              VALUES(I) = 0.
!                          ENDIF

                           IF (LMISS) THEN
                               VALUES(I) = VAL_MISSING
                           ELSE 
                               CALL SORT( ARR, ITOUT, WARR ) !not sure this works because ARR appears to be empty
                               VALUES(I) = WARR(MVAL)
                           ENDIF


                        ENDDO
C                       --- Now encode and write
                        CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                        WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))

                        IF (VMAX .GT. VMAX_INFO) THEN !20130101
                            VMAX_INFO = VMAX
                        ENDIF

                     ENDDO

                    IF (LDEBUG) THEN
                     WRITE(6,*) ' '
                     WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                     WRITE(6,*) ' '
                    ENDIF
                 ENDIF

C ---------------------------------------------------------------------
C ---        AVG and INT
C ---------------------------------------------------------------------

             ELSEIF (STATISTICS(ICODE,UI,IPOST) .EQ. 'AVG' .OR.
     &               STATISTICS(ICODE,UI,IPOST) .EQ. 'INT') THEN

                 IF ( AVERAGING(ICODE,UI,IPOST) .EQ. 'D' .OR.
     &                AVERAGING(ICODE,UI,IPOST) .EQ. 'M' .OR.
     &                AVERAGING(ICODE,UI,IPOST) .EQ. 'Q' ) THEN

                     AVGTYPE = AVERAGING(ICODE,UI,IPOST)

                     CALL FIX24(1,OUTPUT_DATES_24)
                     IF (AVGTYPE .EQ. 'D') THEN
                         IFIRST = OUTPUT_DATES_24(3)
                     ELSEIF (AVGTYPE .EQ. 'M') THEN
                         IFIRST = OUTPUT_DATES_24(2)
                     ELSEIF (AVGTYPE .EQ. 'Q') THEN
                         IFIRST = IQUARTER(OUTPUT_DATES_24(2))
                     ENDIF
                     ITOUT = 0    !RBI 20130101 IN
!!                   ICOUNT = 0    RBI 20130101 OUT
                     ICURR = IFIRST

                     ITEND = 0
!!                   ITLOOP = 0    RBI 20130101 OUT
                     ITLOOP = 1   !RBI 20130101 IN

                     LLAST = .FALSE.

                     VMAX_INFO = VAL_MISSING        ! RBI 20130101
                     DO ITO = 1, NTO
                        LOOP = .TRUE.
                        ICOUNT = 1
                        ITBEG = ITEND+1
                        DO WHILE (LOOP .AND. ITLOOP.LE.NT)    ! SHO 20100809 OUT / RBI 20130101 IN
!                       DO WHILE (LOOP .AND. ITLOOP.LT.NT)    ! SHO 20100809 IN / RBI 20130101 OUT  
                           ITLOOP = ITLOOP + 1
                           CALL FIX24(ITLOOP,OUTPUT_DATES_24)
                           IF (AVGTYPE .EQ. 'D') THEN
                               IVAL = OUTPUT_DATES_24(3)
                           ELSEIF (AVGTYPE .EQ. 'M') THEN
                               IVAL = OUTPUT_DATES_24(2)
                           ELSEIF (AVGTYPE .EQ. 'Q') THEN
                               IVAL = IQUARTER(OUTPUT_DATES_24(2))
                           ENDIF
                           IF (IVAL .EQ. ICURR) THEN
                               ICOUNT = ICOUNT + 1
                           ELSE
!!                             ICOUNT = ICOUNT + 1    RBI 20130101 OUT
                               ITEND = ITLOOP - 1
                               ICURR = IVAL
                               LOOP = .FALSE.
                           ENDIF
                           IF (ITLOOP .EQ. NT) THEN
                               ITEND = NT
                               LOOP = .FALSE.           ! RBI 20130101 IN
                           ENDIF
                        ENDDO
 
                        DO J = NY, 1, -1

                           DO I = 1, NX
                              SOMMA(I) = 0.

                              LMISS = .FALSE.
                              DO ITT = ITBEG, ITEND

                 ! RBI 20130101  - set to missing the statistics
                 ! if the model has a non valid value
                 IF (ABS(VALUES_GR(I,J,ITT) - VAL_MISSING).LT.EPS) THEN 
                     LMISS = .TRUE.
                 ENDIF
                                  SOMMA(I) = SOMMA(I)+VALUES_GR(I,J,ITT)

                              ENDDO

                              ! INT
                              VALUES(I) = SOMMA(I)

                              ! AVG
                              IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'AVG')
     &                        THEN
                                  VALUES(I) = VALUES(I)/FLOAT(ICOUNT)
                              ENDIF
                           
                              IF (LMISS) THEN ! RBI 20130101
                                  VALUES(I) = VAL_MISSING
                              ENDIF

                              IF (VALUES(I) .GT. VMAX_INFO) THEN !20130101
                                  VMAX_INFO = VALUES(I)
                              ENDIF

                           ENDDO

C                           --- Now encode and write
                        CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                           WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))

                        ENDDO
                     ENDDO

                    IF (LDEBUG) THEN
                     WRITE(6,*) ' '
                     WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                     WRITE(6,*) ' '
                    ENDIF
    
              ELSEIF ( AVERAGING(ICODE,UI,IPOST) .EQ. 'MD'  ) THEN  !CHO 20190101

                  AVGTYPE = AVERAGING(ICODE,UI,IPOST)

                  CALL FIX24(1,OUTPUT_DATES_24)
                  IF (AVGTYPE .EQ. 'MD') THEN
                    ICURRMD(1) = (OUTPUT_DATES_24(2) * 100) +
     &              OUTPUT_DATES_24(4)      
                  ENDIF
                     
                  IT = 0
                  NTO = 1
                  ITBEGMD = NT
                  ITENDMD = 0
 
                  DO WHILE (IT .LT. NT)
 
                   IT = IT + 1
      
                   CALL FIX24(IT,OUTPUT_DATES_24)
                   IF (AVGTYPE .EQ. 'MD') THEN
                    IVAL = (OUTPUT_DATES_24(2)* 100) +
     &              OUTPUT_DATES_24(4)
                   ENDIF
       
      
               LMATCHMD = .FALSE.
               DO N = 1, NTO
                    IF (IVAL .EQ. ICURRMD(N)) THEN
             LMATCHMD = .TRUE.
             IF ( IT .LT. ITBEGMD(N) ) ITBEGMD(N) = IT
             IF ( IT .GT. ITENDMD(N) ) ITENDMD(N) = IT
            ENDIF
       
               ENDDO
           
               IF (.NOT. LMATCHMD) THEN
                    NTO = NTO + 1
                    ICURRMD(NTO) = IVAL
            ITBEGMD(NTO) = IT
            ITENDMD(NTO) = IT
                   ENDIF
                      
                  ENDDO
          
c                 DO N = 1, NTO
c           WRITE(*,*) N, ICURRMD(N), ITBEGMD(N), ITENDMD(N)
c          ENDDO 

                     VMAX_INFO = VAL_MISSING        ! RBI 20130101
                     DO ITO = 1, NTO
 
                        DO J = NY, 1, -1

                           DO I = 1, NX
                              SOMMA(I) = 0.

                              LMISS = .FALSE.
                  
                  ICOUNT = 0
                              DO ITT = ITBEGMD(ITO), ITENDMD(ITO), 24

                 ! RBI 20130101  - set to missing the statitics
                 ! if the model has a non valid value
                 IF (ABS(VALUES_GR(I,J,ITT) - VAL_MISSING).LT.EPS) THEN 
                     LMISS = .TRUE.
                 ENDIF
                                  SOMMA(I) = SOMMA(I)+VALUES_GR(I,J,ITT)
                  
                  ICOUNT = ICOUNT + 1

                              ENDDO

                              ! INT
                              VALUES(I) = SOMMA(I)

                              ! AVG
                              IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'AVG')
     &                        THEN
                                  VALUES(I) = VALUES(I)/FLOAT(ICOUNT)
                              ENDIF
                           
                              IF (LMISS) THEN ! RBI 20130101
                                  VALUES(I) = VAL_MISSING
                              ENDIF

                              IF (VALUES(I) .GT. VMAX_INFO) THEN !20130101
                                  VMAX_INFO = VALUES(I)
                              ENDIF

                           ENDDO

C                           --- Now encode and write
                        CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                           WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))
               
                           IF (LDEBUG) THEN
                IF (J. EQ. NY/2) THEN   !CHO 20190101
                 WRITE(6,*) 'AV DIEL MMHH, START STEP , END STEP  ,'//
     &                                 'NUMBER DAYS, SAMPLE VALUE'
                 WRITE(6,*) ICURRMD(ITO), ITBEGMD(ITO), ITENDMD(ITO), 
     &                                 ICOUNT,VALUES(NX/2)
                ENDIF
               ENDIF

                        ENDDO
                     ENDDO

                    IF (LDEBUG) THEN
                     WRITE(6,*) ' '
                     WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                     WRITE(6,*) ' '
                    ENDIF


                 ELSE ! not D, M or Q averaging or MD averaging, i.e. "P" period averaging

                     VMAX_INFO = VAL_MISSING        ! RBI 20130101
                     DO IT = 1, NTOUT

                        DO J = NY, 1, -1

                           DO I = 1, NX

                              SOMMA(I) = 0.
 
                              LMISS = .FALSE.   

                              DO ITT = 1, IAVGH
                                 ITO = (IT-1)*IAVGH + ITT

                 ! RBI 20130101  - set to missing the statitics
                 ! if the model has a non valid value
                 IF (.NOT.LMISS .AND.
     &                ABS(VALUES_GR(I,J,ITT) - VAL_MISSING).LT.EPS) THEN
                     LMISS = .TRUE.
                 ENDIF

                                SOMMA(I) = SOMMA(I) + VALUES_GR(I,J,ITO)
                              ENDDO 


                              ! INT     
                              VALUES(I) = SOMMA(I)

                              SOMMA(I) = 0.

                              ! AVG
                              IF (STATISTICS(ICODE,UI,IPOST) .EQ. 'AVG')
     &                        THEN
                                  VALUES(I) = VALUES(I) / FLOAT(IAVGH)
                              ENDIF

                              IF (LMISS) THEN ! RBI 20130101
                                  VALUES(I) = VAL_MISSING
                              ENDIF

                              IF (VALUES(I) .GT. VMAX_INFO) THEN !20130101
                                  VMAX_INFO = VALUES(I)
                              ENDIF
                           ENDDO 
                            
C                   --- Now encode and write
                         CALL ENCODEG(VALUES,VAL_MISSING,PREC,NX,STRING)
                           WRITE (LOUT,'(A)') STRING(1:LSTRIM(STRING))

                        ENDDO

                     ENDDO

                    IF (LDEBUG) THEN
                     WRITE(6,*) ' '
                     WRITE(6,*) 'Maximum value for this statistics : ',
     &                           VMAX_INFO
                     WRITE(6,*) ' '
                    ENDIF
                 ENDIF

             ELSE !neither MAX/PCT nor AVG/INT

                 WRITE(6,*) 'ERROR'
                 WRITE(6,*) STATISTICS(ICODE,UI,IPOST),
     &                      ' unimplemented.'
             ENDIF

         ENDIF

      ENDDO

      CLOSE (LOUT)

C--------------------------------------------------------------------------------------
C End PROCESSING block
C--------------------------------------------------------------------------------------
