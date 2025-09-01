***************************** Top of Data ****************************
       IDENTIFICATION DIVISION.                                       
       PROGRAM-ID. CLASE29.                                           
       AUTHOR.    FEDERICO FALCON.                                    
      **************************************************************  
      *CLASE 29 SINCRONICA - PRACTICA CON DB2/ACT                  *  
      **************************************************************  
       ENVIRONMENT DIVISION.                                          
      **************************************************************  
       CONFIGURATION SECTION.                                         
                                                                      
      **************************************************************  
       INPUT-OUTPUT SECTION.                                          
       FILE-CONTROL.                                                  
                                                                      
           SELECT ENTRADA ASSIGN TO DDENTRA                           
                 FILE STATUS IS FS-ENTRADA.                           
      **************************************************************  
       I-O-CONTROL.                                                   
                                                                      
       DATA DIVISION.                                                 
       FILE SECTION.                                                  
       FD   ENTRADA                                                   
            BLOCK CONTAINS 0 RECORDS                                  
            RECORDING MODE IS F.                                      
       01   WS-ENTRADA                PIC X(80).                      
      **************************************************************  
       WORKING-STORAGE SECTION.                                       
      **************************************************************  
            COPY NOVECLIE.                                            
                                                                      
            EXEC SQL  INCLUDE SQLCA END-EXEC.                         
                                                                      
            EXEC SQL                                                  
                 INCLUDE TBCURCLI                                     
            END-EXEC.                                                 
     **************************************************************  
      77  FS-ENTRADA                 PIC XX      VALUE SPACES.       
      77  NOT-FOUND     PIC S9(9) COMP VALUE  +100.                  
      77  SQLCODE-EDIT  PIC ++++++9999 VALUE ZEROS.                  
      77  FILLER        PIC X(26) VALUE '* DESCRIPCION SQLCA      *'.
     **************************************************************  
                                                                     
      77  WS-STATUS                  PIC X.                          
          88  WS-FIN                 VALUE 'Y'.                      
          88  WS-NO-FIN              VALUE 'N'.                      
                                                                     
     ***************************VARIABLES**************************  
      77  WS-NOV-LEI                 PIC 9(05) VALUE ZEROES.         
      77  WS-NOV-INS                 PIC 9(05) VALUE ZEROES.         
      77  WS-NOV-ERR                 PIC 9(05) VALUE ZEROES.         
      77  WS-NOV-MOD                 PIC 9(05) VALUE ZEROES.         
                                                                     
      77  WS-CONT                    PIC 9(03) VALUE ZEROES.         
      77  WS-DATO                    PIC X(02) VALUE SPACES.         
                                                                     
      77  WS-TIP-DOC                 PIC X(02) VALUE SPACES.         
      77  WS-NRO-DOC           PIC S9(11)V USAGE COMP-3.             
      01  WS-NRO-CLI           PIC S9(03)V USAGE COMP-3.             
      01  WS-NRO-CLI-AUX       PIC S9(03)V USAGE COMP-3.             
                                                                     
      01 WS-FECHA.                                                   
         03 ANIO          PIC X(04) VALUE SPACES.                    
         03 MES           PIC X(02) VALUE SPACES.                    
         03 DIA           PIC X(02) VALUE SPACES.                    
                                                                     
      01 WS-FECHA2.                                                  
         03 ANIO2         PIC X(04) VALUE SPACES.                    
         03 FILLER        PIC X VALUE '-'  .                         
         03 MES2          PIC X(02) VALUE SPACES.                    
         03 FILLER        PIC X VALUE '-'  .                         
         03 DIA2          PIC X(02) VALUE SPACES.                    
                                                                     
      01 WS-APELL         PIC X(06) VALUE SPACES.                    
     **************************************************************  
      PROCEDURE DIVISION.                                            
     **************************************                          
     *                                    *                          
     *  CUERPO PRINCIPAL DEL PROGRAMA     *                          
     *                                    *                          
     **************************************                          
      MAIN-PROGRAM.                                                  
                                                                     
          PERFORM 1000-INICIO  THRU   F-1000-INICIO.                 
                                                                     
          PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                 
                  UNTIL WS-FIN.                                      
                                                                     
          PERFORM 9999-FINAL    THRU  F-9999-FINAL.                  
                                                                     
      F-MAIN-PROGRAM. GOBACK.                                        
                                                                     
     **************************************                          
     *                                    *                          
     *  CUERPO INICIO APERTURA ARCHIVOS   *                          
     *                                    *                          
     **************************************                          
      1000-INICIO.                                                   
          SET WS-NO-FIN TO TRUE.                                     
                                                                     
          OPEN INPUT  ENTRADA.                                       
                                                                     
                                                                     
          IF FS-ENTRADA  IS NOT EQUAL '00'                           
             DISPLAY '* ERROR EN OPEN ENTRADA  = ' FS-ENTRADA        
             MOVE 9999 TO RETURN-CODE                                
             SET  WS-FIN     TO TRUE                                 
          END-IF.                                                    
                                                                    
     F-1000-INICIO.   EXIT.                                         
    **************************************************************  
     2000-PROCESO.                                                  
                                                                    
         PERFORM 2050-INICIALIZADOR THRU                            
                 F-2050-INICIALIZADOR.                              
                                                                    
         PERFORM 2100-LEER           THRU                           
               F-2100-LEER.                                         
                                                                    
         IF FS-ENTRADA   IS  EQUAL '10'                             
            SET WS-FIN TO TRUE                                      
                                                                    
         ELSE                                                       
              PERFORM 2200-ARMAR-TABLA    THRU                      
                    F-2200-ARMAR-TABLA                              
                                                                    
         END-IF.                                                    
                                                                    
                                                                    
     F-2000-PROCESO. EXIT.                                          
                                                                    
    **************************************************************  
     2050-INICIALIZADOR.                                            
                                                                    
         initialize DCLTBCURCLI.                                    
                                                                    
     F-2050-INICIALIZADOR. EXIT.                                    
    **************************************************************  
     2100-LEER.                                                     
                                                                    
         READ  ENTRADA INTO WS-REG-NOVECLI.                         
                                                                    
         EVALUATE FS-ENTRADA                                        
             WHEN '00'                                              
                    ADD 1 TO WS-NOV-LEI                               
                                                                      
               WHEN '10'                                              
                    SET WS-FIN TO TRUE                                
                                                                      
               WHEN OTHER                                             
                    DISPLAY ' ERROR EN LECTURA NOVEDAD =  ' FS-ENTRADA
                    MOVE 9999 TO RETURN-CODE                          
                    SET WS-FIN TO TRUE                                
                                                                      
           END-EVALUATE.                                              
                                                                      
       F-2100-LEER. EXIT.                                             
                                                                      
      **************************************************************  
       2200-ARMAR-TABLA.                                              
                                                                      
           MOVE NOV-TIP-NOV TO WS-DATO                                
                                                                      
           EVALUATE WS-DATO                                           
                                                                      
               WHEN 'al'                                              
                    PERFORM 3050-VERIFICADOR THRU F-3050-VERIFICADOR  
                    PERFORM 3000-ALTA THRU F-3000-ALTA                
               WHEN 'cl'                                              
                    PERFORM 3100-MOD-NRO-CLI THRU F-3100-MOD-NRO-CLI  
               WHEN 'cn'                                              
                    PERFORM 3200-MOD-NOMB-CLI THRU F-3200-MOD-NOMB-CLI
               WHEN 'cx'                                              
                    PERFORM 3300-MOD-SEX THRU F-3300-MOD-SEX          
               WHEN OTHER                                             
                    CONTINUE                                          
           END-EVALUATE.                                              
                                                                      
                                                                      
       F-2200-ARMAR-TABLA. EXIT.                                      
    **************************************************************  
     3000-ALTA.                                                     
                                                                    
         MOVE NOV-TIP-DOC    TO  WD-TIPDOC                          
         MOVE NOV-NRO-DOC    TO  WS-NRO-DOC                         
         MOVE WS-NRO-DOC     TO  WD-NRODOC                          
         MOVE NOV-CLI-NRO    TO  WS-NRO-CLI                         
         MOVE WS-NRO-CLI     TO  wd-NROCLI                          
         move NOV-CLI-NOMBRE TO  wd-nomape                          
                                                                    
         MOVE NOV-CLI-FENAC  TO  ws-fecha                           
         MOVE  ANIO          TO  ANIO2                              
         MOVE  MES           TO  MES2                               
         MOVE  DIA           TO  DIA2                               
         MOVE WS-FECHA2      TO  WD-FECNAC                          
                                                                    
         move NOV-CLI-SEXO   to  wd-sexo                            
                                                                    
          IF SQLCODE = NOT-FOUND                                    
           EXEC SQL                                                 
                INSERT INTO KC02787.TBCURCLI                        
                (TIPDOC, NRODOC, NROCLI, NOMAPE, FECNAC, SEXO)      
                VALUES ( :WD-TIPDOC,                                
                         :WD-NRODOC,                                
                         :wd-nrocli,                                
                         :WD-NOMAPE,                                
                         :WD-FECNAC,                                
                         :WD-SEXO)                                  
           END-EXEC                                                 
                                                                    
          IF SQLCODE = NOT-FOUND                                    
              MOVE SQLCODE TO SQLCODE-EDIT                          
              DISPLAY 'ERROR EN ALTA '                              
          ELSE                                                      
                 IF SQLCODE = 0                                     
             add 1 to ws-NOV-INS                                    
                      DISPLAY 'INSERT OK  ' WD-TIPDOC ' ' WD-NRODOC  
                  ELSE                                               
                         MOVE SQLCODE TO       SQLCODE-EDIT          
                         ADD 1 TO WS-NOV-ERR                         
                         DISPLAY 'ERROR DB2 '  SQLCODE-EDIT          
                 END-IF                                              
           END-IF.                                                   
                                                                     
      F-3000-ALTA. EXIT.                                             
     **************************************************************  
      3050-VERIFICADOR.                                              
                                                                     
           MOVE 0 TO WS-CONT                                         
           MOVE NOV-TIP-DOC    TO  WD-TIPDOC                         
           MOVE NOV-NRO-DOC    TO  WS-NRO-DOC                        
           MOVE WS-NRO-DOC     TO  WD-NRODOC                         
                                                                     
           EXEC  SQL                                                 
                SELECT NROCLI                                        
                INTO :WS-NRO-CLI-AUX                                 
                FROM KC02787.TBCURCLI                                
                WHERE TIPDOC = :WD-TIPDOC                            
                 AND  NRODOC = :WD-NRODOC                            
           END-EXEC.                                                 
                                                                     
           IF SQLCODE = NOT-FOUND                                    
               MOVE SQLCODE TO SQLCODE-EDIT                          
               DISPLAY 'NO SE ENCUENTRA EN LA BASE DE DATOS '        
           ELSE                                                      
                  IF SQLCODE = 0                                     
                      DISPLAY 'INSERT OK  ' WD-TIPDOC ' ' WD-NRODOC  
                  ELSE                                               
                         MOVE SQLCODE TO       SQLCODE-EDIT          
                         ADD 1 TO WS-NOV-ERR                         
                         DISPLAY 'ERROR DB2 '  SQLCODE-EDIT          
                 END-IF.                                             
                                                                    
     F-3050-VERIFICADOR. EXIT.                                      
    **************************************************************  
     3100-MOD-NRO-CLI.                                              
                                                                    
         MOVE NOV-CLI-NRO    TO  WS-NRO-CLI                         
         MOVE WS-NRO-CLI     TO  wd-NROCLI                          
         MOVE NOV-NRO-DOC    TO  WS-NRO-DOC                         
         MOVE WS-NRO-DOC     TO  WD-NRODOC                          
                                                                    
         EXEC SQL                                                   
                                                                    
              UPDATE KC02787.TBCURCLI                               
              SET    NROCLI = :WD-NROCLI                            
              WHERE  NRODOC = :WD-NRODOC                            
         END-EXEC                                                   
                                                                    
          IF SQLCODE = NOT-FOUND                                    
              MOVE SQLCODE TO SQLCODE-EDIT                          
              DISPLAY 'NO SE ENCUENTRA EN LA BASE DE DATOS '        
          ELSE                                                      
                 IF SQLCODE = 0                                     
                     DISPLAY 'UPDATE OK  ' WD-TIPDOC ' ' WD-NRODOC  
                     ADD 1 TO WS-NOV-MOD                            
                 ELSE                                               
                        MOVE SQLCODE TO       SQLCODE-EDIT          
                        ADD 1 TO WS-NOV-ERR                         
                        DISPLAY 'ERROR DB2 '  SQLCODE-EDIT          
                END-IF.                                             
                                                                    
     F-3100-MOD-NRO-CLI. EXIT.                                      
    **************************************************************  
     3200-MOD-NOMB-CLI.                                             
                                                                    
         MOVE NOV-CLI-NOMBRE TO  WD-NOMAPE                          
         MOVE NOV-CLI-NOMBRE TO  WS-APELL                           
         MOVE NOV-CLI-NRO    TO  WS-NRO-CLI                         
         MOVE WS-NRO-CLI     TO  wd-NROCLI                          
         MOVE NOV-NRO-DOC    TO  WS-NRO-DOC                         
         MOVE WS-NRO-DOC     TO  WD-NRODOC                          
          display ' apellido ' ws-apell                             
          IF WS-APELL IS EQUAL  TO 'falcon'                         
          EXEC SQL                                                  
                                                                    
               UPDATE KC02787.TBCURCLI                              
               SET    NOMAPE = :WD-NOMAPE                           
               WHERE  NRODOC = :WD-NRODOC                           
                 AND  NROCLI = :WD-NROCLI                           
          END-EXEC                                                  
          END-IF                                                    
                                                                    
          IF SQLCODE = NOT-FOUND                                    
              MOVE SQLCODE TO SQLCODE-EDIT                          
              DISPLAY 'NO SE ENCUENTRA EN LA BASE DE DATOS '        
          ELSE                                                      
                 IF SQLCODE = 0                                     
                     DISPLAY 'UPDATE OK  ' WD-TIPDOC ' ' WD-NRODOC  
                     ADD 1 TO WS-NOV-MOD                            
                 ELSE                                               
                        MOVE SQLCODE TO       SQLCODE-EDIT          
                        ADD 1 TO WS-NOV-ERR                         
                        DISPLAY 'ERROR DB2 '  SQLCODE-EDIT          
                END-IF.                                             
                                                                    
     F-3200-MOD-NOMB-CLI. EXIT.                                     
    **************************************************************  
     3300-MOD-SEX.                                                  
                                                                    
         MOVE NOV-CLI-SEXO   TO  WD-SEXO                            
         MOVE NOV-CLI-NRO    TO  WS-NRO-CLI                         
         MOVE WS-NRO-CLI     TO  wd-NROCLI                          
         MOVE NOV-NRO-DOC    TO  WS-NRO-DOC                         
                                                                 
      EXEC SQL                                                   
                                                                 
           UPDATE KC02787.TBCURCLI                               
           SET     SEXO  = :WD-SEXO                              
           WHERE  NRODOC = :WD-NRODOC                            
             AND  NROCLI = :WD-NROCLI                            
      END-EXEC                                                   
                                                                 
       IF SQLCODE = NOT-FOUND                                    
           MOVE SQLCODE TO SQLCODE-EDIT                          
           DISPLAY 'NO SE ENCUENTRA EN LA BASE DE DATOS '        
       ELSE                                                      
              IF SQLCODE = 0                                     
                  DISPLAY 'UPDATE OK  ' WD-TIPDOC ' ' WD-NRODOC  
                  ADD 1 TO WS-NOV-MOD                            
              ELSE                                               
                     MOVE SQLCODE TO       SQLCODE-EDIT          
                     ADD 1 TO WS-NOV-ERR                         
                     DISPLAY 'ERROR DB2 '  SQLCODE-EDIT          
             END-IF.                                             
                                                                 
  F-3300-MOD-SEX. EXIT.                                          
 **************************************************************  
                                                                 
  9999-FINAL.                                                    
                                                                 
        CLOSE ENTRADA.                                           
                                                                 
            IF FS-ENTRADA  IS NOT EQUAL '00'                     
             DISPLAY '* ERROR EN CLOSE NOVEDAD  = '              
                                       FS-ENTRADA                
             MOVE 9999 TO RETURN-CODE                            
            END-IF                                               
      DISPLAY '--------------------------------------------------
      DISPLAY 'CANTIDAD DE REGISTROS LEIDOS      = ' WS-NOV-LEI  
           DISPLAY 'CANTIDAD DE REGISTROS INSERTADOS  = ' wS-NOV-INS  
           DISPLAY 'CANTIDAD DE REGISTROS ERRONEOS    = ' WS-NOV-ERR  
           DISPLAY 'CANTIDAD DE REGISTROS MODIFICADOS = ' WS-NOV-MOD  
           DISPLAY '--------------------------------------------------
                                                                      
                                                                      
                                                                      
                                                                      
            .                                                         
       F-9999-FINAL.                                                  
           EXIT.                                                      
**************************** Bottom of Data **************************
                                                                      
                                                                      
                                                                      
                                                                      
                                                                      
                                                                      