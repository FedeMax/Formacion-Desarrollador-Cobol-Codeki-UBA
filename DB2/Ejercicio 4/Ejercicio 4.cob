***************************** Top of Data ******************************
       IDENTIFICATION DIVISION.                                         
      *                                                        *        
       PROGRAM-ID. PGMD2CBF.                                            
      **********************************************************        
      *                                                        *        
      *  CLASE 47 SINCRONICA - PRACTICA CON DB2                *        
      *                                                        *        
      **********************************************************        
      *      MANTENIMIENTO DE PROGRAMA                         *        
      **********************************************************        
      *  FECHA   *    DETALLE        * COD *                            
      **************************************                            
      *          *                   *     *                            
      *          *                   *     *                            
      **************************************                            
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
             SELECT ENTRADA ASSIGN DDENTRA                              
             FILE STATUS IS FS-ENTRADA.                                 
                                                                        
             SELECT SALIDA ASSIGN DDSALI                                
             FILE STATUS IS FS-SALIDA.                                  
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD ENTRADA                                                       
             BLOCK CONTAINS 0 RECORDS                                   
             RECORDING MODE IS F.                                       
                                                                        
       01 REG-ENTRADA     PIC X(80).                                    
                                                                        
      FD SALIDA                                                        
            BLOCK CONTAINS 0 RECORDS                                   
            RECORDING MODE IS F.                                       
                                                                       
      01 REG-SALIDA      PIC X(80).                                    
                                                                       
     **************************************                            
      WORKING-STORAGE SECTION.                                         
     **************************************                            
      77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
      77  NOT-FOUND     PIC S9(9) COMP VALUE  +100.                    
                                                                       
      77  FS-ENTRADA       PIC XX    VALUE SPACES.                     
      77  FS-SALIDA        PIC XX    VALUE SPACES.                     
                                                                       
      77  WS-TOT-LEI       PIC 9(3)  VALUE ZEROS.                      
      77  WS-TOT-INS       PIC 9(3)  VALUE ZEROS.                      
      77  WS-TOT-MOD       PIC 9(3)  VALUE ZEROS.                      
      77  WS-TOT-ERR       PIC 9(3)  VALUE ZEROS.                      
                                                                       
      01  WS-FLAG-FIN      PIC X.                                      
          88  WS-SI-PROCESO      VALUE ' '.                            
          88  WS-FIN-PROCESO     VALUE 'F'.                            
                                                                       
      01  AUXILIAR         PIC S9(03)V USAGE COMP-3.                   
      01  WS-FLAG-AUX      PIC X.                                      
          88  WS-SI-AUX          VALUE 'S'.                            
          88  WS-NO-AUX          VALUE 'N'.                            
      01  WS-FLAG-AUX2     PIC X.                                      
          88  WS-SI-AUX2         VALUE 'S'.                            
          88  WS-NO-AUX2         VALUE 'N'.                            
      01  WS-NRO-DOC       PIC S9(11)V USAGE COMP-3.                   
      01  WS-NRO-CLI       PIC S9(03)V USAGE COMP-3.                   
                                                                       
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
                                                                        
       01 WS-TITULOS.                                                   
          03  FILLER               PIC X(10)    VALUE SPACES.           
          03  FILLER               PIC X(58)    VALUE                   
           '        ALTAS LEIDAS - DETALLES DE ERRORES                '.
                                                                        
       01 WS-TITULOS2.                                                  
          03  FILLER               PIC X(58)    VALUE                   
           'TIPDOC    NRODOC    NROCLI    APELLIDO Y NOMBRE           '.
          03  FILLER                PIC X(58) VALUE                     
           '  SEXO       FECHA NAC                                    '.
                                                                        
       01 WS-SEPARADOR.                                                 
          03  FILLER               PIC X(58)    VALUE                   
           '----------------------------------------------------------'.
          03  FILLER                PIC X(58) VALUE                     
           '----------------------------------------------------------'.
                                                                        
       01 WS-WRITE.                                                     
          03  FILLER               PIC XX       VALUE SPACES.           
          03  WS-TIPD              PIC XX       VALUE SPACES.           
          03  FILLER               PIC XXXX     VALUE SPACES.           
          03  WS-NDOC              PIC ZZZZZZZ9.                        
          03  FILLER               PIC XXXXX    VALUE SPACES.           
          03  WS-NCLI              PIC ZZ9.                             
          03  FILLER               PIC XXXXXX   VALUE SPACES.           
          03  WS-APELL             PIC X(30)    VALUE SPACES.           
          03  FILLER               PIC XX       VALUE SPACES.           
          03  WS-SEXO              PIC X        VALUE SPACES.           
         03  FILLER               PIC XXXXX    VALUE SPACES.           
         03  WS-FECN              PIC X(10)    VALUE SPACES.           
                                                                       
      77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.  
      77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
                                                                       
           COPY  NOVCLI.                                               
                                                                       
                                                                       
           EXEC SQL                                                    
             INCLUDE SQLCA                                             
           END-EXEC.                                                   
                                                                       
           EXEC SQL                                                    
             INCLUDE TBCURCTA                                          
           END-EXEC.                                                   
                                                                       
           EXEC SQL                                                    
             INCLUDE TBCURCLI                                          
           END-EXEC.                                                   
                                                                       
      77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.  
                                                                       
     ***************************************************************.  
      PROCEDURE DIVISION.                                              
     **************************************                            
     *                                    *                            
     *  CUERPO PRINCIPAL DEL PROGRAMA     *                            
     *                                    *                            
     **************************************                            
      MAIN-PROGRAM.                                                    
                                                                       
          PERFORM 1000-I-INICIO   THRU                                 
                  1000-F-INICIO.                                       
                                                                       
          PERFORM 2000-I-PROCESO  THRU                                 
                  2000-F-PROCESO        UNTIL WS-FIN-PROCESO.          
                                                                        
           PERFORM 9999-I-FINAL    THRU                                 
                   9999-F-FINAL.                                        
                                                                        
           EXEC SQL                                                     
                ROLLBACK                                                
           END-EXEC.                                                    
                                                                        
       F-MAIN-PROGRAM. GOBACK.                                          
                                                                        
      **************************************                            
      *                                    *                            
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            
      *                                    *                            
      **************************************                            
       1000-I-INICIO.                                                   
           SET WS-SI-PROCESO TO TRUE.                                   
                                                                        
           OPEN INPUT  ENTRADA.                                         
                                                                        
                                                                        
           IF FS-ENTRADA  IS NOT EQUAL '00'                             
              DISPLAY '* ERROR EN OPEN ENTRADA  = ' FS-ENTRADA          
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-PROCESO TO TRUE                               
           END-IF                                                       
                                                                        
           OPEN OUTPUT SALIDA.                                          
                                                                        
                                                                        
           IF FS-SALIDA   IS NOT EQUAL '00'                             
              DISPLAY '* ERROR EN OPEN SALIDA   = ' FS-SALIDA           
              MOVE 9999 TO RETURN-CODE                                  
              SET  WS-FIN-PROCESO TO TRUE                               
           END-IF.                                                      
                                                                        
           WRITE REG-SALIDA FROM WS-TITULOS                             
           WRITE REG-SALIDA FROM WS-TITULOS2.                           
           WRITE REG-SALIDA FROM WS-SEPARADOR.                          
                                                                        
                                                                        
       1000-F-INICIO.   EXIT.                                           
      ******************************************************************
       2000-I-PROCESO.                                                  
                                                                        
           PERFORM 2100-LEER THRU F-2100-LEER                           
                                                                        
           EVALUATE NOV-TIP-NOV                                         
               WHEN 'AL'                                                
                 PERFORM 2600-VERIFICADOR THRU F-2600-VERIFICADOR       
                    IF WS-NO-AUX                                        
                    PERFORM 2200-ALTA       THRU F-2200-ALTA            
                    END-IF                                              
               WHEN 'CL'                                                
                    PERFORM 2300-MOD-NROCLI THRU F-2300-MOD-NROCLI      
               WHEN 'CN'                                                
                 PERFORM 2650-VERIFICADOR2 THRU F-2650-VERIFICADOR2     
                    IF WS-SI-AUX2                                       
                    PERFORM 2400-MOD-NOMCLI THRU F-2400-MOD-NOMCLI      
                    END-IF                                              
               WHEN 'CX'                                                
                 PERFORM 2650-VERIFICADOR2 THRU F-2650-VERIFICADOR2     
                    IF WS-SI-AUX2                                       
                    PERFORM 2500-MOD-SEXO   THRU F-2500-MOD-SEXO        
                    END-IF                                              
               WHEN  HIGH-VALUE                                         
                    CONTINUE                                            
               WHEN OTHER                                               
                    ADD 1 TO WS-TOT-ERR                                 
                    WRITE REG-SALIDA FROM WS-WRITE                      
                    INITIALIZE WS-WRITE                                 
           END-EVALUATE.                                                
                                                                        
       2000-F-PROCESO. EXIT.                                            
      ******************************************************************
       2100-LEER.                                                       
                                                                        
           READ  ENTRADA INTO WS-REG-NOVCLI.                            
                                                                        
           EVALUATE FS-ENTRADA                                          
               WHEN '00'                                                
                    ADD 1 TO WS-TOT-LEI                                 
                    MOVE NOV-TIP-DOC      TO WS-TIPD                    
                    MOVE NOV-NRO-DOC      TO WS-NDOC                    
                    MOVE NOV-CLI-NRO      TO WS-NCLI                    
                    MOVE NOV-CLI-APELLIDO TO WS-APELL                   
                    MOVE NOV-CLI-SEXO     TO WS-SEXO                    
                    MOVE NOV-CLI-FENAC    TO WS-FECHA                   
                    MOVE ANIO             TO ANIO2                      
                    MOVE MES              TO MES2                       
                    MOVE DIA              TO DIA2                       
                    MOVE WS-FECHA2        TO WS-FECN                    
                                                                        
               WHEN '10'                                                
                    SET WS-FIN-PROCESO TO TRUE                          
                    MOVE HIGH-VALUE TO NOV-TIP-NOV                      
                                                                        
               WHEN OTHER                                               
                    DISPLAY ' ERROR EN LECTURA ENTRADA =  ' FS-ENTRADA  
                    MOVE 9999 TO RETURN-CODE                            
                    SET WS-FIN-PROCESO TO TRUE                          
                                                                        
           END-EVALUATE.                                                
                                                                        
       F-2100-LEER. EXIT.                                               
      ******************************************************************
       2200-ALTA.                                                       
                                                                        
           MOVE NOV-TIP-DOC      TO WD-TIPDOC                           
           MOVE NOV-NRO-DOC      TO WS-NRO-DOC                          
           MOVE WS-NRO-DOC       TO WD-NRODOC                           
          MOVE NOV-CLI-NRO      TO WS-NRO-CLI                          
          MOVE WS-NRO-CLI       TO WD-NROCLI                           
          MOVE NOV-CLI-APELLIDO TO WD-NOMAPE                           
          MOVE NOV-CLI-SEXO     TO WD-SEXO                             
          MOVE NOV-CLI-FENAC    TO WS-FECHA                            
          MOVE ANIO             TO ANIO2                               
          MOVE MES              TO MES2                                
          MOVE DIA              TO DIA2                                
          MOVE WS-FECHA2        TO WD-FECNAC                           
                                                                       
          EXEC SQL                                                     
               INSERT INTO KC02803.TBCURCLI                            
               (TIPDOC, NRODOC, NROCLI, NOMAPE, FECNAC, SEXO)          
               VALUES ( :WD-TIPDOC,                                    
                        :WD-NRODOC,                                    
                        :WD-NROCLI,                                    
                        :WD-NOMAPE,                                    
                        :WD-FECNAC,                                    
                        :WD-SEXO)                                      
          END-EXEC.                                                    
           IF SQLCODE = NOT-FOUND                                      
               DISPLAY 'ERROR EN ALTA '                                
                   ADD 1 TO WS-TOT-ERR                                 
                   WRITE REG-SALIDA FROM WS-WRITE                      
           ELSE                                                        
                  IF SQLCODE = 0                                       
                      ADD 1 TO WS-TOT-INS                              
                      DISPLAY ' INSERT OK  '                           
                  ELSE                                                 
                         ADD 1 TO WS-TOT-ERR                           
                         DISPLAY 'ERROR DB2 EN ALTA ' SQLCODE          
                 END-IF                                                
           END-IF.                                                     
                                                                       
      F-2200-ALTA. EXIT.                                               
     ******************************************************************
      2300-MOD-NROCLI.                                                 
                                                                       
          MOVE NOV-CLI-NRO      TO WS-NRO-CLI                          
          MOVE WS-NRO-CLI       TO WD-NROCLI                           
          MOVE NOV-NRO-DOC      TO WS-NRO-DOC                          
          MOVE WS-NRO-DOC       TO WD-NRODOC                           
                                                                       
                                                                       
          EXEC SQL                                                     
               UPDATE KC02803.TBCURCLI                                 
               SET    NROCLI = :WD-NROCLI                              
               WHERE  NRODOC = :WD-NRODOC                              
          END-EXEC                                                     
                                                                       
           IF SQLCODE = NOT-FOUND                                      
               DISPLAY 'ERROR EN MODIFICACION NRO CLI '                
                   ADD 1 TO WS-TOT-ERR                                 
                   WRITE REG-SALIDA FROM WS-WRITE                      
           ELSE                                                        
                  IF SQLCODE = 0                                       
                      ADD 1 TO WS-TOT-MOD                              
                      DISPLAY ' MOD NROCLI OK '                        
                  ELSE                                                 
                         ADD 1 TO WS-TOT-ERR                           
                         DISPLAY 'ERROR DB2 EN MOD-NROCLI ' SQLCODE    
                 END-IF                                                
           END-IF.                                                     
                                                                       
      F-2300-MOD-NROCLI. EXIT.                                         
     ******************************************************************
      2400-MOD-NOMCLI.                                                 
                                                                       
          MOVE NOV-CLI-APELLIDO TO WD-NOMAPE                           
          MOVE NOV-CLI-NRO      TO WS-NRO-CLI                          
          MOVE WS-NRO-CLI       TO WD-NROCLI                           
                                                                       
          EXEC SQL                                                     
               UPDATE KC02803.TBCURCLI                                 
                  SET NOMAPE = :WD-NOMAPE                              
                WHERE NROCLI = :WD-NROCLI                              
          END-EXEC.                                                    
                                                                       
           IF SQLCODE = NOT-FOUND                                      
               DISPLAY 'ERROR EN MODIFICACION NOM CLI '                
                   ADD 1 TO WS-TOT-ERR                                 
                   WRITE REG-SALIDA FROM WS-WRITE                      
           ELSE                                                        
                  IF SQLCODE = 0                                       
                      ADD 1 TO WS-TOT-MOD                              
                      DISPLAY ' MOD NOMCLI OK '                        
                  ELSE                                                 
                         ADD 1 TO WS-TOT-ERR                           
                         DISPLAY 'ERROR DB2 EN MOD-NOMCLI '            
                 END-IF                                                
           END-IF.                                                     
                                                                       
      F-2400-MOD-NOMCLI. EXIT.                                         
     ******************************************************************
      2500-MOD-SEXO.                                                   
                                                                       
          MOVE NOV-CLI-SEXO     TO WD-SEXO                             
          MOVE NOV-CLI-NRO      TO WS-NRO-CLI                          
          MOVE WS-NRO-CLI       TO WD-NROCLI                           
                                                                       
          EXEC SQL                                                     
               UPDATE KC02803.TBCURCLI                                 
                  SET SEXO   = :WD-SEXO                                
                WHERE NROCLI = :WD-NROCLI                              
          END-EXEC.                                                    
                                                                       
           IF SQLCODE = NOT-FOUND                                      
               DISPLAY 'ERROR EN MODIFICACION SEXO  '                  
                   ADD 1 TO WS-TOT-ERR                                 
                   WRITE REG-SALIDA FROM WS-WRITE                      
           ELSE                                                        
                  IF SQLCODE = 0                                       
                      ADD 1 TO WS-TOT-MOD                              
                      DISPLAY ' MOD SEXO   OK '                        
                  ELSE                                                 
                         ADD 1 TO WS-TOT-ERR                           
                         DISPLAY 'ERROR DB2 EN MOD-SEXO ' SQLCODE      
                 END-IF                                                
           END-IF.                                                     
                                                                       
      F-2500-MOD-SEXO. EXIT.                                           
     ******************************************************************
      2600-VERIFICADOR.                                                
                                                                       
          MOVE NOV-CLI-NRO TO WD-NROCLI                                
                                                                       
          EXEC SQL                                                     
               SELECT NROCLI                                           
                INTO  :AUXILIAR                                        
                FROM  KC02803.TBCURCLI                                 
               WHERE  NROCLI = :WD-NROCLI                              
          END-EXEC.                                                    
                                                                       
                                                                       
           IF SQLCODE = NOT-FOUND                                      
               SET WS-NO-AUX TO TRUE                                   
           ELSE                                                        
                  IF SQLCODE = 0                                       
                      DISPLAY ' EL CLIENTE YA SE ENCUENTRA EN LA BASE '
                    ' DE DATOS '                                       
                      ADD 1 TO WS-TOT-ERR                              
                      WRITE REG-SALIDA FROM WS-WRITE                   
                  ELSE                                                 
                         ADD 1 TO WS-TOT-ERR                           
                         DISPLAY 'ERROR DB2  EN VERIFICADOR '  SQLCODE 
                 END-IF.                                               
                                                                       
      F-2600-VERIFICADOR. EXIT.                                        
       ******************************************************************
        2650-VERIFICADOR2.                                               
                                                                         
            MOVE NOV-CLI-NRO TO WD-NROCLI                                
                                                                         
            EXEC SQL                                                     
                 SELECT NROCLI                                           
                  INTO  :AUXILIAR                                        
                  FROM  KC02803.TBCURCLI                                 
                 WHERE  NROCLI = :WD-NROCLI                              
            END-EXEC.                                                    
                                                                         
                                                                         
             IF SQLCODE = NOT-FOUND                                      
                        ADD 1 TO WS-TOT-ERR                              
                        WRITE REG-SALIDA FROM WS-WRITE                   
                        DISPLAY ' NO SE ENCUENTRA EN LA BDD '            
             ELSE                                                        
                    IF SQLCODE = 0                                       
                       SET WS-SI-AUX2 TO TRUE                            
                    ELSE                                                 
                           ADD 1 TO WS-TOT-ERR                           
                           DISPLAY 'ERROR DB2 EN VERIFICADOR2'  SQLCODE  
                   END-IF.                                               
                                                                         
        F-2650-VERIFICADOR2. EXIT.                                       
       ******************************************************************
        9999-I-FINAL.                                                    
              CLOSE ENTRADA.                                             
                                                                         
                  IF FS-ENTRADA  IS NOT EQUAL '00'                       
                   DISPLAY '* ERROR EN CLOSE ENTRADA  = '                
                                             FS-ENTRADA                  
                   MOVE 9999 TO RETURN-CODE                              
                  END-IF                                                 
              CLOSE SALIDA.                                              
                                                                         
                 IF FS-SALIDA   IS NOT EQUAL '00'                       
                  DISPLAY '* ERROR EN CLOSE SALIDA   = '                
                                            FS-SALIDA                   
                  MOVE 9999 TO RETURN-CODE                              
                 END-IF                                                 
            .                                                           
       9999-F-FINAL.                                                    
           EXIT.                                                        
**************************** Bottom of Data ****************************
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        