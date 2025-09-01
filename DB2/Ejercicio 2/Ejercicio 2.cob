**************************** Top of Data ****************************
      IDENTIFICATION DIVISION.                                       
     *                                                        *      
      PROGRAM-ID. CLASE28.                                           
     **********************************************************      
     *                                                        *      
     *  PROGRAMA PARA SQL EMBEBIDO                            *      
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
                                                                     
            SELECT SALIDA  ASSIGN DDSALE                             
            FILE STATUS IS WS-CODE-SAL.                              
                                                                     
      DATA DIVISION.                                                 
      FILE SECTION.                                                  
      FD SALIDA                                                      
            BLOCK CONTAINS 0 RECORDS                                 
            RECORDING MODE IS F.                                     
                                                                     
      01 REG-SALIDA      PIC X(112).                                 
                                                                     
     **************************************                          
      WORKING-STORAGE SECTION.                                       
     **************************************                          
      77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
                                                                     
      77  WS-CODE-SAL      PIC XX    VALUE SPACES.                   
      77  WS-TOT-GRABADOS  PIC 9(3)  VALUE ZEROS.                    
      01  WS-FLAG-FIN      PIC X.                                    
          88  WS-SI-PROCESO      VALUE ' '.                          
          88  WS-FIN-PROCESO     VALUE 'F'.                          
                                                                     
      01 WS-TITULO.                                                  
         03  FILLER               PIC X(05)    VALUE SPACES.         
         03  FILLER               PIC X(53)    VALUE                 
          '        LISTADO DE CLIENTES                    FECHA '.   
         03  RESTO                PIC X(10)    VALUE SPACES.         
                                                                     
      01 WS-SUBTITULOS.                                              
         03  FILLER               PIC X(10)    VALUE SPACES.         
         03  FILLER               PIC X(58)    VALUE                 
          'TCUEN  NCUEN  SUC.CUEN  NCLIEN  NOMYAPE                   
         03  FILLER               PIC X(58)    VALUE                 
          '                  SALDO    FECHA SALIDA                  '
                                                                     
     **************************************                          
      01 WS-FECHA.                                                   
         03 ANIO          PIC X(04) VALUE SPACES.                    
         03 MES           PIC X(02) VALUE SPACES.                    
         03 DIA           PIC X(02) VALUE SPACES.                    
                                                                     
      01 WS-FECHA2.                                                  
         03 DIA2          PIC X(02) VALUE SPACES.                    
         03 FILLER        PIC X VALUE '-'  .                         
         03 MES2          PIC X(02) VALUE SPACES.                    
         03 FILLER        PIC X VALUE '-'  .                         
         03 ANIO2         PIC X(04) VALUE SPACES.                    
                                                                     
     *****************************************                       
     *         LAYOUT TABLA CUENTAS          *                       
      *                                       *                       
      *****************************************                       
       01 WS-REG-SALIDA.                                              
                                                                      
           03  FILLER              PIC X(10)    VALUE SPACES.         
           03  REG-TIPCUEN         PIC 9(02)     VALUE ZEROS.         
           03  FILLER              PIC X(05)    VALUE SPACES.         
           03  REG-NROCUEN         PIC 9(05)     VALUE ZEROS.         
           03  FILLER              PIC X(05)     VALUE SPACES.        
           03  REG-SUCUEN          PIC 9(02)     VALUE ZEROS.         
           03  FILLER              PIC X(05)     VALUE SPACES.        
           03  REG-NROCLI          PIC 9(03)     VALUE ZEROS.         
           03  FILLER              PIC X(05)     VALUE SPACES.        
           03  REG-NOMAPE          PIC X(30)     VALUE SPACES.        
           03  FILLER              PIC X(05)     VALUE SPACES.        
           03  REG-SALDO           PIC -Z(09).99   VALUE ZEROS.       
           03  FILLER              PIC X(05)     VALUE SPACES.        
           03  REG-FECSAL          PIC X(10)     VALUE SPACES.        
           03  FILLER              PIC X(07)     VALUE SPACES.        
                                                                      
       77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.        
                                                                      
            EXEC SQL                                                  
              INCLUDE SQLCA                                           
            END-EXEC.                                                 
                                                                      
            EXEC SQL                                                  
              INCLUDE TBCURCTA                                        
            END-EXEC.                                                 
                                                                      
            EXEC SQL                                                  
              INCLUDE TBCURCLI                                        
            END-EXEC.                                                 
                                                                      
            EXEC SQL                                                  
              DECLARE ITEM_CURSOR CURSOR                              
             FOR                                                     
            SELECT A.TIPCUEN,                                        
                   A.NROCUEN,                                        
                   A.SUCUEN,                                         
                   A.NROCLI,                                         
                   B.NOMAPE,                                         
                   A.SALDO,                                          
                   A.FECSAL                                          
               FROM  KC02787.TBCURCTA A                              
                    INNER JOIN                                       
                      KC02787.TBCURCLI B                             
                   ON  A.NROCLI = B.NROCLI                           
                   WHERE A.SALDO > 0                                 
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
                                                                     
      F-MAIN-PROGRAM. GOBACK.                                        
                                                                     
      **************************************                          
      *                                    *                          
      *  CUERPO INICIO APERTURA ARCHIVOS   *                          
      *                                    *                          
      **************************************                          
       1000-I-INICIO.                                                 
           SET WS-SI-PROCESO TO TRUE.                                 
                                                                      
           OPEN OUTPUT SALIDA.                                        
           IF WS-CODE-SAL    IS NOT EQUAL '00'                        
              DISPLAY '* ERROR EN OPEN SALIDA  = ' WS-CODE-SAL        
              MOVE 9999 TO RETURN-CODE                                
              SET  WS-FIN-PROCESO TO TRUE                             
           END-IF.                                                    
                                                                      
           IF WS-CODE-SAL    IS NOT EQUAL '00'                        
              DISPLAY '* ERROR EN WRITE SALIDA  = ' WS-CODE-SAL       
              MOVE 9999 TO RETURN-CODE                                
              SET  WS-FIN-PROCESO TO TRUE                             
           END-IF.                                                    
                                                                      
           EXEC SQL                                                   
              OPEN ITEM_CURSOR                                        
           END-EXEC.                                                  
                                                                      
           IF SQLCODE NOT EQUAL ZEROS                                 
              MOVE SQLCODE   TO WS-SQLCODE                            
              DISPLAY '* ERROR OPEN CURSOR      = ' WS-SQLCODE        
              MOVE 9999 TO RETURN-CODE                                
              SET  WS-FIN-PROCESO TO TRUE                             
           END-IF.                                                    
                                                                      
           ACCEPT WS-FECHA FROM DATE YYYYMMDD.                        
                                                                      
           MOVE ANIO TO ANIO2                                         
           MOVE MES  TO MES2                                          
           MOVE DIA  TO DIA2                                          
                                                                     
          MOVE WS-FECHA2 TO  RESTO.                                  
                                                                     
          WRITE REG-SALIDA     FROM WS-TITULO.                       
          WRITE REG-SALIDA     FROM WS-SUBTITULOS.                   
                                                                     
      1000-F-INICIO.   EXIT.                                         
                                                                     
     **************************************                          
     *                                    *                          
     *  CUERPO PRINCIPAL DEL PROGRAMA     *                          
     *                                    *                          
     **************************************                          
      2000-I-PROCESO.                                                
                                                                     
          EXEC SQL                                                   
             FETCH  ITEM_CURSOR                                      
                    INTO                                             
                       :WS-TIPCUEN,                                  
                       :WS-NROCUEN,                                  
                       :WS-SUCUEN,                                   
                       :WS-NROCLI,                                   
                       :WD-NOMAPE,                                   
                       :WS-SALDO,                                    
                       :WS-FECSAL                                    
          END-EXEC.                                                  
                                                                     
                                                                     
          EVALUATE TRUE                                              
            WHEN SQLCODE EQUAL ZEROS                                 
               DISPLAY 'ACCESO DB2 OK '                              
               MOVE WS-SALDO   TO REG-SALDO                          
               MOVE WS-TIPCUEN TO REG-TIPCUEN                        
               MOVE WS-NROCUEN TO REG-NROCUEN                        
               MOVE WS-SUCUEN  TO REG-SUCUEN                         
               MOVE WS-NROCLI  TO REG-NROCLI                         
               MOVE WS-FECSAL  TO REG-FECSAL                         
                MOVE WD-NOMAPE  TO REG-NOMAPE                         
                PERFORM 3000-I-GRABAR          THRU                   
                        3000-F-GRABAR                                 
                                                                      
             WHEN SQLCODE EQUAL +100                                  
                SET WS-FIN-PROCESO TO TRUE                            
                                                                      
             WHEN OTHER                                               
                MOVE SQLCODE TO WS-SQLCODE                            
                DISPLAY 'ERROR FETCH CURSOR: '   WS-SQLCODE           
                SET WS-FIN-PROCESO TO TRUE                            
           END-EVALUATE.                                              
                                                                      
       2000-F-PROCESO. EXIT.                                          
                                                                      
      **************************************                          
      *                                    *                          
      *  GRABAR SALIDA                     *                          
      *                                    *                          
      **************************************                          
       3000-I-GRABAR.                                                 
                                                                      
           WRITE REG-SALIDA     FROM WS-REG-SALIDA.                   
           ADD  1      TO  WS-TOT-GRABADOS.                           
                                                                      
       3000-F-GRABAR.   EXIT.                                         
                                                                      
                                                                      
      **************************************                          
      *                                    *                          
      *  CUERPO FINAL CIERRE DE FILES      *                          
      *                                    *                          
      **************************************                          
       9999-I-FINAL.                                                  
           EXEC SQL                                                   
              CLOSE ITEM_CURSOR                                       
           END-EXEC.                                                  
                                                                     
          IF SQLCODE NOT EQUAL ZEROS                                 
             MOVE SQLCODE TO WS-SQLCODE                              
             DISPLAY '* ERROR CLOSE CURSOR      = ' WS-SQLCODE       
             MOVE 9999 TO RETURN-CODE                                
          END-IF.                                                    
                                                                     
                                                                     
          CLOSE SALIDA                                               
             IF WS-CODE-SAL  IS NOT EQUAL '00'                       
               DISPLAY '* ERROR EN CLOSE SALIDA  = '                 
                                           WS-CODE-SAL               
               MOVE 9999 TO RETURN-CODE                              
            END-IF.                                                  
                                                                     
     **************************************                          
     *   MOSTRAR TOTALES DE CONTROL                                  
     **************************************                          
                                                                     
          DISPLAY 'TOTAL GRABADOS: '    WS-TOT-GRABADOS.             
                                                                     
      9999-F-FINAL.                                                  
          EXIT.                                                      
     *                                                               
*************************** Bottom of Data **************************
                                                                     