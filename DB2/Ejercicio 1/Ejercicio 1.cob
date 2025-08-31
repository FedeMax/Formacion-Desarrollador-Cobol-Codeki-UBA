***************************** Top of Data ****************************
       IDENTIFICATION DIVISION.                                       
        PROGRAM-ID PGMB2CBF.                                          
      **************************************                          
      *                                    *                          
      *                                    *                          
      **************************************                          
       ENVIRONMENT DIVISION.                                          
       INPUT-OUTPUT SECTION.                                          
       FILE-CONTROL.                                                  
             SELECT NOVEDAD ASSIGN DDNOVED                            
              ORGANIZATION IS INDEXED                                 
              ACCESS MODE IS SEQUENTIAL                               
              RECORD KEY IS WS-KEY                                    
                    FILE STATUS IS WS-NOV-CODE.                       
       DATA DIVISION.                                                 
       FILE SECTION.                                                  
       FD NOVEDAD                                                     
       01 WS-NOVEDAD  .                                               
          03   WS-KEY PIC X(17).                                      
          03   FILLER PIC X(227).                                     
                                                                      
                                                                      
       WORKING-STORAGE SECTION.                                       
                                                                      
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
       77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'.
       77  NOT-FOUND     PIC S9(9) COMP VALUE  +100.                  
       77  WS-SQLCODE    PIC S9(09) VALUE ZEROS.                      
       77  WS-NOV-CODE   PIC XX    VALUE SPACES.                      
       77  FILLER        PIC X(26) VALUE '* DESCRIPCION SQLCA      *'.
                                                                      
      *****************AREA DE COPYS***********************           
           COPY TBVCLIEN.                                             
      *****************************************************           
       01  WS-CONT-L     PIC 9(3)  VALUE ZEROES.                      
     *****************************************************           
      01  WS-CONT-L     PIC 9(3)  VALUE ZEROES.                      
      01  WS-CONT-I     PIC 9(3)  VALUE ZEROES.                      
      01  WS-CONT-E     PIC 9(3)  VALUE ZEROES.                      
                                                                     
      01  APELLIDOYNOMBRE.                                           
          03 APELLIDO   PIC X(30) VALUE SPACES.                      
          03 FILLER     PIC X(01) VALUE SPACE.                       
          03 NOMBRE     PIC X(30) VALUE SPACES.                      
                                                                     
      01  WS-F   .                                                   
          88    WS-FIN  PIC X(2)  VALUE SPACES.                      
          88    WS-NOF  PIC X(2)  VALUE SPACES.                      
     *****************************************************           
     * LAYOUT DEL REGISTRO DE CLIENTES                   *           
     *****************************************************           
          EXEC SQL  INCLUDE TBCURCLI   END-EXEC.                     
     *****************************************************           
     * SQLCA COMMUNICATION AREA CON EL DB2               *           
     *****************************************************           
          EXEC SQL INCLUDE SQLCA  END-EXEC.                          
                                                                     
     *****************************************************           
      PROCEDURE DIVISION.                                            
     **************************************                          
     *                                    *                          
     *  CUERPO PRINCIPAL DEL PROGRAMA     *                          
     *                                    *                          
     **************************************                          
      0000-MAIN-PROCESS.                                             
                                                                     
          PERFORM 1000-INICIO  THRU F-1000-INICIO.                   
                                                                     
          PERFORM 2000-PROCESO THRU F-2000-PROCESO                   
                  UNTIL WS-FIN.                                      
                                                                     
          PERFORM 9999-FINAL   THRU F-9999-FINAL.                    
                                                                     
          GOBACK.                                                    
                                                                     
     **************************************                          
     *                                    *                          
     *  CUERPO INICIO APERTURA FILES      *                          
     *                                    *                          
     **************************************                          
      1000-INICIO.                                                   
          OPEN OUTPUT NOVEDAD.                                       
                                                                     
          IF WS-NOV-CODE IS NOT EQUAL '00'                           
             DISPLAY '* ERROR EN OPEN NOVEDAD = ' WS-NOV-CODE        
             MOVE 3333 TO RETURN-CODE                                
             PERFORM 9999-FINAL THRU F-9999-FINAL                    
          END-IF.                                                    
                                                                     
      F-1000-INICIO. EXIT.                                           
     *****************************************************           
                                                                     
      2000-PROCESO.                                                  
     *****************************************************           
          READ NOVEDAD INTO WK-TBCLIE.                               
                                                                     
          EVALUATE WS-NOV-CODE                                       
            WHEN '00'                                                
            ADD 1 TO WS-CONT-L                                       
                                                                     
            MOVE WK-CLI-TIPO-DOCUMENTO TO WD-TIPDOC                  
            MOVE WK-CLI-NRO-DOCUMENTO  TO WD-NRODOC                  
            MOVE WK-CLI-NRO-CLIENTE    TO WD-NROCLI                  
            MOVE WK-CLI-NOMBRE-CLIENTE TO NOMBRE                     
            MOVE WK-CLI-APELLIDO-CLIENTE TO APELLIDO                 
            MOVE APELLIDOYNOMBRE       TO WD-                        
            MOVE WK-CLI-APELLIDO-CLIENTE TO WD-FECNAC                
            MOVE WK-CLI-TIPO-DOCUMENTO TO WD-SEXO                    
                                                                     
               EXEC SQL SELECT TIPCUEN, NROCUEN, NROCLI              
                    INTO :WS-KIPCUEN, :WS-NROCUEN, :WS-NROCLI        
                    FROM KC02787.TBCURCTA                            
                         WHERE  NROCLI = 123                         
               END-EXEC                                              
                                                                     
               IF SQLCODE = NOT-FOUND                                
                  DISPLAY 'TABLA    VACíA: '                         
               ELSE                                                  
                   IF SQLCODE = 0                                    
                     DISPLAY 'NROCLI   = '    WS-NROCLI              
                     MOVE WS-DETALLE  TO WS-REG-NOVEDAD              
                   ELSE                                              
                        MOVE SQLCODE TO WS-SQLCODE                   
                        DISPLAY 'ERROR DB2 '  WS-SQLCODE             
               END-IF                                                
               END-IF                                                
                                                                     
             WHEN '10'                                               
             SET WS-FIN          TO TRUE                             
                                                                     
          WHEN OTHER                                                 
             DISPLAY '* ERROR                     = ' WS-NOV-CODE    
             MOVE 9999 TO RETURN-CODE                                
             SET WS-FIN  TO TRUE                                     
                                                                     
             END-EVALUATE.                                           
                                                                     
      F-2000-PROCESO. EXIT.                                          
                                                                     
     **************************************                          
     *                                    *                          
     *  CUERPO FINAL CIERRE DE SALIDA     *                          
     *                                    *                          
     **************************************                          
      9999-FINAL.                                                    
          CLOSE SALIDA.                                              
                                                                     
          IF WS-SAL-CODE IS NOT EQUAL '00'                           
             DISPLAY '* ERROR EN CLOSE SALIDA  = ' WS-SAL-CODE       
             MOVE 9999 TO RETURN-CODE                                
          END-IF.                                                    
                                                                     
      F-9999-FINAL.  EXIT.                                           
                                                                     