***************************** Top of Data ******************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PGMAPCBF.                                            
       AUTHOR.    FEDERICO FALCON.                                      
      **************************************************************    
       ENVIRONMENT DIVISION.                                            
      **************************************************************    
       CONFIGURATION SECTION.                                           
                                                                        
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
      **************************************************************    
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT CLIENTE    ASSIGN TO DDCLIEN                          
                 FILE STATUS IS FS-CLIENTE.                             
                                                                        
           SELECT MOVIMI     ASSIGN TO DDMOVIM                          
                 FILE STATUS IS FS-MOVIMI.                              
                                                                        
      *    SELECT SALIDA     ASSIGN TO DDSAL                            
      *          FILE STATUS IS FS-SALIDA.                              
      **************************************************************    
       I-O-CONTROL.                                                     
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD   CLIENTE                                                     
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F.                                         
                                                                        
       01   REG-CLIENTE             PIC X(30).                          
                                                                        
       FD   MOVIMI                                                      
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F.                                         
                                                                        
       01   REG-MOVIMIENTO          PIC X(80).                          
                                                                        
      *FD   SALIDA                                                      
      *    BLOCK CONTAINS 0 RECORDS                                     
      *    RECORDING MODE IS F.                                         
                                                                        
      *01   REG-SALIDA              PIC X(30).                          
      **************************************************************    
       WORKING-STORAGE SECTION.                                         
       77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
       77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'.  
                                                                        
      ***********************FILE STATUS****************************    
       77  FS-CLIENTE                 PIC XX      VALUE SPACES.         
       77  FS-MOVIMI                  PIC XX      VALUE SPACES.         
      *77  FS-SALIDA                  PIC XX      VALUE SPACES.         
                                                                        
       77  WS-STATUS                  PIC X.                            
           88  WS-FIN                 VALUE 'Y'.                        
           88  WS-NO-FIN              VALUE 'N'.                        
                                                                        
       77  WS-CLIEN                   PIC X.                            
           88  WS-FIN-CLI             VALUE 'Y'.                        
           88  WS-NO-FIN-CLI          VALUE 'N'.                        
                                                                        
       77  WS-MOVIMI                  PIC X.                            
           88  WS-FIN-MOV             VALUE 'Y'.                        
           88  WS-NO-FIN-MOV          VALUE 'N'.                        
      *********************VARIABLES A USAR*************************    
       01 WS-CLI-LEIDO              PIC 9(03)   VALUE ZEROES.           
       01 WS-MOV-LEIDO              PIC 9(03)   VALUE ZEROES.           
       01 WS-IGUALES                PIC 9(03)   VALUE ZEROES.           
                                                                        
       01 WS-CLAVE.                                                     
          05 CLI-TIPO                 PIC 9(02)   VALUE ZEROES.         
          05 CLI-CUENTA               PIC 9(08)   VALUE ZEROES.        
                                                                       
       01 WS-CLAVE2.                                                   
          05 MOV-TIPO                 PIC 9(02)   VALUE ZEROES.        
          05 MOV-CUENTA               PIC 9(08)   VALUE ZEROES.        
                                                                       
      ***********************COPYS**********************************   
           COPY CLIENTE.                                               
           COPY MOVIMCC.                                               
                                                                       
       01  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'. 
                                                                       
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
                                                                      
          OPEN INPUT   CLIENTE.                                       
                                                                      
          IF FS-CLIENTE  IS NOT EQUAL '00'                            
             DISPLAY '* ERROR EN OPEN CLIENTE  = ' FS-CLIENTE         
             MOVE 9999 TO RETURN-CODE                                 
             SET  WS-FIN     TO TRUE                                  
          END-IF.                                                     
                                                                      
          OPEN INPUT  MOVIMI.                                         
                                                                      
          IF FS-MOVIMI   IS NOT EQUAL '00'                            
             DISPLAY '* ERROR EN OPEN MOVIMIENTO = ' FS-MOVIMI        
             MOVE 9999 TO RETURN-CODE                                 
             SET  WS-FIN     TO TRUE                                  
          END-IF.                                                     
                                                                      
                                                                      
     *    OPEN OUTPUT SALIDA.                                         
                                                                      
     *    IF FS-SALIDA   IS NOT EQUAL '00'                            
     *       DISPLAY '* ERROR EN OPEN SALIDA     = ' FS-SALIDA        
     *       MOVE 9999 TO RETURN-CODE                                 
     *       SET  WS-FIN     TO TRUE                                  
     *    END-IF.                                                     
                                                                      
          PERFORM 2500-LEER-CLIENTE    THRU F-2500-LEER-CLIENTE.      
          PERFORM 2600-LEER-MOVIMI     THRU F-2600-LEER-MOVIMI.       
                                                                      
      F-1000-INICIO.   EXIT.                                          
     **************************************************************   
                                                                      
      2000-PROCESO.                                                   
                                                                      
     *          DISPLAY '-------------------------'                   
          IF WS-CLAVE = WS-CLAVE2                                     
                                                                      
              DISPLAY '-------------------------'                     
              DISPLAY ' COINCIDENCIA ENCONTRADA '                     
              DISPLAY ' NR DE CLIENTE ' WS-CLAVE                      
                      ' Y NR DE MOVIM ' WS-CLAVE2                     
              DISPLAY '-------------------------'                     
                                                                      
                    ADD 1 TO WS-IGUALES                               
                    PERFORM 2600-LEER-MOVIMI                          
                            THRU F-2600-LEER-MOVIMI                   
                                                                      
          ELSE                                                        
              IF WS-CLAVE > WS-CLAVE2                                 
                                                                      
     *              DISPLAY '-------------------------'               
                    DISPLAY ' ACTUALIZANDO MOVIMIENTO '               
     *              DISPLAY '-------------------------'               
                    PERFORM 2600-LEER-MOVIMI                          
                            THRU F-2600-LEER-MOVIMI                   
                 ELSE                                                 
                                                                      
     *              DISPLAY '-------------------------'               
                    DISPLAY ' ACTUALIZANDO CLIENTE    '               
     *              DISPLAY '-------------------------'               
                    PERFORM 2500-LEER-CLIENTE                         
                            THRU F-2500-LEER-CLIENTE                  
              END-IF                                                  
          END-IF                                                      
                                                                      
          IF WS-FIN-CLI AND WS-FIN-MOV                                
          SET WS-FIN TO TRUE                                          
              DISPLAY '-------------------------'                     
          END-IF.                                                     
                                                                      
      F-2000-PROCESO. EXIT.                                           
                                                                     
    **************************************************************   
     2500-LEER-CLIENTE.                                              
         READ CLIENTE   INTO WS-REG-CLIENTE                          
              AT END SET WS-FIN-CLI TO TRUE.                         
                                                                     
         EVALUATE FS-CLIENTE                                         
           WHEN '00'                                                 
                    ADD 1 TO WS-CLI-LEIDO                            
                    MOVE WS-CLI-TIPO TO CLI-TIPO                     
                    MOVE WS-CLI-CUENTA TO CLI-CUENTA                 
                                                                     
            WHEN '10'                                                
            SET WS-FIN-CLI      TO TRUE                              
                 MOVE HIGH-VALUE TO WS-CLAVE                         
                                                                     
         WHEN OTHER                                                  
            DISPLAY '* ERROR EN LECTURA CLIENTE  = ' FS-CLIENTE      
            MOVE 9999 TO RETURN-CODE                                 
            SET WS-FIN-CLI  TO TRUE                                  
                                                                     
         END-EVALUATE.                                               
                                                                     
     F-2500-LEER-CLIENTE. EXIT.                                      
                                                                     
    **************************************************************   
     2600-LEER-MOVIMI.                                               
         READ MOVIMI     INTO WS-REG-MOVIMI                          
              AT END SET WS-FIN-MOV TO TRUE.                         
                                                                     
         EVALUATE FS-MOVIMI                                          
           WHEN '00'                                                 
                    ADD 1 TO WS-MOV-LEIDO                            
                    MOVE WS-MOV-TIPO TO MOV-TIPO                     
                    MOVE WS-MOV-CUENTA TO MOV-CUENTA                 
                                                                     
            WHEN '10'                                                
            SET WS-FIN-MOV      TO TRUE                              
                 MOVE HIGH-VALUE TO WS-CLAVE2                        
                                                                     
         WHEN OTHER                                                  
            DISPLAY '* ERROR EN LECTURA MOVIMIENTO = ' FS-MOVIMI     
            MOVE 9999 TO RETURN-CODE                                 
            SET WS-FIN-MOV  TO TRUE                                  
                                                                     
         END-EVALUATE.                                               
                                                                     
     F-2600-LEER-MOVIMI. EXIT.                                       
    **************************************************************   
                                                                     
     9999-FINAL.                                                     
                                                                     
         IF RETURN-CODE = 9999                                       
          CONTINUE                                                   
         ELSE                                                        
           CLOSE CLIENTE                                             
               IF FS-CLIENTE  IS NOT EQUAL '00'                      
                DISPLAY '* ERROR EN CLOSE CLIENTE  = '               
                                          FS-CLIENTE                 
                MOVE 9999 TO RETURN-CODE                             
                SET WS-FIN     TO TRUE                               
               END-IF                                                
                                                                     
           CLOSE MOVIMI                                              
               IF FS-MOVIMI   IS NOT EQUAL '00'                      
                DISPLAY '* ERROR EN CLOSE MOVIMIENTO = '             
                                          FS-MOVIMI                  
                MOVE 9999 TO RETURN-CODE                             
                SET WS-FIN     TO TRUE                               
         END-IF                                                      
                                                                     
         DISPLAY '***************************************'           
           DISPLAY ' CLIENTES LEIDOS             :  ' WS-CLI-LEIDO      
           DISPLAY '***************************************'            
           DISPLAY ' MOVIMIENTOS   LEIDOS        :  ' WS-MOV-LEIDO      
           DISPLAY '***************************************'            
           DISPLAY ' COINCIDENCIAS ENCONTRADAS   :  ' WS-IGUALES        
           DISPLAY '***************************************'.           
                                                                        
       F-9999-FINAL.                                                    
           EXIT.                                                        
**************************** Bottom of Data ****************************
                                                                        