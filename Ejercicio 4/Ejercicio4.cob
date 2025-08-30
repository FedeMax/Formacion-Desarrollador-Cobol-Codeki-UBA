       IDENTIFICATION DIVISION.                                        
       PROGRAM-ID. PGMCORT2.                                           
       AUTHOR.    FEDERICO FALCON.                                     
      **************************************************************   
       ENVIRONMENT DIVISION.                                           
      **************************************************************   
       CONFIGURATION SECTION.                                          
                                                                       
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                          
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
       01   REG-ENTRADA             PIC X(93).                         
      **************************************************************   
       WORKING-STORAGE SECTION.                                        
      **************************************************************   
                                                                       
           COPY CLICOB.                                                
                                                                       
      **************************************************************   
       77  FS-ENTRADA               PIC XX      VALUE SPACES.          
      **************************************************************   
                                                                       
       77  WS-STATUS                  PIC X.                           
           88  WS-FIN                 VALUE 'Y'.                       
          88  WS-NO-FIN              VALUE 'N'.                       
                                                                      
     *********************VARIABLES A USAR*************************   
                                                                      
      77 WS-DOC           PIC XX     VALUE SPACES.                    
      77 WS-DOC-ANT       PIC XX     VALUE SPACES.                    
      77 WS-SEXO          PIC  X     VALUE SPACE.                     
      77 WS-SEXO-ANT      PIC  X     VALUE SPACE.                     
                                                                      
      01 WS-BOOL          PIC  X.                                     
         88 SEGUIR                   VALUE 'S'.                       
         88 NO-SEGUIR                VALUE 'N'.                       
                                                                      
     *********************CONTADORES A USAR************************   
      77 WS-CON-MISMO     PIC 9(3)   VALUE ZEROES.                    
      77 WS-CON-TOTAL     PIC 9(4)   VALUE ZEROES.                    
      01 WS-GENERO.                                                   
         05 WS-SEXO-M        PIC 9(3)   VALUE ZEROES.                 
         05 WS-SEXO-F        PIC 9(3)   VALUE ZEROES.                 
         05 WS-SEXO-O        PIC 9(3)   VALUE ZEROES.                 
                                                                      
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
             DISPLAY '* ERROR EN OPEN SUCURSAL = ' FS-ENTRADA         
             MOVE 9999 TO RETURN-CODE                                 
             SET  WS-FIN     TO TRUE                                  
          END-IF.                                                     
                                                                      
          PERFORM 2500-LEER     THRU F-2500-LEER                      
                                                                      
          MOVE WS-DOC         TO    WS-DOC-ANT                        
          MOVE WS-SEXO        TO    WS-SEXO-ANT.                      
                                                                      
      F-1000-INICIO.   EXIT.                                          
     **************************************************************   
                                                                      
      2000-PROCESO.                                                   
                                                                      
          IF WS-DOC   =  WS-DOC-ANT                                   
             AND WS-DOC = ('DU' OR 'PA' OR 'PE' OR 'CI')              
                                                                      
            PERFORM 2400-VERIFICADOR THRU F-2400-VERIFICADOR          
             ADD 1 TO WS-CON-MISMO                                    
                                                                      
          ELSE                                                        
             PERFORM  3000-MOSTRAR-DOCU  THRU                         
                  F-3000-MOSTRAR-DOCU                                
            IF WS-DOC = ('DU' OR 'PA' OR 'PE' OR 'CI')               
          PERFORM 2400-VERIFICADOR THRU F-2400-VERIFICADOR           
            END-IF                                                   
           MOVE  WS-DOC        TO   WS-DOC-ANT                       
                                                                     
        END-IF.                                                      
                                                                     
        PERFORM 2500-LEER     THRU F-2500-LEER.                      
                                                                     
    F-2000-PROCESO. EXIT.                                            
                                                                     
   **************************************************************    
    2400-VERIFICADOR.                                                
                                                                     
           EVALUATE WS-SEXO                                          
               WHEN 'F'                                              
               ADD 1 TO WS-SEXO-F                                    
               WHEN 'M'                                              
               ADD 1 TO WS-SEXO-M                                    
               WHEN OTHER                                            
               ADD 1 TO WS-SEXO-O                                    
           END-EVALUATE.                                             
                                                                     
    F-2400-VERIFICADOR. EXIT.                                        
   **************************************************************    
    2500-LEER.                                                       
        READ ENTRADA   INTO WS-REG-CLICOB                            
                                                                     
        EVALUATE FS-ENTRADA                                          
          WHEN '00'                                                  
                                                                     
               MOVE WS-SUC-TIP-DOC  TO WS-DOC                        
               MOVE WS-SUC-SEXO     TO WS-SEXO                       
               ADD  1  TO  WS-CON-TOTAL                              
                                                                     
          WHEN '10'                                                  

             SET WS-FIN          TO TRUE                               
             PERFORM  3000-MOSTRAR-DOCU THRU                           
                    F-3000-MOSTRAR-DOCU                                
                                                                       
          WHEN OTHER                                                   
             DISPLAY '* ERROR EN LECTURA SUCURSAL = ' FS-ENTRADA       
             MOVE 9999 TO RETURN-CODE                                  
             SET WS-FIN  TO TRUE                                       
                                                                       
          END-EVALUATE.                                                
                                                                       
      F-2500-LEER. EXIT.                                               
                                                                       
     **************************************************************    
      3000-MOSTRAR-DOCU.                                               
                                                                       
          IF WS-DOC-ANT = ('DU' OR 'PA' OR 'PE' OR 'CI')               
            DISPLAY '----------------------------------------------'   
            DISPLAY 'TIPO DE DOCUMENTO ' WS-DOC-ANT                    
            PERFORM 3100-MOSTRAR-SEXO THRU F-3100-MOSTRAR-SEXO         
            DISPLAY 'TOTAL '  WS-CON-MISMO                             
            MOVE 1  TO WS-CON-MISMO                                    
            DISPLAY '----------------------------------------------'   
            INITIALIZE WS-GENERO                                       
          END-IF.                                                      
                                                                       
      F-3000-MOSTRAR-DOCU. EXIT.                                       
     **************************************************************    
      3100-MOSTRAR-SEXO.                                               
                                                                       
                                                                       
          DISPLAY 'FEMENINO  ' WS-SEXO-F                               
          DISPLAY 'MASCULINO ' WS-SEXO-M                               
          DISPLAY 'OTRO      ' WS-SEXO-O.                              
                                                                       
      F-3100-MOSTRAR-SEXO. EXIT.                                       
     **************************************************************    
                                                                    
    9999-FINAL.                                                     
                                                                    
        IF RETURN-CODE = 9999                                       
         CONTINUE                                                   
        ELSE                                                        
          CLOSE ENTRADA                                             
              IF FS-ENTRADA  IS NOT EQUAL '00'                      
               DISPLAY '* ERROR EN CLOSE SUCURSAL = '               
                                         FS-ENTRADA                 
               MOVE 9999 TO RETURN-CODE                             
               SET WS-FIN     TO TRUE                               
              END-IF                                                
                                                                    
                                                                    
   **************************************                           
   *   MOSTRAR TOTALES DE CONTROL       *                           
   **************************************                           
                                                                    
             DISPLAY '---------------------------'                  
             DISPLAY 'TOTAL DE LEIDOS ' WS-CON-TOTAL                
             DISPLAY '---------------------------'                  
                                                                    
        END-IF.                                                     
                                                                    
    F-9999-FINAL.                                                   
        EXIT.                                                       