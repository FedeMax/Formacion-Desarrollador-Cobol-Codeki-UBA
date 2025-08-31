***************************** Top of Data ******************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VECTORES.                                            
       AUTHOR.    FEDERICO FALCON.                                      
      **************************************************************    
       ENVIRONMENT DIVISION.                                            
      **************************************************************    
       CONFIGURATION SECTION.                                           
                                                                        
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           
      **************************************************************    
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
             SELECT PRODUCT ASSIGN TO DDPRODU                           
                    FILE STATUS IS FS-PRODUCT.                          
                                                                        
             SELECT PRECIOS ASSIGN TO DDPRECI                           
                    FILE STATUS IS FS-PRECIO.                           
      **************************************************************    
       I-O-CONTROL.                                                     
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD   PRODUCT                                                     
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F.                                         
       01   REG-PRODUCT           PIC X(32).                            
                                                                        
       FD   PRECIOS                                                     
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F.                                         
       01   REG-PRECIO            PIC X(07).                            
      **************************************************************    
       WORKING-STORAGE SECTION.                                         
      **************************************************************    
       01   REG-PRECIO            PIC X(07).                           
      **************************************************************   
       WORKING-STORAGE SECTION.                                        
      **************************************************************   
       77  FS-PRODUCT                 PIC XX       VALUE SPACES.       
       77  FS-PRECIO                  PIC XX       VALUE SPACES.       
                                                                       
       77  WS-STATUS                  PIC X.                           
           88  WS-FIN                 VALUE 'Y'.                       
           88  WS-NO-FIN              VALUE 'N'.                       
                                                                       
      *********************VARIABLES A USAR*************************   
                                                                       
       77  CONT-I                     PIC  9(02)    VALUE ZEROES.      
       77  CONT-J                     PIC  9(02)    VALUE ZEROES.      
                                                                       
       01  PRODUCTO.                                                   
           05 COD-PROD                PIC  9(02)    VALUE ZEROES.      
           05 DENOMINACION            PIC  X(30)    VALUE SPACES.      
                                                                       
       01  WS-PRECIO.                                                  
           05 COD-PRECIO              PIC  9(02)    VALUE ZEROES.      
           05 PRECIO                  PIC  9(03)V99 VALUE ZEROES.      
                                                                       
       01  WS-PRECIO-PRINT            PIC $ZZ9,99.                     
                                                                       
       01  TABLA.                                                      
           03  ITEMS OCCURS 13 TIMES.                                  
               05 T-COD-PROD              PIC  9(02)    VALUE ZEROES.  
               05 T-DENOMINACION          PIC  X(30)    VALUE SPACES.  
               05 T-PRECIO                PIC  9(03)V99 VALUE ZEROES.  
      **************************************************************   
       PROCEDURE DIVISION.                                             
      **************************************                           
      *                                    *                           
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                           
     *                                    *                            
     **************************************                            
      MAIN-PROGRAM.                                                    
                                                                       
          PERFORM 1000-INICIO  THRU   F-1000-INICIO.                   
                                                                       
          PERFORM 2000-PROCESO    THRU F-2000-PROCESO                  
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
                                                                       
          OPEN INPUT  PRODUCT.                                         
                                                                       
          IF FS-PRODUCT  IS NOT EQUAL '00'                             
             DISPLAY '* ERROR EN OPEN PRODUCTO = ' FS-PRODUCT          
             MOVE 9999 TO RETURN-CODE                                  
             SET  WS-FIN     TO TRUE                                   
          END-IF.                                                      
                                                                       
          OPEN INPUT  PRECIOS.                                         
                                                                       
          IF FS-PRECIO   IS NOT EQUAL '00'                             
             DISPLAY '* ERROR EN OPEN PRECIO   = ' FS-PRECIO           
             MOVE 9999 TO RETURN-CODE                                  
             SET  WS-FIN     TO TRUE                                   
          END-IF.                                                      
                                                                      
      F-1000-INICIO.   EXIT.                                          
     **************************************************************   
                                                                      
      2000-PROCESO.                                                   
                                                                      
          PERFORM 2100-LEER-PRODU     THRU F-2100-LEER-PRODU .        
                                                                      
      F-2000-PROCESO. EXIT.                                           
                                                                      
     **************************************************************   
      2100-LEER-PRODU.                                                
                                                                      
          PERFORM VARYING CONT-I FROM 1 BY 1 UNTIL CONT-I > 13        
            READ PRODUCT   INTO PRODUCTO                              
                                                                      
          EVALUATE FS-PRODUCT                                         
            WHEN '00'                                                 
                   MOVE COD-PROD     TO  T-COD-PROD(CONT-I)           
                   MOVE DENOMINACION TO  T-DENOMINACION(CONT-I)       
                                                                      
             WHEN '10'                                                
             PERFORM 2200-LEER-PRECIO THRU F-2200-LEER-PRECIO         
             SET WS-FIN TO TRUE                                       
                                                                      
          WHEN OTHER                                                  
             DISPLAY '* ERROR EN LECTURA PRODUCTO = ' FS-PRODUCT      
             MOVE 9999 TO RETURN-CODE                                 
             SET WS-FIN  TO TRUE                                      
                                                                      
          END-EVALUATE                                                
                                                                      
          END-PERFORM.                                                
                                                                      
      F-2100-LEER-PRODU. EXIT.                                        
                                                                      
     **************************************************************   
      2200-LEER-PRECIO.                                               
                                                                      
          PERFORM VARYING CONT-I FROM 1 BY 1 UNTIL CONT-I > 13        
          READ PRECIOS   INTO WS-PRECIO                               
                                                                      
          EVALUATE FS-PRODUCT                                         
            WHEN '00'                                                 
               PERFORM VARYING CONT-J FROM 1 BY 1 UNTIL CONT-J > 13   
               IF COD-PRECIO = T-COD-PROD(CONT-J)                     
                 MOVE PRECIO  TO T-PRECIO(CONT-J)                     
                 MOVE 14 TO CONT-J                                    
               END-IF                                                 
             END-PERFORM                                              
                                                                      
             WHEN '10'                                                
             SET WS-FIN          TO TRUE                              
                                                                      
          WHEN OTHER                                                  
             DISPLAY '* ERROR EN LECTURA PRECIO   = ' FS-PRECIO       
             MOVE 14 TO CONT-I                                        
             MOVE 9999 TO RETURN-CODE                                 
             SET WS-FIN  TO TRUE                                      
                                                                      
          END-EVALUATE                                                
          END-PERFORM.                                                
                                                                      
      F-2200-LEER-PRECIO. EXIT.                                       
     **************************************************************   
                                                                      
      9999-FINAL.                                                     
                                                                      
          PERFORM VARYING CONT-I FROM 1 BY 1 UNTIL CONT-I > 13        
            MOVE T-PRECIO(CONT-I) TO WS-PRECIO-PRINT                  
                                                                      
            IF T-COD-PROD(CONT-I) EQUAL ZEROS                         
              CONTINUE                                                
            ELSE                                                      
              DISPLAY 'CODIGO DE PRODUCTO ' T-COD-PROD(CONT-I)        
              DISPLAY 'DENOMINACION ' T-DENOMINACION(CONT-I)          
                                                                      
              IF T-PRECIO (CONT-I) = ZEROS                            
                 DISPLAY 'PRECIO NO ENCONTRADO '                      
              ELSE                                                    
                 DISPLAY 'PRECIO = ' WS-PRECIO-PRINT                  
                                                                      
              END-IF                                                  
            END-IF                                                    
                                                                      
          END-PERFORM.                                                
                                                                      
          IF RETURN-CODE = 9999                                       
           CONTINUE                                                   
          ELSE                                                        
            CLOSE PRODUCT                                             
                IF FS-PRODUCT  IS NOT EQUAL '00'                      
                 DISPLAY '* ERROR EN CLOSE PRODUCT  = '               
                                           FS-PRODUCT                 
                 MOVE 9999 TO RETURN-CODE                             
                 SET WS-FIN     TO TRUE                               
                END-IF                                                
                                                                      
            CLOSE PRECIOS                                             
                IF FS-PRECIO  IS NOT EQUAL '00'                       
                 DISPLAY '* ERROR EN CLOSE PRECIO   = '               
                                           FS-PRECIO                  
                 MOVE 9999 TO RETURN-CODE                             
                 SET WS-FIN     TO TRUE                               
                END-IF                                                
                                                                      
          END-IF.                                                     
        F-9999-FINAL.                                                    
            EXIT.                                                        
 **************************** Bottom of Data ****************************
                                                                         
                                                                         