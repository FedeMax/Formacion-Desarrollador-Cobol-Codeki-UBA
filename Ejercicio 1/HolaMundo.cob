       IDENTIFICATION DIVISION.                                        
       PROGRAM-ID. HOLAM.                                              
       AUTHOR.     FEDERICO FALCON.                                    
      **************************************************************** 
      * PROGRAMA QUE MUESTRA UN HOLA MUNDO Y SUMA DE 1 EN 1 HASTA 10 * 
      **************************************************************** 
       DATA DIVISION.                                                  
       WORKING-STORAGE SECTION.                                        
                                                                       
       01  VALOR                PIC 9 VALUE 1.                         
       01  TOTAL                PIC 9(02) VALUE ZEROES.                
                                                                       
       PROCEDURE DIVISION.                                             
       MAIN-PROGRAM.                                                   
                                                                       
           DISPLAY ' HOLA MUNDO '                                      
                                                                       
           PERFORM SUMAR 10 TIMES                                      
                                                                       
           DISPLAY ' SUMATORIA ' TOTAL                                 
                                                                       
                GOBACK.                                                
       SUMAR.                                                          
                                                                       
           ADD VALOR TO TOTAL                                          
           ADD 1 TO VALOR                                              
           EXIT.                                                       