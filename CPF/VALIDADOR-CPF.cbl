      ******************************************************************
      * Author:    GABRIEL CASANOVA SILVA
      * Date:      27/02/2020
      * Purpose:   ESTUDO
      * Tectonics: cobc
      ******************************************************************
           IDENTIFICATION DIVISION.
           PROGRAM-ID. CPF-VALIDADOR.

           DATA DIVISION.
               WORKING-STORAGE SECTION.

                   1 CPF.
                       5 DIGITO    PIC 9(01) OCCURS 11.

                   77 IND          PIC 9(02).
                   77 MULT         PIC 9(02).
                   77 SOMA         PIC 9(04).
                   77 DIV          PIC 9(02).
                   77 RAN          PIC 99.999.

           PROCEDURE DIVISION.
               MAIN-PROCEDURE.

                ACCEPT CPF.

                PERFORM CALCULO-CPF-DIG-1.
                PERFORM CALCULO-CPF-DIG-2.

                DISPLAY 'CPF VALIDO !'

                COMPUTE SOMA = SOMA * FUNCTION RANDOM.
                DISPLAY SOMA.

           STOP RUN.

           CPF-INVALIDO.
               DISPLAY 'CPF INVALIDO.'
               STOP RUN.

           CALCULO-CPF-DIG-1.
               MOVE 0  TO SOMA
               MOVE 10 TO MULT.

               PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 9

                   COMPUTE SOMA = SOMA + (DIGITO(IND) * MULT)

                   COMPUTE MULT = MULT - 1

               END-PERFORM.

               COMPUTE SOMA = SOMA * 10.
               DIVIDE  SOMA BY 11 GIVING DIV REMAINDER SOMA .

               IF SOMA NOT EQUALS DIGITO(10)
                   GO TO CPF-INVALIDO
               END-IF.

           CALCULO-CPF-DIG-2.
               MOVE 0  TO SOMA.
               MOVE 11 TO MULT.

               PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 10
                   COMPUTE SOMA = SOMA + (DIGITO(IND) * MULT)
                   COMPUTE MULT = MULT - 1
               END-PERFORM.

               COMPUTE SOMA = SOMA * 10.
               DIVIDE  SOMA BY 11 GIVING DIV REMAINDER SOMA .

               IF SOMA NOT EQUALS DIGITO(11)
                   GO TO CPF-INVALIDO
               END-IF.
