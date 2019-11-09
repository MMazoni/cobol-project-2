      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGLA_ESTADOS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 ESTADOS.
               02 FILLER PIC X(21) VALUE "ACACRE".
               02 FILLER PIC X(21) VALUE "ALALAGOAS".
               02 FILLER PIC X(21) VALUE "APAMAPA".
               02 FILLER PIC X(21) VALUE "AMAMAZONAS".
               02 FILLER PIC X(21) VALUE "BABAHIA".
               02 FILLER PIC X(21) VALUE "CECEARA".
               02 FILLER PIC X(21) VALUE "DFDISTRITO FEDERAL".
               02 FILLER PIC X(21) VALUE "ESESPIRITO SANTO".
               02 FILLER PIC X(21) VALUE "GOGOIAS".
               02 FILLER PIC X(21) VALUE "MAMARANHAO".
               02 FILLER PIC X(21) VALUE "MTMATO GROSSO".
               02 FILLER PIC X(21) VALUE "MSMATO GROSSO DO SUL".
               02 FILLER PIC X(21) VALUE "MGMINAS GERAIS".
               02 FILLER PIC X(21) VALUE "PAPARA".
               02 FILLER PIC X(21) VALUE "PBPARAIBA".
               02 FILLER PIC X(21) VALUE "PRPARANA".
               02 FILLER PIC X(21) VALUE "PEPERNAMBUCO".
               02 FILLER PIC X(21) VALUE "PIPIAUI".
               02 FILLER PIC X(21) VALUE "RJRIO DE JANEIRO".
               02 FILLER PIC X(21) VALUE "RNRIO GRANDE DO NORTE".
               02 FILLER PIC X(21) VALUE "RSRIO GRANDE DO SUL".
               02 FILLER PIC X(21) VALUE "RORONDONIA".
               02 FILLER PIC X(21) VALUE "RRRORAIMA".
               02 FILLER PIC X(21) VALUE "SCSANTA CATARINA".
               02 FILLER PIC X(21) VALUE "SPSAO PAULO".
               02 FILLER PIC X(21) VALUE "SESERGIPE".
               02 FILLER PIC X(21) VALUE "TOTOCANTINS".

           01 TABELA-ESTADOS REDEFINES ESTADOS.
               02 TAB-ESTADOS OCCURS 27 TIMES
                           INDEXED BY TABELA-ESTADOS-INDEX.
                   03 SIGLA-T  PIC X(2).
                   03 ESTADO-T PIC X(19).

           01 DADOS.
               02 CONTROLE PIC 9 VALUE ZEROS.
               02 OPCAO PIC X VALUE SPACES.
               02 CONT  PIC 99 VALUE ZEROS.
               02 ENTRADA-SIGLA PIC X(2) VALUE SPACES.

           SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 12 COLUMN 11 VALUE
               "DIGITE A SIGLA DE UM ESTADO:".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CORPO UNTIL OPCAO = "N".
           DISPLAY "FIM DO PROGRAMA" AT 2030.
           STOP "".
           STOP RUN.

       CORPO.
           PERFORM ABERTURA.
           PERFORM RECEBE-SIGLA.
           PERFORM MOSTRA.
           PERFORM CONTINUA UNTIL OPCAO = "S" OR "N".
           DISPLAY SPACE ERASE EOS AT LINE 17.

       ABERTURA.
           MOVE SPACES TO ENTRADA-SIGLA.
           MOVE SPACES TO OPCAO.
           MOVE ZEROS TO CONTROLE.
           DISPLAY TELA.

       RECEBE-SIGLA.
           ACCEPT ENTRADA-SIGLA AT 1240 WITH PROMPT AUTO.
           MOVE FUNCTION UPPER-CASE (ENTRADA-SIGLA) TO ENTRADA-SIGLA.

       MOSTRA.
           MOVE 1 TO CONT.
           PERFORM UNTIL CONT > 27
               IF ENTRADA-SIGLA = SIGLA-T(CONT)
                   DISPLAY "VOCE DIGITOU: " AT 1320
                   DISPLAY ESTADO-T(CONT) AT 1335
                   MOVE 1 TO CONTROLE
               END-IF
               ADD 1 TO CONT
           END-PERFORM.
               IF CONTROLE = 0
                   DISPLAY "ESTADO INEXISTENTE!" AT 1320
           END-IF.

       CONTINUA.
           DISPLAY "DESEJA CONTINUAR? (S/N): " AT 1625.
           ACCEPT OPCAO AT 1650 WITH PROMPT AUTO.
           MOVE FUNCTION UPPER-CASE (OPCAO) TO OPCAO.
           IF OPCAO <> "N" OR OPCAO <> "S"
               DISPLAY "OPCAO INVALIDA! DIGITE S OU N" AT 1725
           END-IF.
       END PROGRAM SIGLA_ESTADOS.
