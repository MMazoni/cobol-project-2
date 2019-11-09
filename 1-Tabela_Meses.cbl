      ******************************************************************
      * Author: GRU
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABELA_MESES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 OPCAO PIC X VALUE SPACES.
      *    Inicialização ou definição do conteúdo da tabela,
      *    que será compartilhada por outra variável
           01 MESES-ANO.
               02 FILLER PIC X(9) VALUE "JANEIRO".
               02 FILLER PIC X(9) VALUE "FEVEREIRO".
               02 FILLER PIC X(9) VALUE "MARCO".
               02 FILLER PIC X(9) VALUE "ABRIL".
               02 FILLER PIC X(9) VALUE "MAIO".
               02 FILLER PIC X(9) VALUE "JUNHO".
               02 FILLER PIC X(9) VALUE "JULHO".
               02 FILLER PIC X(9) VALUE "AGOSTO".
               02 FILLER PIC X(9) VALUE "SETEMBRO".
               02 FILLER PIC X(9) VALUE "OUTUBRO".
               02 FILLER PIC X(9) VALUE "NOVEMBRO".
               02 FILLER PIC X(9) VALUE "DEZEMBRO".
      *    REDEFINES indica que as duas variáveis compartilham
      *     o mesmo espaço de memória.
           01 TABELA-MESES REDEFINES MESES-ANO.
               02 MES-T PIC X(9) OCCURS 12 TIMES.
      *     Indica que a variável MES-T fará o acesso à tabela,
      *     indicando a presença de um índice para pesquisa na tabela
      *     => MES-T(1) = Janeiro até MES-T(12) = Dezembro
           01 DATA-QUALQUER.
               02 DIA PIC 99 VALUE ZEROS.
               02 MES PIC 99 VALUE ZEROS.
               02 ANO PIC 99 VALUE ZEROS.
           01 DATA-M.
               02 DIA-M PIC Z9.
               02 MES-M PIC Z9.
               02 ANO-M PIC Z9.

       SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 12 COLUMN 42 VALUE "/".
               02 LINE 12 COLUMN 45 VALUE "/".
               02 LINE 12 COLUMN 11 VALUE
               "DIGITE A DATA A SER EXIBIDA:".
       PROCEDURE DIVISION.
       INICIO.
           PERFORM CORPO UNTIL OPCAO = "N".
           DISPLAY "FIM DO PROGRAMA" AT 2030.
           STOP "".
           STOP RUN.

       CORPO.
           PERFORM ABERTURA.
           PERFORM RECEBE-DIA UNTIL DIA >= 1 AND <= 31.
           DISPLAY SPACE ERASE EOS AT LINE 15.
           PERFORM RECEBE-MES UNTIL MES >= 1 AND <= 12.
           PERFORM VERIFICA.
           DISPLAY SPACE ERASE EOS AT LINE 15.
           PERFORM RECEBE-ANO UNTIL ANO > 0.
           DISPLAY SPACE ERASE EOS AT LINE 15.
           PERFORM MOSTRA.
           PERFORM CONTINUA UNTIL OPCAO = "S" OR "N".
           DISPLAY SPACE ERASE EOS AT LINE 17.

       ABERTURA.
           DISPLAY TELA.
           PERFORM MSGS-VERIFICA.
           MOVE ZEROS TO DIA.
           MOVE ZEROS TO MES.
           MOVE ZEROS TO ANO.
           MOVE SPACES TO OPCAO.


       RECEBE-DIA.
           ACCEPT DIA-M AT 1240 WITH PROMPT AUTO.
           MOVE DIA-M TO DIA.
           IF DIA < 1 OR DIA > 31
               DISPLAY "DIA DEVE SER ENTRE 1 E 31" AT 1511
           END-IF.

       RECEBE-MES.
           ACCEPT MES-M AT 1243 WITH PROMPT AUTO.
           MOVE MES-M TO MES.
           IF MES < 1 OR MES > 12
               DISPLAY "MES DEVE SER ENTRE 1 E 12" AT 1511
           END-IF.

       RECEBE-ANO.
           ACCEPT ANO-M AT 1246 WITH PROMPT AUTO.
           MOVE ANO-M TO ANO.
           IF ANO = 0
               DISPLAY "ANO DEVE SER MAIOR QUE 0" AT 1511
           END-IF.

       MOSTRA.
           DISPLAY "DATA POR EXTENSO: " AT 1320.
           DISPLAY DIA AT 1420.
           DISPLAY " DE " AT 1422.
           DISPLAY MES-T(MES) AT 1432.
           DISPLAY " DE " AT 1441
           DISPLAY ANO AT 1445.

       VERIFICA.
           IF MES = 2 AND DIA > 28
               PERFORM INICIO
           END-IF.
           IF MES = 4 OR MES = 6 OR MES = 9 OR MES = 11
               IF DIA > 30
                   PERFORM INICIO
               END-IF
           END-IF.

       MSGS-VERIFICA.
           IF MES = 2 AND DIA > 28
               DISPLAY "MES 02 TEM DE 1 A 28 DIAS" AT 1511
           END-IF.
           IF MES = 4 OR MES = 6 OR MES = 9 OR MES = 11
               IF DIA > 30
                   DISPLAY "MES DIGITADO DEVE TER ATE 30 DIAS" AT 1511
               END-IF
           END-IF.

       CONTINUA.
           DISPLAY "DESEJA CONTINUAR? (S/N): " AT 1625.
           ACCEPT OPCAO AT 1650 WITH PROMPT AUTO.
           MOVE FUNCTION UPPER-CASE (OPCAO) TO OPCAO.
           IF OPCAO <> "N" OR OPCAO <> "S"
               DISPLAY "OPCAO INVALIDA! DIGITE S OU N" AT 1725
           END-IF.

       END PROGRAM TABELA_MESES.