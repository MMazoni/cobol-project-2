      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. CADASTRO_CONTA_CORRENTE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	    SELECT ARQ-CADASTRO ASSIGN TO DISK
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS CODIGO
        FILE STATUS IS ARQ-OK.

        SELECT PROXIMO-CODIGO ASSIGN TO DISK
		ORGANIZATION LINE SEQUENTIAL
		ACCESS MODE SEQUENTIAL
		FILE STATUS ARQ-OK.

       DATA DIVISION.
       FILE SECTION.
      * PARAMETROS PARA O ARQUIVO DE CADASTRO
       FD  ARQ-CADASTRO LABEL RECORD STANDARD
           DATA RECORD IS REG-CONTA
           VALUE OF FILE-ID IS "CADASTRO.DAT".
       01 REG-CONTA.
          02 CODIGO PIC 9(4) VALUES ZEROS.
          02 NOME PIC X(50) VALUES SPACES.
          02 AGENCIA PIC X(5) VALUES SPACES.
          02 CONTA PIC X(12) VALUES SPACES.
          02 SALDO PIC S9(12)v99 VALUES ZEROS.

      * DEFINE PARAMETROS PARA GRAVAR AQUIVO QUE CONTEM PROXIMO CODIGO
       FD 	PROXIMO-CODIGO LABEL RECORD STANDARD
		        RECORD CONTAINS 4 CHARACTERS
		        DATA RECORD IS REG-CODIGO
		        VALUE OF FILE-ID IS "CODIGO.DAT".
       01	REG-CODIGO.
		        02 ULT-CODIGO PIC 9(4) VALUES ZEROS.

       WORKING-STORAGE SECTION.
            01 MASCARAS.
               02 CODIGO-M PIC Z(4).
               02 SALDO-M PIC ZZZ.ZZZ.ZZZ.ZZ9,99-.
               02 SALDO-CAPTURA PIC X(16).
            01 OPCAO PIC Z(1) VALUES ZEROS.
            01 ARQ-OK PIC X(2) VALUES SPACES.
            01 SALVAR PIC X(1) VALUES SPACES.

       SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 1 COLUMN 26 VALUE "CADASTRO DE CONTA CORRENTE".
               02 LINE 5 COLUMN 3 VALUE "1 - CADASTRAR NOVA CONTA".
               02 LINE 7 COLUMN 3 VALUE "2 - EDITAR CONTA".
               02 LINE 9 COLUMN 3 VALUE "3 - EXCLUIR CONTA".
               02 LINE 11 COLUMN 3 VALUE "4 - LISTAR CONTAS".
               02 LINE 13 COLUMN 3 VALUE "5 - SAIR".

           01 TELA-FINAL.
               02 BLANK SCREEN.
               02 LINE 20 COLUMN 30 VALUE "FIM DO PROGRAMA".
               
           01 TELA-DADOS.
               02 BLANK SCREEN.
               02 LINE 5 COLUMN 10 VALUE " CODIGO: ".
               02 LINE 7 COLUMN 10 VALUE "AGENCIA: ".
               02 LINE 9 COLUMN 10 VALUE " CONTA: ".
               02 LINE 11 COLUMN 10 VALUE "  NOME: ".
               02 LINE 13 COLUMN 10 VALUE " SALDO: " .

               

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CORPO UNTIL OPCAO = 5.
           DISPLAY TELA-FINAL.
           STOP " ".
           STOP RUN.

       CORPO.
           DISPLAY TELA.
           PERFORM EXECUTAR UNTIL OPCAO = 5.

       EXECUTAR.
           DISPLAY "DIGITE A OPCAO DESEJADA [1-5]: " AT 1503.
           ACCEPT OPCAO AT 1534 WITH PROMPT AUTO.
           EVALUATE OPCAO
               WHEN 1
                   PERFORM CRIAR-CONTA
                   DISPLAY TELA
               WHEN 2
                   PERFORM ALTERAR-CONTA
                   DISPLAY TELA
               WHEN 5
                   CONTINUE
               WHEN OTHER
                  DISPLAY "OPCAO INVALIDA! DIGITE UMA "  AT 1703
                  DISPLAY "TECLA PARA CONTINUAR... " AT 1730
                  STOP " "
                  DISPLAY SPACE ERASE EOS AT LINE 17
           END-EVALUATE.

      ******************************************************************
      *
      * CRIA NOVA CONTA
      *
      ******************************************************************
       CRIAR-CONTA.
            DISPLAY TELA-DADOS.
            MOVE SPACES TO REG-CONTA.
            PERFORM RECEBE-AGENCIA UNTIL AGENCIA <> " ".
            PERFORM RECEBE-CONTA UNTIL CONTA <> " ".
            PERFORM RECEBE-NOME UNTIL NOME <> " ".
            PERFORM RECEBE-SALDO.
            DISPLAY "DESEJA SALVAR O REGISTRO (S/N)? < >" AT 1703.
            ACCEPT SALVAR AT 1736 WITH PROMPT AUTO.
            MOVE FUNCTION UPPER-CASE (SALVAR) TO SALVAR.
            IF SALVAR = "N"
                DISPLAY TELA
            ELSE
               PERFORM SALVAR-NOVA-CONTA
            END-IF.
       
       SALVAR-NOVA-CONTA.
           PERFORM DEFINE-PROXIMO-CODIGO.
           OPEN I-O ARQ-CADASTRO.
           IF ARQ-OK = "35"
                CLOSE ARQ-CADASTRO
                OPEN OUTPUT ARQ-CADASTRO
           END-IF.
           WRITE REG-CONTA
           INVALID KEY
                DISPLAY "ERRO AO GRAVAR!!" AT 1903
                STOP " "
                DISPLAY SPACE ERASE EOS AT LINE 19
           END-WRITE.
           CLOSE ARQ-CADASTRO.
           DISPLAY "REGISTRO SALVO" AT 1903.
           DISPLAY "DIGITE QUALQUER TECLA PARA CONTINUAR..." AT 2103.
           ACCEPT SALVAR AT 2142 WITH PROMPT AUTO.
           DISPLAY TELA.

       RECEBE-AGENCIA.
           ACCEPT AGENCIA AT 0719.
           IF AGENCIA = " "
               DISPLAY "E OBRIGATORIO DIGITAR A AGENCIA " AT 1703
               DISPLAY "(APERTE QUALQUER TECLA...)" AT 1735
               STOP " "
               DISPLAY SPACE ERASE EOS AT LINE 17
           END-IF.

       RECEBE-CONTA.
           ACCEPT CONTA AT 0919.
           IF CONTA = " "
               DISPLAY "E OBRIGATORIO DIGITAR A CONTA " AT 1703
               DISPLAY "(APERTE QUALQUER TECLA...)" AT 1735
               STOP " "
               DISPLAY SPACE ERASE EOS AT LINE 17
           END-IF.

       RECEBE-NOME.
           ACCEPT NOME AT 1119.
           IF NOME = " "
               DISPLAY "E OBRIGATORIO DIGITAR A NOME " AT 1703
               DISPLAY "(APERTE QUALQUER TECLA...)" AT 1735
               STOP " "
               DISPLAY SPACE ERASE EOS AT LINE 17
           END-IF.
           MOVE FUNCTION UPPER-CASE(NOME) TO NOME.
           DISPLAY NOME AT 1119.

       RECEBE-SALDO.
           ACCEPT SALDO-M AT 1319.
           MOVE SALDO-M TO SALDO.
           COMPUTE SALDO = SALDO / 100.
           MOVE SALDO TO SALDO-M.
           DISPLAY SALDO-M AT 1319.

       DEFINE-PROXIMO-CODIGO.
            OPEN INPUT PROXIMO-CODIGO.
            EVALUATE ARQ-OK
                WHEN "00"
                   READ PROXIMO-CODIGO RECORD
                   MOVE ULT-CODIGO TO CODIGO
                   MOVE CODIGO TO CODIGO-M
                   COMPUTE ULT-CODIGO = ULT-CODIGO + 1
      *CODIGO DE QUANDO SE TENTA ABRIR ARQUIVO QUE NÃO EXISTE
                WHEN "35"
                   MOVE 1 TO CODIGO
                   MOVE 2 TO ULT-CODIGO
                   MOVE CODIGO TO CODIGO-M
            END-EVALUATE.
            DISPLAY CODIGO-M AT 0719.
            CLOSE PROXIMO-CODIGO.
            OPEN OUTPUT PROXIMO-CODIGO.
            STOP " ".
            WRITE REG-CODIGO.
            CLOSE PROXIMO-CODIGO.
      ******************************************************************
      * FINALIZAÇÃO DAS FUNÇÕES PARA CRIAÇÃO DE NOVA CONTA
      ******************************************************************     
      ******************************************************************
      *
      * ALTERAR CONTA
      *
      ******************************************************************
       ALTERAR-CONTA.
           DISPLAY TELA-DADOS.
           MOVE SPACES TO REG-CONTA.
           PERFORM RECEBE-AGENCIA UNTIL AGENCIA <> " ".
           PERFORM RECEBE-CONTA UNTIL CONTA <> " ".
           DISPLAY "DESEJA ALTERAR A CONTA (S/N)? < >" AT 1703.
           ACCEPT SALVAR AT 1734 WITH PROMPT AUTO.
           MOVE FUNCTION UPPER-CASE (SALVAR) TO SALVAR.
           IF SALVAR = "N"
               DISPLAY TELA
            ELSE IF SALVAR = "S"
                DISPLAY "TESTE"
            ELSE
               DISPLAY "APENAS S/N!" AT 1903
      * MELHORAR ESSA PARTE (LOOP S/N)
               PERFORM ALTERAR-CONTA
            END-IF.

       END PROGRAM CADASTRO_CONTA_CORRENTE.
