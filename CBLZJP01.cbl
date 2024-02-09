       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZJP01.
      ******************************************************************
      * Author: JENYFFER LAURA PROCHNO PEREIRA
      * Date: 16012024
      * Purpose: CALCULAR O SALARIO LIQUIDO DOS FUNCIONARIOS
      *          DE UMA EMPRESA
      * Tectonics: .CBL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-SALARIO-BRUTO            PIC 9(013)V99  VALUES ZEROS.
       01  WS-HORAS-TRAB               PIC 9(004)     VALUES ZEROS.
       01  WS-SALARIO-LIQUIDO          PIC 9(013)V99  VALUES ZEROS.
       01  WS-HORA-EXTRA               PIC 9(013)V99  VALUES ZEROS.
       01  WS-SALBRUTO-HORA-EXTRA      PIC 9(013)V999 VALUES ZEROS.
       01  WS-HEXTRA-CALCULADA         PIC 9(013)V99  VALUES ZEROS.
       01  WS-IMPOSTO-RENDA            PIC 9(013)V99  VALUES ZEROS.
       01  WS-ENCARGOS                 PIC 9(013)V99  VALUES ZEROS.

       PROCEDURE DIVISION.
       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
       0000-PRINCIPAL-FIM.
           EXIT.

      *******************************************************************
      *    INICIALIZACAO DO PROGRAMA                                    *
      *******************************************************************
       1000-INICIALIZAR                SECTION.
      *    SOLICITA AO TRABALHADOR O VALOR DO SALÁRIO BRUTO
           DISPLAY 'POR GENTILEZA, DIGITE O SEU SALARIO BRUTO: '
           ACCEPT WS-SALARIO-BRUTO
      *    SOLICITA AO TRABALHADOR AS HORAS TRABALHADAS
           DISPLAY 'AGORA, POR GENTILEZA, DIGITE AS HORAS TRABALHADAS: '
           ACCEPT WS-HORAS-TRAB
           .
       1000-INICIALIZAR-FIM.
           EXIT.

      *******************************************************************
      *    LOGICA CENTRAL DO PROGRAMA                                   *
      *******************************************************************
       2000-PROCESSAR                  SECTION.
      *    CHAMA A VALIDACAO DA ENTRADA DE INFORMACOES DO TRABALHADOR
           PERFORM 2100-VALIDACAO-INFORMACOES
      *    CHAMA O CALC PARA INCLUSAO DAS HORAS EXTRAS AO SALARIO BRUTO
           PERFORM 2200-CALCULAR-HORA-EXTRA
      *    CHAMA O CALC PARA SABER O QUANTO DE DESCONTO O TRAB TERA
           PERFORM 2300-CALCULO-SALARIO-LIQ
           .
       2000-PROCESSAR-FIM.
           EXIT.

      *******************************************************************
      *    VALIDAR A ENTRADA DE INFORMACOES DO TRABALHADOR              *
      *******************************************************************
       2100-VALIDACAO-INFORMACOES      SECTION.
      *    SE O TRABALHADOR DIGITAR 0 NAS ENTRADAS RECEBE UMA MENSAGEM
           IF WS-SALARIO-BRUTO = 0 OR WS-HORAS-TRAB = 0
               DISPLAY 'VALOR INVALIDO, INSIRA VALORES VALIDOS!'
                   ELSE
      *    CASO ELE DIGITE VALORES MAIORES QUE ZERO, O PROGRAMA SEGUE
                       PERFORM 2200-CALCULAR-HORA-EXTRA
                       PERFORM 2300-CALCULO-SALARIO-LIQ
                       DISPLAY 'O SEU SALARIO LIQUIDO E DE R$: '
                               WS-SALARIO-LIQUIDO
           END-IF.

       2100-VALIDACAO-INFORMACOES-FIM.
           EXIT.
      *******************************************************************
      *    CALCULO PARA INCLUSAO DAS HORAS EXTRAS AO SALARIO BRUTO      *
      *******************************************************************
       2200-CALCULAR-HORA-EXTRA        SECTION.

           IF WS-HORAS-TRAB > 160
      *    CALCULO HORAS EXTRAS TRABALHADAS
               COMPUTE WS-HORA-EXTRA = WS-HORAS-TRAB - 160
      *    CALCULO SALARIO BRUTO DAS HORA EXTRA
               COMPUTE WS-SALBRUTO-HORA-EXTRA = WS-SALARIO-BRUTO / 160
      *    CALCULO SALARIO BRUTO VEZES HORA EXTRA VEZES 50%
               COMPUTE WS-HEXTRA-CALCULADA = WS-HORA-EXTRA
                       * WS-SALBRUTO-HORA-EXTRA * 1,5
      *    CALCULO SALARIO BRUTO COM HORA EXTRA
               COMPUTE WS-SALARIO-LIQUIDO = WS-SALARIO-BRUTO
                       + WS-HEXTRA-CALCULADA
           ELSE
               COMPUTE WS-SALARIO-LIQUIDO = WS-SALARIO-BRUTO
           END-IF.

       2200-CALCULAR-HORA-EXTRA-FIM.
           EXIT.
      *******************************************************************
      *    CALCULO PARA SABER O QUANTO DE DESCONTO O TRABALHADOR TERA   *
      *******************************************************************
       2300-CALCULO-SALARIO-LIQ    SECTION.
      *    SE O SALARIO BRUTO FOR MENOS QUE R$1200,00 NAO FAZ NADA.
           IF WS-SALARIO-BRUTO < 1200
               CONTINUE
      *    SE O SALARIO BRUTO FOR ENTRE R$1200/R$1600 FAZ O CALCULO
               ELSE IF WS-SALARIO-BRUTO >= 1200 AND
                       WS-SALARIO-BRUTO <= 1600
      *    CALCULO DE DESCONTO DE IMPOSTO DE RENDA DE 8%
                   COMPUTE WS-IMPOSTO-RENDA = WS-SALARIO-BRUTO * 0,08
      *    CALCULO DE ENCARGOS DE 5%
                   COMPUTE WS-ENCARGOS = WS-SALARIO-BRUTO * 0,05
      *    CALCULO DE DESCONTO PARA SALARIOS MENORES QUE R$1600,00
                   COMPUTE WS-SALARIO-LIQUIDO = WS-SALARIO-LIQUIDO -
                           WS-IMPOSTO-RENDA - WS-ENCARGOS
      *    SE O SALARIO BRUTO FOR MAIOR QUE R$1601 FAZ ESSE CALCULO
                  ELSE IF WS-SALARIO-BRUTO > 1600
      *    CALCULO DE DESCONTO DE IMPOSTO DE RENDA DE 15%
                      COMPUTE WS-IMPOSTO-RENDA = WS-SALARIO-BRUTO * 0,15
      *    CALCULO DE ENCARGOS DE 7%
                      COMPUTE WS-ENCARGOS = WS-SALARIO-BRUTO * 0,07
      *    CALCULO DE DESCONTO PARA SALARIOS MAIORES QUE R$1600,00
                      COMPUTE WS-SALARIO-LIQUIDO = WS-SALARIO-LIQUIDO -
                               WS-IMPOSTO-RENDA - WS-ENCARGOS
           END-IF.

       2300-CALCULO-SALARIO-LIQ-FIM.
           EXIT.
      *******************************************************************
      *    FINALIZAR PROGRAMA                                           *
      *******************************************************************
       3000-FINALIZAR                  SECTION.
           DISPLAY ' '
           DISPLAY 'TEMOS ORGULHO DE TER VOCE COMO NOSSO COLABORADOR!'
           DISPLAY 'FINALIZANDO O PROGRAMA!'
           STOP RUN
           .
       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM CBLZJP01.
