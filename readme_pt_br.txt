<div class="moz-text-flowed" style="font-family: -moz-fixed">
      Servidor de banco de dados Leto é um servidor de banco de dados de várias
      plataformas ou um banco de dados com um sistema de gerenciamento.
      Sistema destinado a Programas do Cliente escritos em x[H]arbour,
      poderá trabalhar com arquivos DBF/CDX, localizados em um servidor remoto.

      1. Directory structure

      bin/          -    Arquivo executável do servidor
      doc/          -    documentação
      include/      -    arquivos de cabeçalho de origem
      lib/          -    lib(Biblioteca) Rdd
      source/
          client/   -    fontes de acesso CLIENTE do rdd
          common/   -    Alguns arquivos de origem comuns
          client/   -    fontes de acesso SERVIDOR do Rdd
      tests/        -    Progamas de exemplos de uso do RDD
      utils/
          manage/   -    Utilitario de acesso ao servidor


      2. Criando binários

      2.1 Compilador Borland Win32 C

      Uma variável de ambiente, que define um caminho para o diretório x[H]arbour,
      HB_PATH deve ser definida antes de compilar a LIB.
      Isso pode ser feito, por exemplo, adicionando uma linha em um make_b32.bat ou no PATH:

      SET HB_PATH=c:\harbour

      Se você usar xHarbour, tire comentários de uma linha ' XHARBOUR = yes ' em Makefile.BC.
      Em seguida, executar o make_b32.bat e letodb.exe arquivo executável do servidor
      será exibida em uma bin/diretório e rdd rddleto.lib biblioteca em um lib/diretório.

      2.2 Compilador Linux GNU C

      Uma variável de ambiente, que define um caminho para o diretório Harbour,
      HB_ROOT deve ser definida antes compilando.
      Ou apenas altere o valor de HRB_DIR na Makefile.Linux.

      Em seguida, executar o make_linux.sh e letodb arquivo executável do servidor
      será exibida em uma bin/diretório e rdd librddleto.a biblioteca em um lib/diretório.

      2.3 xHarbour Builder(Comercial)

      Execute o make_xhb.bat para construir binários com este compilador.
      Provavelmente, você precisará alterar o caminho para sua cópia Construtor
      de expressões na make_xhb.bat xHarbour. O valor padrão é:

      set XHB_PATH=c:\xhb

      3. Executando e parando servidor

      Apenas executá-lo:

      letodb.exe                    ( under Windows )
      ./letodb                      ( under Linux )

      Para o desligamento do servidor, execute o arquivo executável mesmo com um parâmetro 'stop'

      letodb.exe stop               ( under Windows )
      ./letodb stop                 ( under Linux )

      4. Configuração do servidor

      4.1 letodb.ini

      Você pode fornecer configuração arquivo letodb.ini se você não estiver
      satisfeito com valores parâmetros padrão.
      No momento existem os seguintes parâmetros (padrão os valores são designados):

      Port = 2812              -    Numero da porta do servidor
      DataPath = \dados        -    Um caminho para um diretório de dados(DBF) em um servidor, esse caminho é valido apartir de onde o server do letoDB esta iniciando
      LogPath = "letodb.log"   -    caminho e o nome de um arquivo de log, nesse exemplo esta criando no mesmo diretorio onde o letodb.exe esta iniciando
      Default_Driver = CDX     -    Padrão RDD para abrir arquivos no servidor ( CDX/NTX ) o Padrão é CDX
      Lower_Path = 0           -    Caso seja definido como 1, vai converter todos os caminhos para minúsculas
      EnableFileFunc = 0            Caso seja definido como 1, vai Ativar o uso de funções como( leto_file(),leto_ferase(), leto_frename() )
      EnableAnyExt = 0         -    Caso seja definido como 1, é Ativado a criação de arquivos de dados(DBF) e índices com qualquer extensão, com exceção do padrão (DBF, CDX, NTX)
      Pass_for_Login = 0       -    Caso seja definido como 1, a autenticação do usuário é necessária para acessar o servidor
      Pass_for_Manage = 0      -    Caso seja definido como 1, a autenticação do usuário é necessária para usar funções de gestão (Leto_mggetinfo (), etc)
      Pass_for_Data = 0        -    Caso seja definido como 1, a autenticação do usuário é necessário ter acesso de escrita aos dados
      Pass_File = "leto_users" -    o caminho e o nome do arquivo com as informações dos usuários
      Crypt_Traffic = 0        -    Caso seja definido como 1, passa os dados pela a rede cripitografado

      É possível definir estrutura [DATABASE] (Banco de dados) se você precisa ter
      um diretório, onde arquivos são abertos por outro RDD:

      [DATABASE]
      DataPath =               -    (Opção obrigatória)
      Driver = CDX             -    ( CDX/NTX )

      Você pode definir quantas seções [DATABASE], conforme necessário.

      No Ambiente Windows o letodb.ini deve ser colocado em um diretório,
      de onde servidor é iniciado.

      No Linux o programa procura-lo no diretório de onde o servidor
      é iniciado e se não tiver sucesso, vai procurar no diretório /etc.


      4.2 Autenticação

      Para ativar autenticação do subsistema você precisa configurar um dos seguintes parâmetros em
      letodb.ini a 1º é: Pass_for_Login, Pass_for_Manage, Pass_for_Data.
      Mas antes você precisa criar pelo menos um usuário com direitos administrador, porque quando autenticação
      subsistema funcionar, apenas usuários autenticados com admin direitos são capazes de acrescentar / alterar os usuários
      e as senhas.
      Para adicionar um usuário, você precisará incluir uma chamada de LETO_USERADD () no seu lado cliente
      programa, por exemplo:
      
      LETO_USERADD( "admin", "secret:)", "YYY" )

      Onde "YYY" é uma STRING, o que confere direitos de administrador, gerenciar e escrever acesso. Você pode
      também utilizar o utils/manager/console.prg programa para definir ou alterar dados de autenticação.

      Para se conectar a um servidor com uma autenticação de dados (nome de usuário e senha), você precisará
      utilização da Função LETO_CONNECT()
      Abaixo um exemplo de Conexão:

      FUNCTION MAIN()
      IF ( leto_Connect( "//192.168.243.16/","USUARIO","SENHA" ) ) == -1
         nRes := leto_Connect_Err()
         IF nRes == LETO_ERR_LOGIN
            ? "Falha ao Logar"
         ELSEIF nRes == LETO_ERR_RECV
            ? "Error ao conectar"
         ELSEIF nRes == LETO_ERR_SEND
            ? "Erro de envio"
         ELSE
            ? "Não connectado"
         ENDIF

         Return Nil
      ENDIF
      ? "Conectado em " + leto_GetServerVersion()
      INKEY(0)
      leto_DisConnect()
      RETURN
  
      5. A conexão com o servidor a partir de programas do cliente.

      Para poder se conectar ao servidor você precisará vincular a rddleto.lib (Windows)
      ou librddleto.a (Linux) e no seu Aplicativo ten que adicionar no início do seu .PRG
      principal duas linhas:

      REQUEST LETO
      RDDSETDEFAULT( "LETO" )

      Para abrir um arquivo DBF em um servidor, você precisará escrever em uma
      instrução SET PATH TO ou no comando use diretamente um caminho para
      o servidor em um formulário padrão //ip_address:Port/data_path/Nome_Arquivo.

      Se um parâmetro 'datapath' de um arquivo de configuração do servidor é definido
      como um valor não vazio, você precisará designar não o caminho completo
      para um arquivo no servidor, mas somente uma relativa (relativamente a ' Datapath').

      Por exemplo, se você precisar abrir um arquivo Test.dbf, que está localizado
      no valor de parâmetro (de um arquivo de configuração do servidor letodb.ini)
      Servidor 192.168.5.22 em um diretório /dados Mydir e o 'datapath' é ' / Dados ',
      a sintaxe será o seguinte:

      USE "//192.168.5.22:2812/mydir/test"

      Se o servidor não executa ou você escrever um caminho errado, você terá erros de execução.
      É possível verificar acessibilidade de um servidor antes de abrir arquivos usando
      a função leto_Connect (cAddress), que retorna "-1" no caso de tentativa sem êxito:

      Se o servidor não executar ou você escreve um caminho errado, você terá erros de execução.
      É possível verificar a acessibilidade de um servidor antes de abrir arquivos usando
      a função leto_Connect (cAddress) , que retorna -1 no caso de tentativa sem êxito:

      Exemplo:
      IF leto_Connect( "//192.168.5.22:2812/mydir/" ) == -1
         Alert( "Can't connect to server ..." )
      ENDIF

      6. Utilitário de gerenciamento.

      Há dois utilitários de gerenciamento, interface gráfica com o usuário
      e do console, as fontes estão no diretório utils/manage.

      O utilitário GUI, Manage.prg, é feita com a biblioteca HwGUI.
      Se tiver HwGUI, basta escrever na linha ' set HWGUI_INSTALL= ""
      do arquivo utils/Manage/BLD.bat um caminho para o diretório HwGUI
      e executar o BLD.bat, ele criará Manage.exe para você.

      Para aqueles, que não usam HwGUI, há um utilitário Modo de console,
      console.prg. Para criar um console.exe, você pode usar para criar programas
      o hbmake do x[H]arbour e o hbmk2 no Harbour, você precisa apenas adicionar rddleto.lib à lista de bibliotecas externas.
      Execute o console.exe com um servidor Nome ou IP e porta como um parâmetro:

      Exexmplos:
      console.exe server_name:nPort
      console.exe ip_address:nPort
      console.exe 192.168.254.17:2812

      O nome_do_servidor e endereço_IP em uma linha de comando devem
      ser sem zeros à esquerda barras ('//'), pois interpreta Clipper/Harbour
      de maneira especial.

      7. Algumas observações extras:
      - O letodb.exe que vai rodar como um serviço no servidor usando a porta 2812, então tem
      que liberar essa porta no modem do servidor dando redirecionamento para o IP do servidor LETODB.
      Esse letodb.exe pode rodar de qualquer lugar ou seja qualquer pasta do servidor.
      - Ao finalizar o Aplicativo deve encerrar a conexão com LETO_DISCONNECT()
      - Incluir em Pontos estratégico do sistema o uso de leto_BeginTransaction(), leto_Rollback() e
      leto_CommitTransaction() na pasta \letodb\tests\ tem exemplo de uso.


Nota: Texto originalmente escrito por Alexander Kresin e traduzido por Leonardo Machado </div>