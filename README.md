# RAR Framework in R [ ![Codeship Status for aleDsz/rarframeworkR](https://app.codeship.com/projects/8e0ddf10-65b3-0135-2167-6e2b51196e77/status?branch=master)](https://app.codeship.com/projects/240767)

## 1. Introdução

Após ter criado o mesmo framework, originalmente em [PHP](https://github.com/aleDsz/rarframework), percebi que eu teria a mesma necessidade de um ORM em outras linguagens. Assim como eu precisei quando comecei a utilizar o R em ambiente profissional e, com a praticidade que eu tinha em PHP, resolvi adaptar para R.

Mas, o projeto ainda não está 100% completo, devido ao R não ter um suporte total ao POO.


## 2. Como Funciona

Através do pacote DBI, é possível realizar uma conexão com vários tipos de banco de dados. Além disso, por meio do `Generics`, é possível acessar o conteúdo de um objeto e obter todas as informações necessárias para criar uma instrução SQL.

Neste caso, uma classe deve seguir o seguinte modelo:

```R
ClasseTeste <- methods::setRefClass(
    
    # Nome da Tabela
    "nome_do_campo",
    
    # Campos da Tabela
    fields = list(
        # Aqui você precisa informar o tipo do
        # campo no banco de dados, seguindo os tipos de dados do R
        nome_do_campo = "character"
    ) 
)
```

## 3. Como Utilizar

Para que você possa utilizar todos as funcionalidades do framework no seu ambiente, você pode criar 1 (ou mais, dependendo da sua forma de trabalho) classe para acessar ao banco de dados de forma genérica.

```R
ModelDataAccess <- methods::setRefClass(
    "ModelDataAccess",

    methods = list(

        initialize = function() {
            tryCatch({
                databaseFactory <- rarframeworkR:::DatabaseFactory$new()
                databaseFactory$getDataContextInstance()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        create = function(obj) {
            tryCatch({
                sqlStatement   <- rarframeworkR:::SqlStatementInsert$new(obj);
                commandContext <- rarframeworkR:::CommandContext$new(sqlStatement$getSql());
                
                commandContext$executeQuery()
            }, error = function (ex) {
                stop (ex$message)
            })
        },
        
        save = function(obj) {
            tryCatch({
                sqlStatement   <- rarframeworkR:::SqlStatementUpdate$new(obj);
                commandContext <- rarframeworkR:::CommandContext$new(sqlStatement$getSql());
                
                commandContext$executeQuery()
            }, error = function (ex) {
                stop (ex$message)
            })
        },
        
        find = function(obj) {
            tryCatch({
                sqlStatement   <- rarframeworkR:::SqlStatementSelect$new(obj);
                objContext     <- rarframeworkR:::ObjectContext$new(obj);
                commandContext <- rarframeworkR:::CommandContext$new(sqlStatement$getSql(FALSE));
                
                return (objContext$getObject(commandContext$executeReader()))
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        findAll = function(obj) {
            tryCatch({
                sqlStatement   <- rarframeworkR:::SqlStatementSelect$new(obj);
                objContext     <- rarframeworkR:::ObjectContext$new(obj);
                commandContext <- rarframeworkR:::CommandContext$new(sqlStatement$getSql(TRUE));
                
                return (objContext$getObjects(commandContext$executeReader()))
            }, error = function (ex) {
                stop (ex$message)
            })
        },
        
        remove = function(obj) {
            tryCatch({
                sqlStatement   <- rarframeworkR:::SqlStatementDelete$new(obj);
                commandContext <- rarframeworkR:::CommandContext$new(sqlStatement$getSql());
                
                commandContext$executeQuery()
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)
```

**OBS.:** Você não precisa criar a classe de forma genérica, você pode criar uma classe de acesso a dados para cada entidade que você criar no modelo citado acima.

E para que o ORM consiga se conectar com o banco de dados, você precisa criar um arquivo de configuração com o nome: `databaseConfig.json` e ele deve seguir o modelo abaixo:

```json
{
    "host" : "localhost",
    "port" : 3306,
    "user" : "root",
    "pwd"  : "123",
    "db"   : "foo",
    "type" : "mysql"
}
```

**OBS.:** Neste modelo, estamos informando um banco MySQL.

## 4. Como Contribuir

Para contribuir, você pode realizar um **fork** do nosso repositório e nos enviar um Pull Request.

## 5. Doação

Caso queria fazer uma doação para o projeto, você pode realizar [aqui](https://twitch.streamlabs.com/aleDsz)

## 6. Suporte

Caso você tenha algum problema ou uma sugestão, você pode nos contatar [aqui](https://github.com/aleDsz/rarframeworkR/issues).

## 7. Licença

Cheque [aqui](LICENSE)
