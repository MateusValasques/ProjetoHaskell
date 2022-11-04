module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.Environment

type Saldo = Double


opsaldo :: Saldo -> IO Saldo
opsaldo saldo = do putStr "Saldo:"
                   print saldo
                   return saldo

deposito :: Saldo -> IO Saldo
deposito saldo = do putStr "Valor:"
                    valor <- readLn
                    let deposito = valor::Saldo
                    if deposito < 0
                    then do putStrLn "Valor invalido"
                            return saldo
                    else return (saldo + deposito)

saque :: Saldo -> IO Saldo
saque saldo = do putStr "Valor:"
                 valor <- readLn
                 let saque = valor::Saldo
                 if saque < 0
                 then do putStrLn "Valor invalido"
                         return saldo
                 else
                     if saldo - saque < 0
                     then do putStrLn "Saldo insuficiente"
                             return saldo
                     else return (saldo - saque)

menu :: Saldo -> IO Saldo
menu saldo = do putStrLn "Simulação"
                putStrLn "1- Saldo\n 2- Deposito\n 3- Saque\n 4- Encerrar\n"
                putStrLn "Escolha uma opção: "
                opcao <- getLine

                if opcao == "1"
                    then do opsaldo saldo
                            menu saldo
                else
                    if opcao == "2"
                    then do saldo <- deposito saldo
                            menu saldo
                    else

                        if opcao == "3"
                            then do saldo <- saque saldo
                                    menu saldo
                        else
                            if opcao == "4"
                                then do putStrLn "Agradecemos pela preferência"
                                        args <- getArgs
                                        writeFile (args !! 0) (show saldo)
                                        return 0
                                else do putStrLn "Erro, Opcao Incorreta"
                                        menu saldo


main :: IO()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Simulacao de caixa automatico"
          args <- getArgs
          let arq = (args !! 0)
          arquivo <- readFile arq
          if arquivo == ""
            then menu 0.0
                else do let saldo = read(arquivo)::Saldo
                        menu saldo
          return ()
