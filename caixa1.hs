module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.Environment
                              

main :: IO()
main = do hSetBuffering stdout NoBuffering
    if menu saldo != 0 then do menu saldo
    return ()
    
opsaldo :: Double -> Double
opsaldo saldo = do putStr "Saldo:" 
                    print saldo
                    return saldo

deposito :: Double -> Double
deposito saldo = do putStr "Valor:" 
                    valor <- readLn
                    let deposito = valor::Double
                    if deposito < 0
                        then do putStrLn "Valor invalido."
                            return saldo
                    else return (saldo + deposito)

saque :: Double -> Double
saque saldo = do putStr "Valor:" 
                valor <- readLn
                let saque = valor::Double
                if saque < 0
                then do putStrLn "Valor invalido."
                        return saldo
                else 
                    if saldo - saque < 0
                        then do putStrLn "Saldo insuficiente"
                            return saldo
                    else return (saldo - saque)

menu :: Double -> Double
menu saldo = do putStrLn "Simulação"
                 
                putStrLn "1- Saldo\n 2- Deposito\n 3- Saque\n 4- Encerrar\n"
                putStrLn "Escolha uma opção: "
                opcao <- getLine

                if opcao == "1" 
                    then do opsaldo saldo
                            menu 1
                         
                if opcao == "2" 
                then do saldo <- deposito saldo
                        menu saldo
                    
                if opcao == "3" 
                    then do saldo <- saque saldo
                            menu saldo
                            
                if opcao == "4"
                    then do putStrLn "Encerrado"
                            
                            return 0
                else do putStrLn "Erro"
                        menu saldo
