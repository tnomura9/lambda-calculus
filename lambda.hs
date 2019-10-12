import Data.Maybe
import Data.IORef
import qualified Data.Map as Map
import Text.Parsec
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Identity
import qualified Control.Exception as Exc

type Name = String
data Term = Var Name
          | Abs Name Term
          | App Term Term
          | Bind Name Term
          deriving (Show)

type Env = Map.Map String Val

data Val = Atom String
         | FunVal Env String Term
         | Default Val Val
         deriving (Show)

evalTerm :: Env -> Term -> Eval0 Val
evalTerm env (Var x) = case Map.lookup x env of
                     Just val -> return val
                     Nothing -> return (Atom x)
evalTerm env (Abs s t) = return $ FunVal env s t
evalTerm env (App t1 t2) = do
                     v1 <- evalTerm env t1
                     v2 <- evalTerm env t2
                     case v1 of
                       FunVal env' s body -> evalTerm (Map.insert s v2 env') body
                       _ -> return $ Default v1 v2

type Eval0 a = Identity a

runEval0 :: Eval0 a -> a
runEval0 ev = runIdentity ev

parseTerm :: String -> Either ParseError Term
parseTerm input = parse term "lambda" input
  where
    term = var <|> abstr <|> appl <|> bind
    var = try (do
       x <- many1 alphaNum <* spaces
       return (Var x))
    abstr = try (do
       string "(l"
       x <- many1 alphaNum <* string "."
       e1 <- term
       string ")" <* spaces
       return (Abs x e1))
    appl = try (do
       string "(" <* spaces
       e1 <- term
       e2 <- term
       string ")" <* spaces
       return (App e1 e2))
    bind = try (do
       string "(=" <* spaces
       x <- many1 alphaNum <* spaces
       e1 <- term
       string ")" <* spaces
       return (Bind x e1))

unparse :: Val -> String
unparse (Atom x) = x
unparse (FunVal e x t) = "(l" ++ x ++ "." ++ (unparse (runEval0 (evalTerm e t))) ++ ")"
unparse (Default v1 v2) = "(" ++ (unparse v1) ++ " " ++ (unparse v2) ++ ")"

loadFile :: IORef Env -> InputT IO ()
loadFile ioref = do
  fname <- getInputLine "filename: "
  case fname of
    Just fnm -> lift $ bindAll ioref fnm
    Nothing -> outputStrLn "error"

loadTerm :: String -> IO [Either ParseError Term]
loadTerm fname = map parseTerm <$> lines <$> readFile fname

bindOneTerm :: IORef Env -> Either ParseError Term -> IO ()
bindOneTerm ioref tm = do
  env <- readIORef ioref
  case tm of
    Right (Bind x e) -> do
      val <- return $ runEval0 $ evalTerm env e
      writeIORef ioref (Map.insert x val env)
    Right exp -> do print exp; print "not written"
    Left err -> print err

bindAll :: IORef Env -> String -> IO ()
bindAll ioref fname = do
  Exc.catch ( do
    binds <- loadTerm fname
    mapM_ (bindOneTerm ioref) binds
    return ()
    ) $ \(SomeException e) -> print e

main :: IO ()
main = do
  global <- newIORef Map.empty
  runInputT defaultSettings (loop global)
    where
      loop :: (IORef (Map.Map Name Val)) -> InputT IO ()
      loop glb = do
        minput <- getInputLine "lambda> "
        case minput of
          Nothing -> return ()
          Just ":quit" -> return ()
          Just ":load" -> do loadFile glb; loop glb
          Just input -> do
            env <- lift $ readIORef glb
            expr <- return $ parseTerm input
            case expr of
              Right (Bind x e) -> do
                let val = runEval0 $ evalTerm env e
                lift $ writeIORef glb (Map.insert x val env)
              Right exp ->
                outputStrLn $ unparse $ runEval0 $ evalTerm env exp
              Left err ->
                outputStrLn $ show err
            loop glb
