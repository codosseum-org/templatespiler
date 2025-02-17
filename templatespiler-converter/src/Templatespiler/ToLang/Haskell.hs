{-# LANGUAGE LambdaCase #-}

module Templatespiler.ToLang.Haskell where

import Data.Containers.ListUtils
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Language.Templatespiler.Syntax (Binding, BindingList)
import Language.Templatespiler.Syntax as IR
import Templatespiler.Convert.ToImperative (identToVarName)
import Templatespiler.IR.Common
import Templatespiler.ToLang.Monad (ToLang, ToLangT, runToLang)

data Expression
  = Int Int
  | Float Float
  | String Text
  | Var VarName
  | Tuple [Expression]
  | List [Expression]
  | Do (NonEmpty Statement)
  | -- | Read a line from stdin, with an optional type annotation
    ReadLn (Maybe Text)
  | Read (Maybe Text) Expression
  | GetLine
  | ReplicateM Expression Expression
  | LetIn Text Expression Expression
  | Expression :<$>: Expression
  | Pure Expression
  deriving stock (Show)

data Statement
  = LiftExpr Expression
  | Bind Binder Expression
  | Let VarName Expression
  deriving stock (Show)

data Binder
  = NamedBinder VarName
  | ListBinder [Binder]
  | TupleBinder [Binder]
  deriving stock (Show)

-- | A haskell program is just a single expression
type HaskellProgram = Expression

data ToHaskellWarning
type HaskellWriter a = ToLang [ToHaskellWarning] a

toHaskell :: BindingList -> (HaskellProgram, [ToHaskellWarning])
toHaskell (BindingList bindings) = runToLang $ do
  decls <- traverse bindingToHaskell bindings
  -- add a pure () at the end to make sure the program compiles
  pure $ Do (fmap snd decls <> (LiftExpr (Pure $ Tuple []) :| []))

bindingToHaskell :: Binding -> HaskellWriter (Maybe Binder, Statement)
bindingToHaskell (Binding name t) = do
  let varName = identToVarName name
  (bind, expr) <- readBindingExpr t

  let binder = fromMaybe (NamedBinder varName) bind
  pure $ (Just binder, Bind binder expr)

bindingOrCombinatorToHaskell :: IR.BindingOrCombinator -> HaskellWriter (Maybe Binder, Expression)
bindingOrCombinatorToHaskell (NamedBinding b) =
  bindingToHaskell b >>= \case
    (_, Bind name expr) -> pure (Just name, expr)
    _ -> error "impossible"
bindingOrCombinatorToHaskell (UnnamedBinding c) = combinatorVarToHaskell c
bindingOrCombinatorToHaskell (GroupBinding (BindingList bindings)) = do
  (binds, decls) <- NE.unzip <$> traverse bindingToHaskell bindings
  let justBinds = catMaybes $ toList binds
  case justBinds of
    [] -> pure (Nothing, Do decls)
    x : xs -> let asTuple = binderNames AsTuple (x :| xs) in pure (Just asTuple, Do decls)

readBindingExpr :: IR.Type -> HaskellWriter (Maybe Binder, Expression)
readBindingExpr (TerminalType IntType) = pure (Nothing, ReadLn (Just "Int"))
readBindingExpr (TerminalType FloatType) = pure (Nothing, ReadLn (Just "Float"))
readBindingExpr (TerminalType StringType) = pure (Nothing, GetLine)
readBindingExpr (CombinatorType c) = combinatorVarToHaskell c

{- | Defines an expression specifically to /parse/ a binding from an existing expression
This is useful for when we want to parse a binding from a larger expression
For example, 'parseBindingExpr (Var "foo") (TerminalType IntType)' generates the expression 'read @Int foo'
-}
parseBindingExpr :: Expression -> IR.Type -> Expression
parseBindingExpr expr (TerminalType IntType) = Read (Just "Int") expr
parseBindingExpr expr (TerminalType FloatType) = Read (Just "Float") expr
parseBindingExpr expr (TerminalType StringType) = expr
parseBindingExpr expr (CombinatorType c) = error $ "TODO: " <> show c

combinatorVarToHaskell :: IR.Combinator -> HaskellWriter (Maybe Binder, Expression)
combinatorVarToHaskell (NamedCombinator newName c) = do
  (oldName, r) <- readBindingExpr c
  pure (Just $ NamedBinder (identToVarName newName), r)
combinatorVarToHaskell (ArrayCombinator len b) = do
  let lenExpr = Int len
  (bind, bExpr) <- bindingOrCombinatorToHaskell b
  pure (bind, ReplicateM lenExpr bExpr)
combinatorVarToHaskell (ListCombinator b) = do
  -- we first have to emit a line to read the count
  let countBind = Bind (NamedBinder "count") (ReadLn (Just "Int"))
  (bind, bExpr) <- bindingOrCombinatorToHaskell b

  {- now we want to generate something like ```
  count <- readLn @Int
  res <- replicateM count (do
    ...
  )
  ```
  -}
  let replicateMExpr = ReplicateM (Var "count") bExpr
  pure (bind, Do $ countBind :| [LiftExpr replicateMExpr])
combinatorVarToHaskell (SepByCombinator " " (BindingList binders)) = do
  let getWords = Var "words" :<$>: GetLine

  -- there is now a special case where we can either use the result of `words <$> getLine` directly, or apply a `fmap read`
  -- to it, if the binder is all the same terminal type
  -- for example, (sep-by " " Int) can be simply translated to (read <$> words <$> getLine)
  -- even simpler, (sep-by " " String) can be translated to (words <$> getLine)

  let listBinder = bindingNames AsList binders
  case nubOrd (bindingType <$> toList binders) of
    [TerminalType StringType] -> pure (Just listBinder, getWords)
    [TerminalType IntType] -> pure (Just listBinder, Var "read" :<$>: getWords)
    [TerminalType FloatType] -> pure (Just listBinder, Var "read" :<$>: getWords)
    _different -> do
      -- if they're different, we have to write a multi-liner:
      -- 1 - read the line :  [bind1, bind2, ...] <- words <$> getLine
      -- 2 - for each input, parse it:
      --  let bind1 = read @T1 input1
      --  let bind2 = read @T2 input2
      --  ...

      let wordsLine = Bind listBinder (Var "words" :<$>: GetLine)
      let (names, lets) =
            NE.unzip
              ( fmap
                  ( \(Binding name t) ->
                      (identToVarName name, Let (identToVarName name) (parseBindingExpr (Var $ identToVarName name `withSuffix` "tmp") t))
                  )
                  binders
              )
      -- and now return the values in a tuple
      let pure' = LiftExpr $ Pure (Tuple (Var <$> toList names))
      pure (Just (ListBinder (NamedBinder <$> toList names)), Do $ wordsLine <| lets <> (pure' :| []))

data BindingNamesAs = AsTuple | AsList
bindingNames :: BindingNamesAs -> NonEmpty Binding -> Binder
bindingNames as bindings =
  let names = fmap (\(Binding name _) -> identToVarName name `withSuffix` "tmp") bindings
   in case names of
        x :| [] -> NamedBinder x
        _names -> case as of
          AsTuple -> TupleBinder (NamedBinder <$> toList names)
          AsList -> ListBinder (NamedBinder <$> toList names)

binderNames :: BindingNamesAs -> NonEmpty Binder -> Binder
binderNames as binders =
  let allNames binder = case binder of
        NamedBinder name -> [name]
        ListBinder names -> concatMap allNames names
        TupleBinder names -> concatMap allNames names
   in let names = concatMap allNames binders
       in case names of
            [x] -> NamedBinder x
            _names -> case as of
              AsTuple -> TupleBinder (NamedBinder <$> names)
              AsList -> ListBinder (NamedBinder <$> names)