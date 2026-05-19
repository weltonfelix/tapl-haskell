module TypeChecker where

import AST

import Control.Monad.State
import Control.Monad.Except (throwError)

-- Either is a pre-defined data type in Haskell.
-- It is often used to deal with computations that might fail, and
-- is defined as:
--
-- data Either a b = Left a
--                 | Right b
--
-- Either is also an instance of Monad. Remember, a Monad
-- is a triple (M a, >>=, return), where M a is any parametric
-- type.
--
-- The Either Monad is likely implemented as:
--
-- instance Monad (Either a) where
--   return = Right
--   Left v >>= f = Left v
--   Right v >>= f = f v
--
-- Our design is to benefit from the Either monad to deal with
-- the situation that a type checker might eventually fail.
--
-- In the symply typed lambda calculos, computations might not
-- only fail, but also manipulate a state. In our case, the
-- state is the type environment (or type context); a sequence
-- of tuples (Name, Type).
--
-- Since the type checker deals with two kinds of side effects
-- (errors and state), we can use the monad transformer StateT
-- to combine both the State and Either monads.
--
-- The state monad has the operations 'get' (to get the environment) and
-- 'put' (to update the environment).

type Env = [(Name, Type)]
type Err = Either String

type Res a = StateT Env Err a

checker :: Expr -> Res Type
checker expr = case expr of
  ETrue -> return TBool
  EFalse -> return TBool

  If e1 e2 e3 ->
    checker e1 >>= \t1 ->
    checker e2 >>= \t2 ->
    checker e3 >>= \t3 ->
    if t1 == TBool
    then if t2 == t3 then return t2 else throwError ("then/else branches have different types: " ++ show t2 ++ " vs " ++ show t3)
    else throwError ("condition of if must be Bool, got " ++ show t1)

  Zero -> return TNat
  Succ e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("succ expects Nat, got " ++ show t)
  Pred e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("pred expects Nat, got " ++ show t)
  IsZero e -> checker e >>= \t -> if t == TNat then return TBool else throwError ("isZero expects Nat, got " ++ show t)

  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> throwError ("variable not in scope: " ++ x)
      Just t -> return t

  Abs (x, t1) e -> do
    env <- get             -- obtains the environment from the state
    put $ (x, t1) : env      -- updates the state with a new environment
    t2 <- checker e        -- checker for 'e' in the new environment
    put env                -- restores the environment
    return $ t1 `TArrow` t2

  App e1 e2 -> do
    t1 <- checker e1
    t2 <- checker e2

    case t1 of
      (t11 `TArrow` t12) -> if t2 == t11 then return t12 else throwError ("argument type mismatch: expected " ++ show t11 ++ ", got " ++ show t2)
      _ -> throwError ("expected a function type, got " ++ show t1)

  Let x e1 e2 -> do
    env <- get                   -- salva Γ
    t1  <- checker e1            -- Γ ⊢ t₁ : T₁
    put $ (x, t1) : env          -- Γ' = Γ, x:T₁
    t2  <- checker e2            -- Γ' ⊢ t₂ : T₂
    put env                      -- restaura Γ
    return t2                    -- tipo do let é T₂

  Inl e t -> do
    t1' <- checker e
    case t of
      (t1 `TSum` _t2) -> if t1' == t1 then return t else throwError ("type mismatch: expected " ++ show t1 ++ ", got " ++ show t1')
      _ -> throwError ("expected a sum type, got " ++ show t)
  
  Inr e t -> do
    t2' <- checker e
    case t of
      (_t1 `TSum` t2) -> if t2' == t2 then return t else throwError ("type mismatch: expected " ++ show t2 ++ ", got " ++ show t2')
      _ -> throwError ("expected a sum type, got " ++ show t)

  Case e (xl, el) (xr, er) -> do
    t <- checker e
    case t of
      (tl `TSum` tr) -> do
        env <- get
        put $ (xl, tl) : env
        tl' <- checker el
        put $ (xr, tr) : env
        tr' <- checker er
        put env
        if tl' == tr' then return tl' else throwError ("type mismatch: expected both " ++ show t ++ ", got " ++ show tl' ++ " and " ++ show tr')
      _ -> throwError ("expected a sum type, got " ++ show t)

  -- T-Variant: Γ ⊢ <lj=tj> as <li:Ti> : <li:Ti>
  Tag lj tj varT -> do
    tj' <- checker tj
    case varT of
      TVariant fields ->
        case lookup lj fields of
          Nothing -> throwError ("label " ++ lj ++ " not found in variant type " ++ show varT)
          Just expectedT ->
            if tj' == expectedT
              then return varT
              else throwError ("variant field type mismatch: expected " ++ show expectedT ++ ", got " ++ show tj')
      _ -> throwError ("expected a variant type annotation, got " ++ show varT)

  -- T-Case: all branches must typecheck to the same type T
  CaseVariant e branches -> do
    t <- checker e
    case t of
      TVariant fields -> do
        -- ensure case is exhaustive (same labels as variant type)
        let varLabels  = map fst fields
            caseLabels = map (\(l,_,_) -> l) branches
        if varLabels /= caseLabels
          then throwError ("case branches " ++ show caseLabels ++ " do not match variant labels " ++ show varLabels)
          else do
            env <- get
            branchTypes <- mapM (\(li, xi, ti) ->
              case lookup li fields of
                Nothing -> throwError ("label " ++ li ++ " not in variant")
                Just fieldT -> do
                  put $ (xi, fieldT) : env
                  bt <- checker ti
                  put env
                  return bt
              ) branches
            case branchTypes of
              [] -> throwError "case expression has no branches"
              (bt:bts) ->
                if all (== bt) bts
                  then return bt
                  else throwError ("case branches have different types: " ++ show branchTypes)
      _ -> throwError ("expected a variant type, got " ++ show t)