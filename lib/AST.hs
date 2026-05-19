module AST where

type Name = String

data Expr = ETrue
          | EFalse
          | If {cond :: Expr, exprThen :: Expr, exprElse :: Expr}
          | Zero
          | Succ Expr
          | Pred Expr
          | IsZero Expr
          | Var Name                  -- x               vars in Lambda Calculus
          | Abs (Name, Type) Expr     -- (\x:T . expr)   abstraction in Lambda Calculus
          | App Expr Expr             -- t1 t2           application in Lambda Calculus
          | Inl Expr Type
          | Inr Expr Type
          | Case Expr (Name, Expr) (Name, Expr)
          | Tuple [Expr]
          | Proj Int Expr
          | Let Name Expr Expr
          | Tag Name Expr Type            -- <l=t> as T      (variant tagging)
          | CaseVariant Expr [(Name, Name, Expr)]  -- case t of <li=xi>=>ti  (variant case)
     deriving (Eq, Show)

data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VInl Value Type
           | VInr Value Type
           | VTag Name Value Type         -- <l=v> as T
     deriving (Eq, Show)

data Type = TBool
          | TNat
          | Type `TArrow` Type
          | Type `TSum` Type
          | TVariant [(Name, Type)]       -- <l1:T1, l2:T2, ...>
          | TTuple [Type]
     deriving (Eq, Show)
