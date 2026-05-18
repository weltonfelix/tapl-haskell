module Main (main) where

import Test.HUnit
import Control.Monad.State (evalStateT)
import AST
import TypeChecker

run :: Expr -> Either String Type
run e = evalStateT (checker e) []

-- Bool literals
testTrue :: Test
testTrue = TestCase $ assertEqual "ETrue has type TBool" (Right TBool) (run ETrue)

testFalse :: Test
testFalse = TestCase $ assertEqual "EFalse has type TBool" (Right TBool) (run EFalse)

-- Nat literals
testZero :: Test
testZero = TestCase $ assertEqual "Zero has type TNat" (Right TNat) (run Zero)

testSuccZero :: Test
testSuccZero = TestCase $ assertEqual "Succ Zero has type TNat" (Right TNat) (run (Succ Zero))

testPredSuccZero :: Test
testPredSuccZero = TestCase $ assertEqual "Pred (Succ Zero) has type TNat" (Right TNat) (run (Pred (Succ Zero)))

testSuccNested :: Test
testSuccNested = TestCase $ assertEqual "Succ (Succ Zero) has type TNat" (Right TNat) (run (Succ (Succ Zero)))

-- IsZero
testIsZeroZero :: Test
testIsZeroZero = TestCase $ assertEqual "IsZero Zero has type TBool" (Right TBool) (run (IsZero Zero))

testIsZeroSucc :: Test
testIsZeroSucc = TestCase $ assertEqual "IsZero (Succ Zero) has type TBool" (Right TBool) (run (IsZero (Succ Zero)))

-- If expressions (well-typed)
testIfBoolBranches :: Test
testIfBoolBranches = TestCase $
  assertEqual "if true then true else false : TBool"
    (Right TBool)
    (run (If ETrue ETrue EFalse))

testIfNatBranches :: Test
testIfNatBranches = TestCase $
  assertEqual "if false then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If EFalse Zero (Succ Zero)))

testIfCondIsZero :: Test
testIfCondIsZero = TestCase $
  assertEqual "if iszero 0 then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If (IsZero Zero) Zero (Succ Zero)))

-- If expressions (ill-typed)
testIfNonBoolCond :: Test
testIfNonBoolCond = TestCase $
  case run (If Zero ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testIfBranchMismatch :: Test
testIfBranchMismatch = TestCase $
  case run (If ETrue Zero EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Succ / Pred on non-Nat (ill-typed)
testSuccBool :: Test
testSuccBool = TestCase $
  case run (Succ ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testPredBool :: Test
testPredBool = TestCase $
  case run (Pred EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- IsZero on non-Nat (ill-typed)
testIsZeroBool :: Test
testIsZeroBool = TestCase $
  case run (IsZero ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Var
testVarUnbound :: Test
testVarUnbound = TestCase $
  case run (Var "x") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Abs (well-typed)
testAbsIdentityBool :: Test
testAbsIdentityBool = TestCase $
  assertEqual "\\x:Bool. x : Bool -> Bool"
    (Right (TBool `TArrow` TBool))
    (run (Abs ("x", TBool) (Var "x")))

testAbsIdentityNat :: Test
testAbsIdentityNat = TestCase $
  assertEqual "\\x:Nat. x : Nat -> Nat"
    (Right (TNat `TArrow` TNat))
    (run (Abs ("x", TNat) (Var "x")))

testAbsConstant :: Test
testAbsConstant = TestCase $
  assertEqual "\\x:Bool. zero : Bool -> Nat"
    (Right (TBool `TArrow` TNat))
    (run (Abs ("x", TBool) Zero))

testAbsNested :: Test
testAbsNested = TestCase $
  assertEqual "\\x:Bool. \\y:Nat. x : Bool -> Nat -> Bool"
    (Right (TBool `TArrow` (TNat `TArrow` TBool)))
    (run (Abs ("x", TBool) (Abs ("y", TNat) (Var "x"))))

-- App (well-typed)
testAppIdentityBool :: Test
testAppIdentityBool = TestCase $
  assertEqual "(\\x:Bool. x) true : TBool"
    (Right TBool)
    (run (App (Abs ("x", TBool) (Var "x")) ETrue))

testAppIdentityNat :: Test
testAppIdentityNat = TestCase $
  assertEqual "(\\x:Nat. x) zero : TNat"
    (Right TNat)
    (run (App (Abs ("x", TNat) (Var "x")) Zero))

testAppReturnsBool :: Test
testAppReturnsBool = TestCase $
  assertEqual "(\\x:Nat. isZero x) zero : TBool"
    (Right TBool)
    (run (App (Abs ("x", TNat) (IsZero (Var "x"))) Zero))

-- App (ill-typed)
testAppNotAFunction :: Test
testAppNotAFunction = TestCase $
  case run (App ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testAppArgMismatch :: Test
testAppArgMismatch = TestCase $
  case run (App (Abs ("x", TBool) (Var "x")) Zero) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Inl (well-typed)
testInl :: Test
testInl = TestCase $
  assertEqual "inl zero as Nat + Bool : Nat + Bool"
    (Right (TNat `TSum` TBool))
    (run (Inl Zero (TNat `TSum` TBool)))

-- Inl (ill-typed)
testInlTypeMismatch :: Test
testInlTypeMismatch = TestCase $
  case run (Inl ETrue (TNat `TSum` TBool)) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type mismatch error, got " ++ show t)

-- Inr (well-typed)
testInr :: Test
testInr = TestCase $
  assertEqual "inr true as Nat + Bool : Nat + Bool"
    (Right (TNat `TSum` TBool))
    (run (Inr ETrue (TNat `TSum` TBool)))

-- Inr (ill-typed)
testInrTypeMismatch :: Test
testInrTypeMismatch = TestCase $
  case run (Inr Zero (TNat `TSum` TBool)) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type mismatch error, got " ++ show t)

-- Case (well-typed)
testCase :: Test
testCase = TestCase $
  assertEqual "case inl zero as Nat + Bool of inl x => x | inr y => false : Nat"
    (Right TNat)
    (run (Case (Inl Zero (TNat `TSum` TBool))
              ("x", Var "x")
              ("y", Zero)))

-- Case (ill-typed)
testCaseNotASum :: Test
testCaseNotASum = TestCase $
  case run (Case Zero
              ("x", Var "x")
              ("y", Zero)) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testCaseTypeMismatch :: Test
testCaseTypeMismatch = TestCase $
  case run (Case (Inl Zero (TNat `TSum` TBool))
              ("x", Var "x")
              ("y", ETrue)) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type mismatch error, got " ++ show t)

-- Let (well-typed)
testLetNat :: Test
testLetNat = TestCase $
  assertEqual "let x = zero in succ x : TNat"
    (Right TNat)
    (run (Let "x" Zero (Succ (Var "x"))))

testLetBool :: Test
testLetBool = TestCase $
  assertEqual "let x = true in x : TBool"
    (Right TBool)
    (run (Let "x" ETrue (Var "x")))

testLetShadow :: Test
testLetShadow = TestCase $
  assertEqual "let x = zero in let x = true in x : TBool"
    (Right TBool)
    (run (Let "x" Zero (Let "x" ETrue (Var "x"))))

-- Let (ill-typed)
testLetBodyError :: Test
testLetBodyError = TestCase $
  case run (Let "x" ETrue (Succ (Var "x"))) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testLetScopeLeaks :: Test
testLetScopeLeaks = TestCase $
  case run (App (Let "x" ETrue (Var "x")) (Var "x")) of
    Left _  -> return ()
    Right t -> assertFailure ("x should not be in scope outside let, got " ++ show t)

tests :: Test
tests = TestList
  [ TestLabel "ETrue"                testTrue
  , TestLabel "EFalse"               testFalse
  , TestLabel "Zero"                 testZero
  , TestLabel "Succ Zero"            testSuccZero
  , TestLabel "Pred (Succ Zero)"     testPredSuccZero
  , TestLabel "Succ (Succ Zero)"     testSuccNested
  , TestLabel "IsZero Zero"          testIsZeroZero
  , TestLabel "IsZero (Succ Zero)"   testIsZeroSucc
  , TestLabel "If bool branches"     testIfBoolBranches
  , TestLabel "If nat branches"      testIfNatBranches
  , TestLabel "If iszero cond"       testIfCondIsZero
  , TestLabel "If non-bool cond"     testIfNonBoolCond
  , TestLabel "If branch mismatch"   testIfBranchMismatch
  , TestLabel "Succ Bool"            testSuccBool
  , TestLabel "Pred Bool"            testPredBool
  , TestLabel "IsZero Bool"          testIsZeroBool
  , TestLabel "Var unbound"          testVarUnbound
  , TestLabel "Abs identity Bool"    testAbsIdentityBool
  , TestLabel "Abs identity Nat"     testAbsIdentityNat
  , TestLabel "Abs constant"         testAbsConstant
  , TestLabel "Abs nested"           testAbsNested
  , TestLabel "App identity Bool"    testAppIdentityBool
  , TestLabel "App identity Nat"     testAppIdentityNat
  , TestLabel "App returns Bool"     testAppReturnsBool
  , TestLabel "App not a function"   testAppNotAFunction
  , TestLabel "App arg mismatch"     testAppArgMismatch
  , TestLabel "Inl"     testInl
  , TestLabel "Inl type mismatch"    testInlTypeMismatch
  , TestLabel "Inr"     testInr
  , TestLabel "Inr type mismatch"    testInrTypeMismatch
  , TestLabel "Case"    testCase
  , TestLabel "Case not a sum"    testCaseNotASum
  , TestLabel "Case type mismatch"    testCaseTypeMismatch
  , TestLabel "Let Nat"              testLetNat
  , TestLabel "Let Bool"             testLetBool
  , TestLabel "Let shadow"           testLetShadow
  , TestLabel "Let body error"       testLetBodyError
  , TestLabel "Let scope leaks"      testLetScopeLeaks
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result > 0
    then fail "Some tests failed."
    else return ()
