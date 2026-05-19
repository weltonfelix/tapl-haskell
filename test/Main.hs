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

-- <physical=zero> as <physical:Nat, virtual:Bool>  ->  <physical:Nat, virtual:Bool>
testTagWellTyped :: Test
testTagWellTyped = TestCase $
  assertEqual "<physical=zero> as <physical:Nat, virtual:Bool>"
    (Right (TVariant [("physical", TNat), ("virtual", TBool)]))
    (run (Tag "physical" Zero (TVariant [("physical", TNat), ("virtual", TBool)])))

-- <virtual=true> as <physical:Nat, virtual:Bool> -> <physical:Nat, virtual:Bool>
testTagSecondLabel :: Test
testTagSecondLabel = TestCase $
  assertEqual "<virtual=true> as <physical:Nat, virtual:Bool>"
    (Right (TVariant [("physical", TNat), ("virtual", TBool)]))
    (run (Tag "virtual" ETrue (TVariant [("physical", TNat), ("virtual", TBool)])))

-- <physical=true> as <physical:Nat, ...> -> type mismatch 
testTagTypeMismatch :: Test
testTagTypeMismatch = TestCase $
  case run (Tag "physical" ETrue (TVariant [("physical", TNat), ("virtual", TBool)])) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type mismatch, got " ++ show t)

-- label not present in the variant type
testTagUnknownLabel :: Test
testTagUnknownLabel = TestCase $
  case run (Tag "unknown" Zero (TVariant [("physical", TNat)])) of
    Left _  -> return ()
    Right t -> assertFailure ("expected label-not-found error, got " ++ show t)

-- annotation is not a variant type
testTagNotVariantAnnotation :: Test
testTagNotVariantAnnotation = TestCase $
  case run (Tag "l" Zero TNat) of
    Left _  -> return ()
    Right t -> assertFailure ("expected error: annotation is not a variant type, got " ++ show t)

-- case (<none=unit> as <none:Unit, some:Nat>) of
--   <none=u> => false
--   <some=v> => iszero v
-- → TBool
testCaseVariantWellTyped :: Test
testCaseVariantWellTyped = TestCase $
  let optNat = TVariant [("none", TBool), ("some", TNat)]
      scrutinee = Tag "none" EFalse optNat
  in assertEqual "case <none=false> as <none:Bool, some:Nat> of <none=u>=>u | <some=v>=>iszero v"
    (Right TBool)
    (run (CaseVariant scrutinee
           [ ("none", "u", Var "u")
           , ("some", "v", IsZero (Var "v"))
           ]))

-- branches return different types → error
testCaseVariantBranchMismatch :: Test
testCaseVariantBranchMismatch = TestCase $
  let optNat = TVariant [("none", TBool), ("some", TNat)]
      scrutinee = Tag "none" EFalse optNat
  in case run (CaseVariant scrutinee
                [ ("none", "u", Var "u")       -- TBool
                , ("some", "v", Var "v")        -- TNat
                ]) of
    Left _  -> return ()
    Right t -> assertFailure ("expected branch mismatch error, got " ++ show t)

-- scrutinee is not a variant type
testCaseVariantNotVariant :: Test
testCaseVariantNotVariant = TestCase $
  case run (CaseVariant Zero [("l", "x", Var "x")]) of
    Left _  -> return ()
    Right t -> assertFailure ("expected error: not a variant, got " ++ show t)

-- case labels don't match variant labels
testCaseVariantLabelMismatch :: Test
testCaseVariantLabelMismatch = TestCase $
  let varT = TVariant [("a", TNat), ("b", TBool)]
      scrutinee = Tag "a" Zero varT
  in case run (CaseVariant scrutinee
                [ ("a", "x", Var "x")
                , ("wrong", "y", ETrue)
                ]) of
    Left _  -> return ()
    Right t -> assertFailure ("expected label mismatch error, got " ++ show t)
-- Tuples (well-typed)
testTuple :: Test
testTuple = TestCase $
  assertEqual "{true,0,false}"
    (Right (TTuple [TBool, TNat, TBool]))
    (run (Tuple [ETrue, Zero, EFalse]))

testSingletonTuple :: Test
testSingletonTuple = TestCase $
  assertEqual "{0}"
    (Right (TTuple [TNat]))
    (run (Tuple [Zero]))

testEmptyTuple :: Test
testEmptyTuple = TestCase $
  assertEqual "{}"
    (Right (TTuple []))
    (run (Tuple []))

-- Projection (well-typed)
testProjection :: Test
testProjection = TestCase $
  assertEqual "proj 1 {true,0,false}"
    (Right TNat)
    (run (Proj 1 (Tuple [ETrue, Zero, EFalse])))

-- Projection (ill-typed)
testProjectionOutOfBounds :: Test
testProjectionOutOfBounds = TestCase $
  case run (Proj 10 (Tuple [ETrue])) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testProjectionNonTuple :: Test
testProjectionNonTuple = TestCase $
  case run (Proj 0 ETrue) of
    Left _ -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Nested Tuple (well-typed)
testNestedTuple :: Test
testNestedTuple = TestCase $
  assertEqual "{{0,true},false}"
    (Right (TTuple [TTuple [TNat, TBool], TBool]))
    (run (Tuple [Tuple [Zero, ETrue], EFalse]))

-- Records (well-typed)
testRecordEmpty :: Test
testRecordEmpty = TestCase $
  assertEqual "{} has type TRecord []"
    (Right (TRecord []))
    (run (Record []))
 
testRecordBasic :: Test
testRecordBasic = TestCase $
  assertEqual "{x=true, y=zero} has type {x:Bool, y:Nat}"
    (Right (TRecord [("x", TBool), ("y", TNat)]))
    (run (Record [("x", ETrue), ("y", Zero)]))
 
testRecordFunctionField :: Test
testRecordFunctionField = TestCase $
  assertEqual "{f=\\x:Bool.x} has type {f:Bool->Bool}"
    (Right (TRecord [("f", TBool `TArrow` TBool)]))
    (run (Record [("f", Abs ("x", TBool) (Var "x"))]))
 
testRecordNested :: Test
testRecordNested = TestCase $
  assertEqual "{a={b=zero}} has type {a:{b:Nat}}"
    (Right (TRecord [("a", TRecord [("b", TNat)])]))
    (run (Record [("a", Record [("b", Zero)])]))
 
-- Records (ill-typed)
testRecordIllTypedField :: Test
testRecordIllTypedField = TestCase $
  case run (Record [("x", Succ ETrue)]) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)
 
-- RecordProj (well-typed)
testRecordProjFirst :: Test
testRecordProjFirst = TestCase $
  assertEqual "{x=true, y=zero}.x has type TBool"
    (Right TBool)
    (run (RecordProj (Record [("x", ETrue), ("y", Zero)]) "x"))
 
testRecordProjSecond :: Test
testRecordProjSecond = TestCase $
  assertEqual "{x=true, y=zero}.y has type TNat"
    (Right TNat)
    (run (RecordProj (Record [("x", ETrue), ("y", Zero)]) "y"))
 
testRecordProjNested :: Test
testRecordProjNested = TestCase $
  assertEqual "{a={b=zero}}.a.b has type TNat"
    (Right TNat)
    (run (RecordProj
           (RecordProj (Record [("a", Record [("b", Zero)])]) "a")
           "b"))
 
testRecordProjThroughAbs :: Test
testRecordProjThroughAbs = TestCase $
  assertEqual "(\\r:{x:Nat}. r.x) {x=zero} has type TNat"
    (Right TNat)
    (run (App
           (Abs ("r", TRecord [("x", TNat)]) (RecordProj (Var "r") "x"))
           (Record [("x", Zero)])))
 
-- RecordProj (ill-typed)
testRecordProjMissingLabel :: Test
testRecordProjMissingLabel = TestCase $
  case run (RecordProj (Record [("x", ETrue)]) "y") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)
 
testRecordProjFromNonRecord :: Test
testRecordProjFromNonRecord = TestCase $
  case run (RecordProj ETrue "x") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)


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
  , TestLabel "Tag well-typed"              testTagWellTyped
  , TestLabel "Tag second label"            testTagSecondLabel
  , TestLabel "Tag type mismatch"           testTagTypeMismatch
  , TestLabel "Tag unknown label"           testTagUnknownLabel
  , TestLabel "Tag not variant annotation"  testTagNotVariantAnnotation
  , TestLabel "CaseVariant well-typed"      testCaseVariantWellTyped
  , TestLabel "CaseVariant branch mismatch" testCaseVariantBranchMismatch
  , TestLabel "CaseVariant not variant"     testCaseVariantNotVariant
  , TestLabel "CaseVariant label mismatch"  testCaseVariantLabelMismatch
  , TestLabel "Tuple"                     testTuple
  , TestLabel "Singleton Tuple"          testSingletonTuple
  , TestLabel "Empty Tuple"              testEmptyTuple
  , TestLabel "Projection"               testProjection
  , TestLabel "Projection Out Of Bounds" testProjectionOutOfBounds
  , TestLabel "Projection Non Tuple"     testProjectionNonTuple
  , TestLabel "Nested Tuple" testNestedTuple
  , TestLabel "Record empty"              testRecordEmpty
  , TestLabel "Record basic"              testRecordBasic
  , TestLabel "Record function field"     testRecordFunctionField
  , TestLabel "Record nested"             testRecordNested
  , TestLabel "Record ill-typed field"    testRecordIllTypedField
  , TestLabel "RecordProj first"          testRecordProjFirst
  , TestLabel "RecordProj second"         testRecordProjSecond
  , TestLabel "RecordProj nested"         testRecordProjNested
  , TestLabel "RecordProj through abs"    testRecordProjThroughAbs
  , TestLabel "RecordProj missing label"  testRecordProjMissingLabel
  , TestLabel "RecordProj from non-record" testRecordProjFromNonRecord
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result > 0
    then fail "Some tests failed."
    else return ()
