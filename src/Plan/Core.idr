module Plan.Core

import Data.IORef

%default total

data Exists : (a : Type -> Type) -> Type where
  Evidence : (t : Type) -> a t -> Exists a

record Action (param : Type) (sysdata : Type) where
  constructor MkAction
  name : String
  expected : param -> sysdata -> sysdata
  execute : param -> sysdata -> IORef Bool -> IO ()

ActionE : Type -> Type
ActionE sysdata = Exists (\p => Action p sysdata)

record Step (param : Type) (sysdata : Type) where
  constructor MkStep
  action : Action param sysdata
  value : param

StepE : Type -> Type
StepE sysdata = Exists (\p => Step p sysdata)

record PlanningSystem (sysdata : Type) where
  constructor MkPlanningSystem
  name : String
  constraint : Maybe (sysdata -> Bool)
  optimize : Maybe (sysdata -> Double)
  actions : List (ActionE sysdata)
  datafetcher : IO sysdata

makePlan : {n : Nat} -> PlanningSystem sysdata -> sysdata -> Maybe (List (StepE sysdata))
makePlan system = ?plan

executePlan : sysdata -> List (StepE sysdata) -> IORef Bool -> IO ()
executePlan _ Nil _ = pure ()
executePlan systemData (Evidence t (MkStep action value) :: remainder) cancelFlag = do
  cancelled <- readIORef cancelFlag
  case cancelled of
    True => pure ()
    _ => do
      let systemDataPostExecution = (expected action) value systemData
      (execute action) value systemData cancelFlag
      executePlan systemDataPostExecution remainder cancelFlag
