module Plan.Core

%default total

record Action (param : Type) (sysdata : Type) where
  constructor MkAction
  name : String
  expected : param -> sysdata -> sysdata
  execute : param -> sysdata -> MVar Bool -> IO ()

record Step (param : Type) (sysdata : Type) where
  constructor MkStep
  action : Action param sysdata
  value : param

record PlanningSystem (sysdata : Type) where
  constructor MkPlanningSystem
  name : String
  constraint : Maybe (sysdata -> Bool)
  optimize : Maybe (sysdata -> Double)
  actions : List (Exists (\p => Action p sysdata))
  datafetcher : IO sysdata

StepE : Type -> Type
StepE sysdata = Exists (\p => Step p sysdata)

executeAction : Action param sysdata -> param -> sysdata -> MVar Bool -> IO ()
executeAction (MkAction _ _ execute) value systemData cancelFlag = execute value systemData cancelFlag

expectAction : Action param sysdata -> param -> sysdata -> sysdata
expectAction (MkAction _ expected _) value systemData = expected value systemData

fetchSystemData : PlanningSystem sysdata -> IO (Maybe sysdata)
fetchSystemData (MkPlanningSystem _ _ _ _ fetcher) = fetcher

makePlan : PlanningSystem sysdata -> sysdata -> Maybe (List (StepE sysdata))
makePlan system = ?plan

executePlan : sysdata -> List (StepE sysdata) -> MVar Bool -> IO ()
executePlan _ Nil _ = do pure ()
executePlan systemData (Evidence (MkStep _ action value) :: remainder) cancelFlag = do
  cancelled <- tryTakeMVar cancelFlag
  case cancelled of
    Just True => pure ()
    _ => do
      let systemDataPostExecution = expectAction action value systemData
      executeAction action value systemData cancelFlag
      executePlan systemDataPostExecution remainder cancelFlag
