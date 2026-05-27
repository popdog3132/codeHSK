module ScoringContext
  ( ScoreContext(..)
  , scoreContextName
  ) where

data ScoreContext
  = SearchScoring
  | HoleExpansionScoring
  | FunctionBodyScoring
  deriving (Show, Eq)

scoreContextName :: ScoreContext -> String
scoreContextName context =
  case context of
    SearchScoring ->
      "search"
    HoleExpansionScoring ->
      "hole_expansion"
    FunctionBodyScoring ->
      "function_body"
