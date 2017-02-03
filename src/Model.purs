module Model where

type Exercise = {
    name :: String,
    weight :: Number
}

type ExerciseResults = {
    exercise :: Exercise,
    results :: Array Int
}

type Workout = {
    results :: Array ExerciseResults,
    currentWeight :: Number
}
