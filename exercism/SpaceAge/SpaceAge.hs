module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYearInSeconds = 31557600

earthAge :: Float -> Float
earthAge seconds = seconds / 31557600

orbitalPeriod :: Planet -> Float
orbitalPeriod planet = case planet of 
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Earth -> 1
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.791321

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthAge seconds / orbitalPeriod planet
