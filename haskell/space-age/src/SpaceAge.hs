module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / yearToSec 0.2408467
ageOn Venus seconds = seconds / yearToSec 0.61519726
ageOn Earth seconds = seconds / yearToSec 1
ageOn Mars seconds = seconds / yearToSec 1.8808158
ageOn Jupiter seconds = seconds / yearToSec 11.862615
ageOn Saturn seconds = seconds / yearToSec 29.447498
ageOn Uranus seconds = seconds / yearToSec 84.016846
ageOn Neptune seconds = seconds / yearToSec 164.79132

yearToSec :: Float -> Float
yearToSec diffEarth = 60*60*24*365.25*diffEarth 