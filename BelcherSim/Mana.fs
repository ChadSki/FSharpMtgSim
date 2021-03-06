﻿module Mana

open Cards

// Represents a quantity of mana.
//
// Rather than represent every kind of mana possible in MtG, this
// code focuses on mana encountered while playing Belcher.
//
// `redgreen` mana gets its own type, for Manamorphose.
//
// `other` mana is colored mana that we don't use. It's important to
// distinguish from colorless, because Chrome Mox needs a colored
// card to imprint.
type ManaAmount =
    { red:int; green:int; redgreen:int; colorless:int; other:int }

    static member (+) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       + right.red
          green     = left.green     + right.green
          redgreen  = left.redgreen  + right.redgreen
          colorless = left.colorless + right.colorless
          other     = left.other     + right.other }

    static member (*) (left:ManaAmount, right:int) =
        { red       = left.red       * right
          green     = left.green     * right
          redgreen  = left.redgreen  * right
          colorless = left.colorless * right
          other     = left.other     * right }

    // Important: Not commutative!
    // Rather than subtracting (spending) mana immediately, I recommend adding up
    // what you want to spend, and keep checking whether the subtraction works out.
    // Only commit to the subtraction as late as possible.
    static member (-) (left:ManaAmount, right:ManaAmount) : ManaAmount option =

        // Can't cast things that require `other` mana (we can use it for colorless though)
        if not (right.other = 0) then None
        else
            // Start with naive subtractions
            let mutable red = left.red - right.red
            let mutable green = left.green - right.green

            // Green and red costs can be paid with `redgreen`
            let mutable redgreen = left.redgreen
            if green < 0 then
                redgreen <- redgreen + green
                green <- 0
            if red < 0 then
                redgreen <- redgreen + red
                red <- 0

            // Fail if we ran out of colored mana
            if redgreen < 0 then None
            else
                // Naive subtract colorless
                let mutable colorless = left.colorless - right.colorless

                // Colorless costs can be paid with `other`
                let mutable other = left.other
                if colorless < 0 then
                    other <- other + colorless
                    colorless <- 0

                // Redgreen/colorless costs can be paid with `green` or
                // `red` or `redgreen` (attempted in that order).

                // First add the whole mess together.
                let mutable remaining = right.redgreen
                if other < 0 then
                    remaining <- remaining - other
                    other <- 0

                green <- green - remaining      // Try taking from green.
                if green < 0 then
                    red <- red + green          // Try taking from red
                    green <- 0

                if red < 0 then
                    redgreen <- redgreen + red  // Try taking from redgreen
                    red <- 0

                // If we're in negative redgreen mana, we've run out.
                if redgreen < 0 then None
                else Some { red       = red
                            green     = green
                            redgreen  = redgreen
                            colorless = colorless
                            other     = other }

let Magnitude (m:ManaAmount) : int =
    m.red + m.green + m.redgreen + m.colorless + m.other

type ManaColor = Red | Green | RedGreen | Colorless | Other

let ColorLabel = function
    | Red -> "R"
    | Green -> "G"
    | RedGreen -> "R/G"
    | Colorless -> "C"
    | Other -> "?"

let Color = function
    | { red=r; green=g; redgreen=rg; colorless=_; other=_ }
      when (r > 0 && g > 0) || rg > 0
        -> RedGreen

    | { red=r; green=_; redgreen=_; colorless=_; other=_ }
      when r > 0
        -> Red

    | { red=_; green=g; redgreen=_; colorless=_; other=_ }
      when g > 0
        -> Green

    | { red=_; green=g; redgreen=_; colorless=_; other=o }
      when o > 0
        -> Other

    | _ -> Colorless

let noMana = { red=0; green=0; redgreen=0; colorless=0; other=0 }

let OneMana = function
    | Red -> { red=1; green=0; redgreen=0; colorless=0; other=0 }
    | Green -> { red=0; green=1; redgreen=0; colorless=0; other=0 }
    | RedGreen -> { red=0; green=0; redgreen=1; colorless=0; other=0 }
    | Colorless -> { red=0; green=0; redgreen=0; colorless=1; other=0 }
    | Other -> { red=0; green=0; redgreen=0; colorless=0; other=1 }

let twoRedMana = OneMana Red + OneMana Red
let threeRedMana = twoRedMana + OneMana Red
let fiveRedMana = twoRedMana + threeRedMana
let twoRedGreenMana = OneMana RedGreen + OneMana RedGreen
let threeRedGreenMana = twoRedGreenMana + OneMana RedGreen
let threeColorlessMana = OneMana Colorless + OneMana Colorless + OneMana Colorless

let Cost = function
    | BurningWish ->           { red=1; green=0; redgreen=0; colorless=1; other=0 }
    | ChancellorOfTheTangle -> { red=0; green=3; redgreen=0; colorless=4; other=0 }
    | ChromeMox ->             noMana
    | DesperateRitual ->       { red=1; green=0; redgreen=0; colorless=1; other=0 }
    | ElvishSpiritGuide ->     { red=0; green=1; redgreen=0; colorless=2; other=0 }
    | EmptyTheWarrens ->       { red=1; green=0; redgreen=0; colorless=3; other=0 }
    | GitaxianProbe ->         { red=0; green=0; redgreen=0; colorless=1; other=1 }
    | GoblinCharbelcher ->     { red=0; green=0; redgreen=0; colorless=4; other=0 }
    | LandGrant ->             { red=0; green=1; redgreen=0; colorless=1; other=0 }
    | LionsEyeDiamond ->       noMana
    | LotusPetal ->            noMana
    | Manamorphose ->          { red=0; green=0; redgreen=2; colorless=0; other=0 }
    | PyreticRitual ->         { red=1; green=0; redgreen=0; colorless=1; other=0 }
    | RiteOfFlame ->           { red=1; green=0; redgreen=0; colorless=0; other=0 }
    | SeethingSong ->          { red=1; green=0; redgreen=0; colorless=2; other=0 }
    | SerumPowder ->           { red=0; green=0; redgreen=0; colorless=3; other=0 }
    | SimianSpiritGuide ->     { red=1; green=0; redgreen=0; colorless=2; other=0 }
    | StreetWraith ->          { red=0; green=0; redgreen=0; colorless=3; other=2 }
    | Taiga ->                 noMana
    | TinderWall ->            { red=0; green=1; redgreen=0; colorless=0; other=0 }


let CanPay (pendingCosts:ManaAmount) manaPool =
    match manaPool - pendingCosts with
    | None -> false
    | Some _ -> true
