module Mana

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

    static member (-) (left:ManaAmount, right:ManaAmount) =
        { red       = left.red       - right.red
          green     = left.green     - right.green
          redgreen  = left.redgreen  - right.redgreen
          colorless = left.colorless - right.colorless
          other     = left.other     - right.other }

let Magnitude (m:ManaAmount) : int =
    m.red + m.green + m.redgreen + m.colorless + m.other

let noMana = { red=0; green=0; redgreen=0; colorless=0; other=0 }

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

type ManaColor =
     | Red
     | Green
     | RedGreen
     | Colorless
     | Other

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
