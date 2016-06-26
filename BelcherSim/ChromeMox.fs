module ChromeMox

open Cards
open CardGroup
open Mana

// Returns `None` if a card should never be imprinted, and the
// priority otherwise. Lower number is higher priority.
let ChromeMoxImprintPriority card =

    // Too important.
    if IsWinCondition card then None

    // Good choice for imprinting, because it's too expensive to cast normally.
    else if card = ChancellorOfTheTangle then Some (0, card)

    // TODO: These priorities might need adjustment.
    else match Color (Cost card) with
         | Colorless -> None  // Cannot tap Chrome Mox for colorless mana.
         | RedGreen -> Some (1, card)
         | Other -> Some (2, card)  // Less useful color, but better sacrifices? ¯\_(ツ)_/¯
         | Red | Green -> Some (3, card)

// Used to determine the best card to imprint on Chrome Mox.
// May return `None`, in which case it's better to play Chrome Mox without imprinting.
let ChooseImprint cardGroup =
    match cardGroup |> List.choose ChromeMoxImprintPriority with

    // Nothing worth imprinting
    | [] -> None

    // Pick the top prioritized card
    | x -> Some (x |> Shuffle  // TODO: does this help randomize among equally-prioritized cards?
                   |> List.sort
                   |> List.head
                   |> fun (_, card) -> card)
