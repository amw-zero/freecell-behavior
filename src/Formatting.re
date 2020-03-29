let string_of_list = l => "[" ++ String.concat(", ", l) ++ "]";
let string_of_tuple = t => "(" ++ fst(t) ++ "," ++ snd(t) ++ ")";
let string_of_card_list = cards =>
  Belt.List.map(cards, c => FreeCellBehavior.string_of_card(Some(c)))
  |> String.concat(", ");

let string_of_optional_card_list = cards =>
  Belt.List.map(cards, c => FreeCellBehavior.string_of_card(c))
  |> String.concat(", ");
