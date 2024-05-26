open Csv

type mode = Bus | Train | Avion | Voiture

type transport = {
  mode: mode;
  heure_depart: string;
  heure_arrivee: string;
  duree: float;
  cout: float;
}

type activite = {
  nom: string;
  categorie: string;
  cout: float;
}

let read_csv filename =
  let csv = Csv.load filename in
  let rec parse_rows rows current_section transports activites =
    match rows with
    | [] -> (transports, activites)
    | row::rest ->
      if List.length row = 0 then
        parse_rows rest None transports activites
      else
        match current_section with
        | None ->
          (match row with
           | ["Mode"; "Heure Départ"; "Heure Arrivée"; "Durée"; "Coût"] ->
             print_endline "Transports:";
             parse_rows rest (Some "transport") transports activites
           | ["Nom"; "Catégorie"; "Coût"] ->
             print_endline "Activités:";
             parse_rows rest (Some "activite") transports activites
           | _ -> parse_rows rest None transports activites)
        | Some "transport" ->
          (match row with
           | [mode; heure_depart; heure_arrivee; duree; cout] ->
             let mode = match mode with
               | "Bus" -> Bus
               | "Train" -> Train
               | "Avion" -> Avion
               | "Voiture" -> Voiture
               | _ -> failwith "Mode de transport inconnu"
             in
             let transport = {
               mode = mode;
               heure_depart = heure_depart;
               heure_arrivee = heure_arrivee;
               duree = float_of_string duree;
               cout = float_of_string cout;
             } in
             parse_rows rest current_section (transport::transports) activites
           | _ -> parse_rows rest current_section transports activites)
        | Some "activite" ->
          (match row with
           | [nom; categorie; cout] ->
             let activite = {
               nom = nom;
               categorie = categorie;
               cout = float_of_string cout;
             } in
             parse_rows rest current_section transports (activite::activites)
           | _ -> parse_rows rest current_section transports activites)
        | _ -> parse_rows rest current_section transports activites
  in
  parse_rows csv None [] []

let itineraire_optimal transports =
  List.fold_left (fun acc t ->
    match acc with
    | None -> Some t
    | Some t' -> if t.cout < t'.cout then Some t else acc) None transports

let filtrer_activites categorie activites =
  List.filter (fun a -> a.categorie = categorie) activites

let () =
  let (transports, activites) = read_csv "data.csv" in
  List.iter (fun t -> Printf.printf "Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f\n"
               (match t.mode with
                | Bus -> "Bus"
                | Train -> "Train"
                | Avion -> "Avion"
                | Voiture -> "Voiture")
               t.heure_depart t.heure_arrivee t.duree t.cout) transports;
  List.iter (fun a -> Printf.printf "Nom: %s, Catégorie: %s, Coût: %.2f\n"
               a.nom a.categorie a.cout) activites;

  match itineraire_optimal transports with
  | None -> print_endline "Aucun transport trouvé"
  | Some t ->
    Printf.printf "Itinéraire optimal - Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f\n"
      (match t.mode with
       | Bus -> "Bus"
       | Train -> "Train"
       | Avion -> "Avion"
       | Voiture -> "Voiture")
      t.heure_depart t.heure_arrivee t.duree t.cout;

  let activites_sportives = filtrer_activites "Sport" activites in
  List.iter (fun a -> Printf.printf "Nom: %s, Catégorie: %s, Coût: %.2f\n"
               a.nom a.categorie a.cout) activites_sportives



