open Csv 

type mode = Bus | Train | Avion | Voiture

type transport = {
  mode: mode;
  heure_depart: string;
  heure_arrivee: string;
  duree: float;
  cout: float;
}

type categorie_activite = Culture | Sport | Loisir

type activite = {
  nom: string;
  categorie: categorie_activite;
  cout: float;
}

let parse_mode = function
  | "Bus" -> Bus
  | "Train" -> Train
  | "Avion" -> Avion
  | "Voiture" -> Voiture
  | _ -> failwith "Mode de transport inconnu"

let parse_categorie = function
  | "Culture" -> Culture
  | "Sport" -> Sport
  | "Loisir" -> Loisir
  | _ -> failwith "Catégorie d'activité inconnue"

let read_csv filename =
  let csv = Csv.load filename in
  let rec parse_rows rows current_section transports activites =
    match rows with
    | [] -> List.rev transports, List.rev activites
    | row::rest ->
      match current_section, row with
      | None, ["Mode"; "Heure Départ"; "Heure Arrivée"; "Durée"; "Coût"] ->
        parse_rows rest (Some "transport") transports activites
      | None, ["Nom"; "Catégorie"; "Coût"] ->
        parse_rows rest (Some "activite") transports activites
      | Some "transport", [mode; heure_depart; heure_arrivee; duree; cout] ->
        let transport = {
          mode = parse_mode mode;
          heure_depart = heure_depart;
          heure_arrivee = heure_arrivee;
          duree = float_of_string duree;
          cout = float_of_string cout;
        } in
        parse_rows rest current_section (transport::transports) activites
      | Some "activite", [nom; categorie; cout] ->
        let activite = {
          nom = nom;
          categorie = parse_categorie categorie;
          cout = float_of_string cout;
        } in
        parse_rows rest current_section transports (activite::activites)
      | _ -> parse_rows rest current_section transports activites
  in
  parse_rows csv None [] []

let filtrer_activites categorie activites =
  List.filter (fun a -> a.categorie = categorie) activites

let itineraire_optimal transports =
  match List.fold_left (fun acc t ->
    match acc with
    | None -> Some t
    | Some t' -> if t.cout < t'.cout then Some t else acc) None transports with
  | None -> None
  | Some t ->
    Some (Printf.sprintf "Itinéraire optimal - Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f"
      (match t.mode with Bus -> "Bus" | Train -> "Train" | Avion -> "Avion" | Voiture -> "Voiture")
      t.heure_depart t.heure_arrivee t.duree t.cout)

let () =
  let transports, activites = read_csv "data.csv" in
  
  (* Afficher les transports *)
  List.iter (fun t -> Printf.printf "Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f\n"
               (match t.mode with Bus -> "Bus" | Train -> "Train" | Avion -> "Avion" | Voiture -> "Voiture")
               t.heure_depart t.heure_arrivee t.duree t.cout) transports;

  (* Afficher les activités *)
  List.iter (fun a -> Printf.printf "Nom: %s, Catégorie: %s, Coût: %.2f\n"
               a.nom (match a.categorie with Culture -> "Culture" | Sport -> "Sport" | Loisir -> "Loisir") a.cout) activites;

  (* Afficher l'itinéraire optimal *)
  match itineraire_optimal transports with
  | None -> print_endline "Aucun transport trouvé"
  | Some info -> print_endline info;

  (* Filtrer et afficher les activités sportives *)
  let activites_sportives = filtrer_activites Sport activites in
  List.iter (fun a -> Printf.printf "Nom: %s, Catégorie: Sport, Coût: %.2f\n" a.nom a.cout) activites_sportives
