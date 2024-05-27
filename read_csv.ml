open Csv 

type mode_transport = Bus | Train | Avion | Voiture

type transport = {
  mode_transport: mode_transport;
  heure_depart: string;
  heure_arrivee: string;
  duree: float;
  cout: float;
}

type categorie_activite = Culture | Sport | Loisir

type activite = {
  nom_activite: string;
  categorie_activite: categorie_activite;
  cout_activite: float;
}

let parse_mode_transport = function
  | "Bus" -> Bus
  | "Train" -> Train
  | "Avion" -> Avion
  | "Voiture" -> Voiture
  | _ -> failwith "Mode de transport inconnu"

let parse_categorie_activite = function
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
      | Some "transport", [mode_transport; heure_depart; heure_arrivee; duree; cout] ->
        let transport = {
          mode_transport = parse_mode_transport mode_transport;
          heure_depart = heure_depart;
          heure_arrivee = heure_arrivee;
          duree = float_of_string duree;
          cout = float_of_string cout;
        } in
        parse_rows rest current_section (transport::transports) activites
      | Some "activite", [nom_activite; categorie_activite; cout_activite] ->
        let activite = {
          nom_activite = nom_activite;
          categorie_activite = parse_categorie_activite categorie_activite;
          cout_activite = float_of_string cout_activite;
        } in
        parse_rows rest current_section transports (activite::activites)
      | _ -> parse_rows rest current_section transports activites
  in
  parse_rows csv None [] []


let itineraire_optimal transports =
  match List.fold_left (fun acc transport ->
    match acc with
    | None -> Some transport
    | Some t' -> if transport.cout < t'.cout then Some transport else acc) None transports with
  | None -> None
  | Some transport ->
    Some (Printf.sprintf "Itinéraire optimal - Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f"
      (match transport.mode_transport with
       | Bus -> "Bus"
       | Train -> "Train"
       | Avion -> "Avion"
       | Voiture -> "Voiture")
      transport.heure_depart transport.heure_arrivee transport.duree transport.cout)

let filtrer_activites categorie activites =
  List.filter (fun activite -> activite.categorie_activite = categorie) activites

let afficher_transports transports =
  List.iter (fun transport -> Printf.printf "Mode: %s, Départ: %s, Arrivée: %s, Durée: %.2f, Coût: %.2f\n"
               (match transport.mode_transport with
                | Bus -> "Bus"
                | Train -> "Train"
                | Avion -> "Avion"
                | Voiture -> "Voiture")
               transport.heure_depart transport.heure_arrivee transport.duree transport.cout) transports

let afficher_activites activites =
  List.iter (fun activite -> Printf.printf "Nom: %s, Catégorie: %s, Coût: %.2f\n"
               activite.nom_activite (match activite.categorie_activite with
                                      | Culture -> "Culture"
                                      | Sport -> "Sport"
                                      | Loisir -> "Loisir")
               activite.cout_activite) activites

let afficher_itineraire_optimal transports =
  match itineraire_optimal transports with
  | None -> print_endline "Aucun transport trouvé"
  | Some info -> print_endline info

let () =
  let transports, activites = read_csv "data.csv" in
  
  (* Afficher les transports *)
  afficher_transports transports;

  (* Afficher les activités *)
  afficher_activites activites;

  (* Afficher l'itinéraire optimal *)
  afficher_itineraire_optimal transports;

  (* Filtrer et afficher les activités sportives *)
  let activites_sportives = filtrer_activites Sport activites in
  List.iter (fun activite -> Printf.printf "Nom: %s, Catégorie: Sport, Coût: %.2f\n" 
                activite.nom_activite activite.cout_activite) activites_sportives

