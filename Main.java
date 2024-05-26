import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

// Classe Destination
class Destination implements Comparable<Destination> {
    private String pays;
    private String ville;
    private int duree;
    private double budget;

    public Destination(String pays, String ville, int duree, double budget) {
        this.pays = pays;
        this.ville = ville;
        this.duree = duree;
        this.budget = budget;
    }

    public String getPays() {
        return pays;
    }

    public String getVille() {
        return ville;
    }

    public int getDuree() {
        return duree;
    }

    public double getBudget() {
        return budget;
    }

    @Override
    public String toString() {
        return "Destination{" +
                "pays='" + pays + '\'' +
                ", ville='" + ville + '\'' +
                ", duree=" + duree +
                " jours, budget=" + budget +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Destination that = (Destination) o;
        return duree == that.duree &&
                Double.compare(that.budget, budget) == 0 &&
                Objects.equals(pays, that.pays) &&
                Objects.equals(ville, that.ville);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pays, ville, duree, budget);
    }

    @Override
    public int compareTo(Destination other) {
        int result = this.pays.compareTo(other.pays);
        if (result == 0) {
            result = this.ville.compareTo(other.ville);
            if (result == 0) {
                result = Integer.compare(this.duree, other.duree);
                if (result == 0) {
                    result = Double.compare(this.budget, other.budget);
                }
            }
        }
        return result;
    }
}

// Classe Activité
class Activité implements Comparable<Activité> {
    private String description;
    private double prix;

    public Activité(String description, double prix) {
        this.description = description;
        this.prix = prix;
    }

    public String getDescription() {
        return description;
    }

    public double getPrix() {
        return prix;
    }

    @Override
    public String toString() {
        return "Activité{" +
                "description='" + description + '\'' +
                ", prix=" + prix +
                '}';
    }

    @Override
    public int compareTo(Activité other) {
        int result = this.description.compareTo(other.description);
        if (result == 0) {
            result = Double.compare(this.prix, other.prix);
        }
        return result;
    }
}

// Classe Site
class Site implements Comparable<Site> {
    private String nom;
    private double distance;

    public Site(String nom, double distance) {
        this.nom = nom;
        this.distance = distance;
    }

    public String getNom() {
        return nom;
    }

    public double getDistance() {
        return distance;
    }

    @Override
    public String toString() {
        return "Site{" +
                "nom='" + nom + '\'' +
                ", distance=" + distance +
                '}';
    }

    @Override
    public int compareTo(Site other) {
        int result = this.nom.compareTo(other.nom);
        if (result == 0) {
            result = Double.compare(this.distance, other.distance);
        }
        return result;
    }
}

// Classe PlanificationVoyage
class PlanificationVoyage implements Iterable<Activité> {
    private Map<Destination, SortedSet<Activité>> planification;
    private int nombreOperations = 0;
    private Destination destinationCourante;

    public PlanificationVoyage() {
        this.planification = new TreeMap<>();
    }

    public void ajouterActivité(Destination destination, Activité activité) {
        if (!planification.containsKey(destination)) {
            planification.put(destination, new TreeSet<>());
        }
        planification.get(destination).add(activité);
        nombreOperations++;
    }

    public SortedSet<Activité> obtenirActivités(Destination destination) {
        if (planification.containsKey(destination)) {
            return planification.get(destination);
        } else {
            return Collections.emptySortedSet();
        }
    }

    public void setDestinationCourante(Destination destination) {
        this.destinationCourante = destination;
    }

    @Override
    public Iterator<Activité> iterator() {
        return new ActivitéIterateur(destinationCourante);
    }

    private class ActivitéIterateur implements Iterator<Activité> {
        private int indiceIterateur = 0;
        private int nombreOperationsReference = nombreOperations;
        private boolean nextEffectue = false;
        private List<Activité> activités;

        public ActivitéIterateur(Destination destination) {
            if (destination == null) {
                this.activités = Collections.emptyList();
            } else {
                this.activités = new ArrayList<>(planification.getOrDefault(destination, Collections.emptySortedSet()));
            }
        }

        @Override
        public boolean hasNext() {
            return indiceIterateur < activités.size();
        }

        @Override
        public Activité next() {
            verificationConcurrence();
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            nextEffectue = true;
            return activités.get(indiceIterateur++);
        }

        @Override
        public void remove() {
            if (!nextEffectue) {
                throw new IllegalStateException();
            }
            verificationConcurrence();
            activités.remove(--indiceIterateur);
            nombreOperations++;
            nombreOperationsReference++;
            nextEffectue = false;
        }

        private void verificationConcurrence() {
            if (nombreOperations != nombreOperationsReference) {
                throw new ConcurrentModificationException();
            }
        }
    }

    @Override
    public String toString() {
        return "PlanificationVoyage{" +
                "planification=" + planification +
                '}';
    }
}

// Classe ParcoursVoyage
class ParcoursVoyage implements Iterable<Site> {
    private Map<Destination, SortedSet<Site>> parcours;
    private int nombreOperations = 0;

    public ParcoursVoyage() {
        this.parcours = new TreeMap<>();
    }

    public void ajouterSite(Destination destination, Site site) {
        if (!parcours.containsKey(destination)) {
            parcours.put(destination, new TreeSet<>());
        }
        parcours.get(destination).add(site);
        nombreOperations++;
    }

    public SortedSet<Site> obtenirSites(Destination destination) {
        if (parcours.containsKey(destination)) {
            return parcours.get(destination);
        } else {
            return Collections.emptySortedSet();
        }
    }

    @Override
    public Iterator<Site> iterator() {
        return new Iterateur();
    }

    private class Iterateur implements Iterator<Site> {
        private int indiceIterateur = 0;
        private int nombreOperationsReference = nombreOperations;
        private boolean nextEffectue = false;
        private List<Site> tousLesSites;

        public Iterateur() {
            tousLesSites = new ArrayList<>();
            for (SortedSet<Site> sites : parcours.values()) {
                tousLesSites.addAll(sites);
            }
        }

        @Override
        public boolean hasNext() {
            return indiceIterateur < tousLesSites.size();
        }

        @Override
        public Site next() {
            verificationConcurrence();
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            nextEffectue = true;
            return tousLesSites.get(indiceIterateur++);
        }

        @Override
        public void remove() {
            if (!nextEffectue) {
                throw new IllegalStateException();
            }
            verificationConcurrence();
            tousLesSites.remove(--indiceIterateur);
            nombreOperations++;
            nombreOperationsReference++;
            nextEffectue = false;
        }

        private void verificationConcurrence() {
            if (nombreOperations != nombreOperationsReference) {
                throw new ConcurrentModificationException();
            }
        }
    }

    @Override
    public String toString() {
        return "ParcoursVoyage{" +
                "parcours=" + parcours +
                '}';
    }
}

// Classe Main pour tester les fonctionnalités
public class Main {
    public static void main(String[] args) {
        Destination paris = new Destination("France", "Paris", 5, 1000.0);
        Destination rome = new Destination("Italie", "Rome", 7, 1500.0);

        Activité visiteTourEiffel = new Activité("Visite de la Tour Eiffel", 25.0);
        Activité visiteLouvre = new Activité("Visite du Louvre", 15.0);
        Activité visiteColisée = new Activité("Visite du Colisée", 30.0);
        Activité baladeBateau = new Activité("Balade en bateau", 40.0);

        PlanificationVoyage planificationVoyage = new PlanificationVoyage();
        planificationVoyage.ajouterActivité(paris, visiteTourEiffel);
        planificationVoyage.ajouterActivité(paris, visiteLouvre);
        planificationVoyage.ajouterActivité(rome, visiteColisée);
        planificationVoyage.ajouterActivité(rome, baladeBateau);

        Site siteTourEiffel = new Site("Tour Eiffel", 1.0);
        Site siteColisée = new Site("Colisée", 2.5);
        Site siteVatican = new Site("Vatican", 3.0);

        ParcoursVoyage parcoursVoyage = new ParcoursVoyage();
        parcoursVoyage.ajouterSite(paris, siteTourEiffel);
        parcoursVoyage.ajouterSite(rome, siteColisée);
        parcoursVoyage.ajouterSite(rome, siteVatican);

        // Afficher toutes les informations pour la destination Rome
        afficherInformationsDestination(rome, planificationVoyage, parcoursVoyage);

        // Écrire les données dans un fichier CSV
        ecrireCSV("data.csv", Arrays.asList(paris, rome), planificationVoyage, parcoursVoyage);
    }

    public static void afficherInformationsDestination(Destination destination, PlanificationVoyage planificationVoyage, ParcoursVoyage parcoursVoyage) {
        System.out.println("Informations pour la destination : " + destination.getVille());
        System.out.println("Pays : " + destination.getPays());
        System.out.println("Durée du voyage : " + destination.getDuree() + " jours");
        System.out.println("Budget : " + destination.getBudget() + " €");

        System.out.println("\nActivités à faire :");
        SortedSet<Activité> activites = planificationVoyage.obtenirActivités(destination);
        for (Activité activite : activites) {
            System.out.println(activite);
        }

        System.out.println("\nSites à visiter :");
        SortedSet<Site> sites = parcoursVoyage.obtenirSites(destination);
        for (Site site : sites) {
            System.out.println(site);
        }
    }

    public static void ecrireCSV(String filename, List<Destination> destinations, PlanificationVoyage planificationVoyage, ParcoursVoyage parcoursVoyage) {
        try (FileWriter writer = new FileWriter(filename)) {
            // Écrire les destinations
            writer.append("Pays,Ville,Durée,Budget\n");
            for (Destination destination : destinations) {
                writer.append(destination.getPays()).append(",")
                      .append(destination.getVille()).append(",")
                      .append(String.valueOf(destination.getDuree())).append(",")
                      .append(String.valueOf(destination.getBudget())).append("\n");
            }

            // Écrire les activités
            writer.append("\nDescription,Prix\n");
            for (Destination destination : destinations) {
                SortedSet<Activité> activites = planificationVoyage.obtenirActivités(destination);
                for (Activité activite : activites) {
                    writer.append(activite.getDescription()).append(",")
                          .append(String.valueOf(activite.getPrix())).append("\n");
                }
            }

            // Écrire les sites
            writer.append("\nNom,Distance\n");
            for (Destination destination : destinations) {
                SortedSet<Site> sites = parcoursVoyage.obtenirSites(destination);
                for (Site site : sites) {
                    writer.append(site.getNom()).append(",")
                          .append(String.valueOf(site.getDistance())).append("\n");
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
