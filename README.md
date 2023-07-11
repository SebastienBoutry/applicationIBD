# applicationIBD


# Application Diatomées 2023

Cette application Shiny, appelée "Diatomées 2023", permet de visualiser et de comparer les données de prélèvements diatomiques provenant de la base publique NAIADES qui répertorie l'ensemble des espèces de diatomées prélevées sur le territoire Français depuis 2007. L'application est structurée en plusieurs onglets, chacun offrant des fonctionnalités spécifiques. Au démarrage de l'application, le chargement de la dernière version des données est effectué, ce qui peut prendre quelques instants. Une fois chargées, les données sont formatées et rendues disponibles directement.

## Onglet "Bienvenue"

Dans cet onglet, une brève introduction à l'application est donnée. L'utilisateur est informé que l'application permet de visualiser et de comparer les données de prélèvements diatomiques de la base NAIADES. Les fonctionnalités spécifiques de chaque onglet sont également mentionnées. Si l'utilisateur a un doute, il peut revenir à cet onglet autant de fois qu'il le souhaite pour prendre en main l'application.

## Onglet "Données Brutes"

Cet onglet permet d'accéder aux données brutes extraites depuis NAIADES. Une interface de navigation par années de prélèvements est fournie dans le panneau de gauche. Chaque année est représentée par un tableau qui peut être filtré en fonction des vameurs présentes dans les colonnes. 

Les données peuvent être téléchargées au format CSV en utilisant le bouton "Download" situé en bas de chaque tableau. Le téléchargement peut être long, surtout pour les années récentes (à partir de 2015), ainsi l'utilisateur doit éviter d'utiliser le bouton 'Download' de cet onglet sans avoir été prévenu du temps de traitement de cette demande.

## Onglet "Carte"

Cet onglet est l'onglet principal de l'application. Il permet de visualiser les données sur une carte interactive centrée sur les taxons présents dans la base de données brute. L'utilisateur peut sélectionner un ou plusieurs taxons à afficher à l'aide d'une liste déroulante. Lorsqu'un ou plusieurs taxons sont sélectionnés, les taxons compris dans l'appelation apparaissent en dessous de la lise déroulante. Sur la droite, la carte affiche les emplacements géographiques du ou des taxons sélectionnés, et un menu permet de cocher les années à observer. Les points de la carte sont cliquables et affichent les informations du prélèvement correspondant. Deux histogrammes présentent l'évolution de l'abondance relative et du nombre d'occurrences du taxon sélectionné. Lorsqu'un deuxième taxon est sélectionné, les représentations graphiques des deux taxons se superposent pour permettre la comparaison. Attention, seulement deux taxons sont autorisés pour la comparaison afin d'éviter des problèmes d'affichage.

### Sous-Onglet "Données"

Ce sous-onglet affiche les données du ou des taxons sélectionnés depuis le panneau "Carte". Les données peuvent également être téléchargées au format CSV en utilisant le bouton "Download" et peuvent être filtré de la même manière que celle du panneau "Données Brutes".

### Sous-Onglet "Profile"

Ce sous-onglet comporte deux onglets : "Trophique" et "Écologique".

L'onglet "Trophique" permet de visualiser les préférenda physico-chimiques du ou des taxons sélectionnés. Au total 6 variables sont prises en compte à savoir la conductivité, l'azote organique, les nitrates, le phosphore, le PH, la saturation en oxygène et la demande en oxygène à 5 jours. Pour chaque paramètre, il est possible d'accéder aux seuils minimum et maximum pour lesquels le ou les taxons sont retrouvés ainsi que leur optimum. Ces informations ont été récupérées à partir des travaux de David Carayon en 2019, qui a dressé un tableau de profils physico-chimiques pour de nombreux taxons. Si un taxon n'est pas présent dans la base de David Carayon, un message d'information est affiché. Les valeurs des paramètres se superposent lorsque deux taxons sont sélectionnés, toujours pour permettre la comparaison.

Dans l'onglet "Écologique", on peut vérifier si le ou les taxons sélectionnés sont indicateurs de l'Indice Biologique Diatomées (IBD). Si c'est le cas, les graphiques des profiles écologiques s'affichent à l'écran, sinon un message est renvoyé.

---