# applicationIBD


# Application VisualDiatoms

Cette application vous permet de visualiser et de comparer les données de prélèvements diatomiques présents sur la base publique NAIADES.
            Si c'est la première fois que vous entrez sur l'application, une notice d'utilisation est à votre disposition ci-dessous.

## Introduction

L'objectif de cette application est d'améliorer la compréhension de la donnée diatomique, en proposant à l'utilisateur de visualiser les taxons enregistrés sur la base de données publique NAIADES. Grâce à l'interface interactive, vous allez pouvoir sélectionner, télécharger et même comparer les données des prélèvements floristiques opérés par les agences de l'eau depuis 2007. Notre application se décompose en plusieurs onglets qui rendent la navigation simple et intuitive. Pour être certains que vous profiterez pleinement de l'expérience, la suite de cette page décrit de manière synthétique les fonctionnalités que vous retrouverez dans l'application.

## Onglet "Données Brutes"

En cliquant sur cet onglet, vous avez directement accès à la dernière version des données brutes extraites et formatées depuis NAIADES. Servez-vous du panneau de gauche pour naviguer entre les années de prélèvements. Chaque tableau d'une année est construit de la même façon, les colonnes sont requêtables pour celles présentant des caractères. Les colonnes numérique peuvent quand à elles être filtrées à l'aide d'un curseur qui apparaît lorsque vous cliquez dans la barre de recherche de la colonne. Enfin, si vous en avez besoin, vous pouvez télécharger les données d'une année au format CSV à l'aide du bouton 'Download' situé en bas de chaque tableau.Veuillez noter que le volume des données est assez important et peut donc prendre plusieurs secondes voir quelques minutes !

## Onglet "Carte"

Cet onglet est composé de plusieurs éléments. Le panneau 'Chorologie' vous permet de visualiser les données de la base brute avec une vue centrée sur les taxons. Sélectionnez le ou les taxons à afficher à l'aide de la liste déroulante 'Liste des taxons disponibles'. La carte de l'onglet affichera ensuite les emplacements géographiques des taxons sélectionnés, et un menu vous permettra de cocher les années que vous souhaitez observer. Vous pouvez sélectionner jusqu'à deux taxons. Chaque point de la carte est cliquable et affichera les informations du prélèvement que vous regardez. En dessous de 'Liste des taxons disponibles', les taxons appariés à celui ou ceux que vous avez choisit vous sont précisés. Enfin, deux histogrammes interactifs sont activables via les boutons 'Afficher les Abondances Moyennes' et 'Afficher les occurences'. Ils présentent l'évolution de l'abondance relative (en pour 1000) et l'évolution du nombre d'occurences du ou des taxons dans les relevés des années ou il(s) est/sont vu(s), ce qui vous permet d'avoir une idée de son/leur importance. Lorsque vous sélectionnez un deuxième taxon, les représentations graphiques de chacun (Carte et Plot) se superposent pour vous permettre de comparer les deux.

### Sous-Onglet "Données"

Dans ce panneau, vous pouvez afficher et télécharger les données des taxons que vous avez sélectionnés depuis le panneau Chorologie. Utilisez le bouton 'Download' pour télécharger les données au format CSV.

### Sous-Onglet "Profils"

Ce sous-onglet comporte deux onglets : "Trophique" et "Écologique".

Dans l'onglet 'Trophique', vous pouvez afficher les informations concernant les préférenda pyshico-chimiques des taxons que vous avez sélectionné. Ces informations ont été récupérées grâce aux travaux de David Carayon en 2019 qui a dréssé un tableau de profils physico-chimiques d'un grand nombre de taxons. Si un taxon n'est pas présent dans la base de David Carayon, alors un message apparaîtra vous le précisant. Les valeurs des paramètres se superposent la encore si vous sélectionnez deux taxons.

Dans l'onglet 'Écologique', vous pouvez voir si le taxon sélectionné est indicateur ou pas de l'Indice Biologique Diatomées (IBD). Si il l'est, vous verrez le profil écologique du taxon en question s'afficher à l'écran, sinon un message apparaîtra, comme pour l'onglet Trophique

---