# Evaluateur-Typeur-de-Lambda-Calcul

## 1. Architecture du Projet
```
└───src
    ├───main : executable des tests
    ├───test
        ├───entiers.ml
        ├───let.ml
        ├───listes.ml
        ├───point_fixe.ml
        ├───produit.ml
        ├───somme.ml
        ├───test_types.ml
    ├───Makefile : pour buikd le projet
    ├───eval.ml : code de l'évaluateur
    └───type.ml : code du typeur
```

### Compilation et Exécution

Pour compiler et exécuter :
- **make** : Compile le projet, génère l'exécutable 'main'
- **make clean** : Supprime les fichiers générés pour une recompilation propre.
- **./main** : Exécute le fichier main, contenant tous les tests

## 2. Fonctionnalités Réalisées

### Partie 2 : Évaluateur pour le Lambda-Calcul Pur
- Implémentation d'un **évaluateur** pour le λ-calcul avec des stratégies de réduction basées sur la méthode **LtR-CbV** (gauche à droite, Call-by-Value).
- Les fonctionnalités principales incluent l'α-conversion, la substitution, et une fonction de normalisation pour réduire les termes à leur forme normale.
- Tests : Les exemples classiques (identité, fonctions récursives) ont été utilisés pour valider l'évaluateur.

### Partie 3 : Typage Simples
- Construction d'un système de **typage simple**, incluant des types flèches (`Arr`), des variables de type, et des types de base (`Nat`, `List`).
- Génération d’un système d’équations pour chaque terme.
- Implémentation d’un algorithme d’unification pour résoudre ces équations.
- Tests : Valider le processus de typage

### Partie 4 : Extensions du λ-Calcul
Enrichissement du λ-calcul avec plusieurs fonctionnalités supplémentaires :
- **Types avancés** :
  - **Somme (`Sum`)** : Représente une union de types (Gauche ou Droite). 
  - **Produit (`Prod`)** : Permet de grouper plusieurs valeurs.
- **Opérations sur les entiers** : Addition, soustraction, et multiplications avec des types associés.
- **Listes** : Avec les constructeurs `cons`, `head`, `tail`, et des branchements basés sur l'état de la liste.
- **Fixpoint** : Pour définir des fonctions récursives.
- **Typage Polymorphe** : Introduction des types universels (`ForAll`) pour capturer des expressions génériques.

Les termes enrichis ont été testés sur des cas pratiques, comme la définition de fonctions polymorphes, et des exemples utilisant les nouveaux types.

## 3. Difficultés Rencontrées

- **Gestion des substitutions** : Gérer correctement les substitutions dans des types comme `ForAll`, `Prod`, ou `Sum` a été compliqué, notamment pour éviter des conflits de noms de variables libres qui pouvaient entraîner des incohérences dans le typage.

- **Unification des types polymorphes** : Les types généralisés (`ForAll`). Il a fallu être vigilant pour renommer correctement les variables liées et éviter les chevauchements ou les bugs dans le processus.

- **Cohérence entre le typeur et l'évaluateur** : Assurer que le typeur et l'évaluateur fonctionnent bien ensemble. Il fallait s’assurer que tous les termes bien typés soient évalués correctement sans provoquer d’incohérences dans le système.

- **Conception et écriture des tests** : En générant des tests avec ChatGPT, certaines conception des fonctions n'étaient pas bonnes, reppaser dessus.

## 4. Méthodologie et Collaboration

- Collaboration avec Yok Yann Huynh et Marc Xu : les idées principales, la conception, et de nombreux fragments de code ont été développés conjointement. 
- Utilisation de ChatGPT pour générer et ajuster certains tests, on utilisera les mêmes fichiers de test
- Reprise et adaptation de nombreux exemples et algorithmes vus en cours.

## 5. Conclusion

J'ai pu avancé jusqu'à la partie 4 du projet, en enrichissant le système de typage et à intégrer des concepts avancés comme les types somme et produit.

Je n'ai pas eu le temps d'aborder la partie 5, qui concerne les traits impératif. 
Cela reste un aspect que j'aurais aimé approfondir pour obtenir un projet plus complet.

Ce projet m'a permis de mieux comprendre les mécanismes des langages fonctionnels, et je suis content des résultats obtenus. Il reste cependant des extensions qui auraient été intéressantes à implémenter.