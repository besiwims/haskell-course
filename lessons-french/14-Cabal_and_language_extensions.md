Cabal_and_language_extensions

Here’s the French translation of your text:  

---

**Cabal et Extensions de Langage**  
### Plan  
#### Cabal  
- Introduction à Cabal  
- Création d'un nouveau projet Haskell  
- Exploration du fichier Cabal avec une bibliothèque externe  
- Compilation et exécution de notre exécutable  

#### Extensions de Langage et Pragmas  
- `NumericUnderscores`  
- `TypeApplications`  

Les leçons en vidéo et en écrit diffèrent car le format vidéo permet d'expliquer à travers des exemples, tandis que l'écrit est plus adapté aux explications séquentielles. Profitez de cela ! 😃 Si quelque chose ne vous semble pas clair dans un format, il le sera peut-être dans l'autre !  

---

### **Cabal**  
Cabal est un système de construction et de gestion des bibliothèques et programmes Haskell. Le nom Cabal signifie **Common Architecture for Building Applications and Libraries** (Architecture Commune pour la Construction d’Applications et de Bibliothèques).  

Le terme *Cabal* est polysémique. Il peut désigner :  
1. **cabal-the-spec** (les fichiers `.cabal`),  
2. **cabal-the-library** (le code qui interprète les fichiers `.cabal`),  
3. **cabal-the-tool** (le package `cabal-install` qui fournit l'exécutable `cabal`).  

Ces trois éléments fonctionnant généralement ensemble, nous les considérerons comme une seule entité, appelée **Cabal**.  

⚠️ Cette leçon suppose que vous avez déjà installé Cabal sur votre système. Si ce n’est pas le cas, reportez-vous à la leçon précédente *Installer Haskell Localement*.  

Cabal facilite la tâche des développeurs en leur permettant de :  
✅ Utiliser les packages d'autres développeurs  
✅ Créer et partager leurs propres bibliothèques  
✅ Configurer la compilation, l'exécution des tests et des exécutables  

Par défaut, Cabal utilise **Hackage** comme référentiel de bibliothèques. **Hackage** est une base centrale contenant des milliers de bibliothèques et programmes Haskell prêts à l’emploi.  

Cabal maintient un index local de Hackage pour résoudre les dépendances. Il est donc important de le mettre à jour régulièrement avec :  
```sh
cabal update
```
📌 [Voir la documentation ici](https://cabal.readthedocs.io/).  

---

## **Création d'un nouveau projet Haskell**  
Pour créer un nouveau projet Haskell selon l’architecture Cabal, utilisez la commande :  
```sh
cabal init
```
Cabal vous posera plusieurs questions sur votre projet. Pas d’inquiétude, tout peut être modifié plus tard dans le fichier `.cabal` !  

💡 **Avant d’exécuter `cabal init`**, créez un dossier pour votre projet et placez-vous dedans.  

Votre structure de projet pourrait ressembler à ceci :  
```
Projet
 ├── app
 │   ├── Main.hs
 ├── CHANGELOG.md
 ├── Projet.cabal
 ├── LICENSE
```
- `app/` contient le code source principal (ex : `Main.hs`).  
- `CHANGELOG.md` enregistre les modifications entre les versions.  
- `Projet.cabal` contient la configuration du projet.  
- `LICENSE` indique la licence du logiciel.  

Une structure plus courante dans les projets Haskell :  
```
Projet
 ├── app
 │   ├── Main.hs
 ├── src
 │   ├── ...
 ├── CHANGELOG.md
 ├── Projet.cabal
 ├── LICENSE
```
La seule différence ici est que le code source est principalement dans `src/`, et `app/` ne contient que l'exécutable principal.  

---

### **Commandes essentielles**
- **Compilation** :  
  ```sh
  cabal build
  ```
- **Exécution** :  
  ```sh
  cabal exec Projet
  ```
- **Compilation et exécution en une seule commande** :  
  ```sh
  cabal run
  ```
- **Exécution des tests** (non abordé ici) :  
  ```sh
  cabal test
  ```

---

## **Exploration du fichier `.cabal`**  
Le fichier `.cabal` contient des règles formatées de manière similaire aux fichiers YAML.  

Exemple :  
```cabal
cabal-version:      3.0
name:               ForestGame
version:            0.1.0.0
synopsis:           Un jeu sympa
description:        Ce jeu est vraiment super cool ! Faites-moi confiance.
license:            MIT
license-file:       LICENSE
author:             Robertino Martinez
maintainer:         robertino.martinez@iohk.io
category:           Game
build-type:         Simple
extra-doc-files:    CHANGELOG.md
```
La partie suivante définit notre exécutable :  
```cabal
executable ForestGame
    main-is:          Main.hs
    other-modules:    Forest.Level1
                    , User.Actions.Move
    build-depends:    base   ^>=4.16.4.0
                    , random ^>=1.2.1.1
    hs-source-dirs:   app, src
    default-language: Haskell2010
```
📌 **Explication rapide :**  
- `main-is`: fichier source principal  
- `other-modules`: modules auxiliaires  
- `build-depends`: bibliothèques requises  
- `hs-source-dirs`: dossiers contenant le code source  

---

## **Compilation et exécution de l’exécutable**  
Une fois votre `.cabal` bien défini, vous pouvez compiler votre projet avec :  
```sh
cabal build
```
Puis, exécuter le programme avec :  
```sh
cabal exec ForestGame
```
Ou encore plus simple, utiliser :  
```sh
cabal run
```
🔥 Cette commande **recompile automatiquement** les fichiers modifiés avant d’exécuter le programme !  

---

## **Extensions de Langage et Pragmas**  
Haskell permet d’activer des **extensions de langage** qui modifient son comportement. On peut les activer via :  
- Le fichier `.cabal`  
- Un **pragme de langage** (`LANGUAGE Pragma`) en haut du fichier concerné :  
  ```haskell
  {-# LANGUAGE extension_name #-}
  ```

### **Exemple 1 : NumericUnderscores**  
Cette extension permet d'ajouter des underscores `_` dans les nombres pour améliorer la lisibilité.  

```haskell
{-# LANGUAGE NumericUnderscores #-}

userGems = 15_894_231
tank = 314_159_265_358
```
👀 **Améliore la lisibilité sans affecter la valeur numérique !**  

---

### **Exemple 2 : TypeApplications**  
Cette extension permet d’indiquer explicitement le type d'une fonction polymorphe avec `@`.  

Sans `TypeApplications` :  
```haskell
read "4" :: Int  -- Doit spécifier Int
```
Avec `TypeApplications` :  
```haskell
{-# LANGUAGE TypeApplications #-}

read @Int "4"  -- Plus lisible !
```
🎯 **Cela permet de contrôler le type utilisé sans changer la structure du code !**  

---

## **Conclusion**  
Nous avons vu :  
✔️ Comment créer un projet Haskell avec Cabal  
✔️ Comment configurer et utiliser le fichier `.cabal`  
✔️ Comment compiler et exécuter un programme  
✔️ Comment utiliser les extensions `NumericUnderscores` et `TypeApplications`  

🎯 **À vous de jouer !** N’hésitez pas à tester ces concepts et à explorer la documentation officielle de Cabal ! 🚀  

---
