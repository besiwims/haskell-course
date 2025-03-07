Modules

Bien sûr ! Voici la traduction en français :  

---

# Modules  
## Plan  
- Importation de modules  
- Contrôle des environnements  
- Contrôle des espaces de noms  
- Création de nos propres modules  
- La bibliothèque Prelude et les bibliothèques standard  

Les leçons en vidéo et les leçons écrites diffèrent, car le format vidéo permet d'expliquer clairement les concepts en refactorisant le code, tandis que la version écrite est plus adaptée aux explications séquentielles. Profitez-en ! Si quelque chose ne vous semble pas clair dans un format, peut-être le sera-t-il dans l'autre !  

Grosso modo, un module Haskell est simplement une collection de fonctions, de types et de classes de types liés, que l'on peut importer et utiliser dans notre code. Mais ce n'est pas seulement cela.  

Les modules nous permettent de structurer, de réutiliser et de maintenir notre code et notre environnement.  

Mais avant d'apprendre à créer nos propres modules, voyons comment utiliser ceux qui sont pré-définis.  

---

## Importation de modules  

Nous allons importer plusieurs modules à plusieurs reprises dans cette leçon. Si vous exécutez les cellules de manière séquentielle, vous rencontrerez des erreurs là où vous ne devriez pas. Dans ces cas-là, redémarrez le noyau (dans le menu **Kernel** ci-dessus) pour supprimer toutes les importations et exécutez uniquement la cellule en cours, en ignorant les précédentes.  

Imaginons que votre application ait besoin de manipuler des fichiers et des dossiers. Nous pouvons utiliser un module appelé `System.Directory`, qui contient plusieurs fonctions, actions et types liés à la manipulation de fichiers et de répertoires.  

Pour importer ce module, on utilise le mot-clé `import` suivi du nom du module :  

```haskell
import System.Directory
```

Cela doit être fait avant de définir des fonctions, donc les importations se trouvent généralement en haut du fichier.  

En ajoutant cette ligne de code, nous avons accès à toutes les fonctions, actions, types et classes de types du module `System.Directory`. Vous pouvez consulter la documentation complète de ce module ici (lien).  

Une des fonctions fournies est `listDirectory` :  

```haskell
listDirectory :: FilePath -> IO [FilePath]
```

Elle prend en paramètre un chemin de type `FilePath` (qui est simplement un alias pour `String`) et renvoie une action IO qui, lorsqu'elle est exécutée, retourne une liste de tous les éléments (fichiers et dossiers) présents dans le répertoire indiqué.  

Par exemple, si nous l'utilisons pour voir le contenu du répertoire actuel dans ce **Jupyter Notebook**, nous obtenons :  

```haskell
import System.Directory

listDirectory "."
```

Sortie possible :  

```haskell
["23-State-Monad.ipynb","21-Reader-Monad.ipynb","24-Monadic-functions.ipynb", ...]
```

Comme vous pouvez le voir, le dossier actuel contient tous les fichiers des leçons précédentes.  

### Rechercher un fichier dans un répertoire  

Imaginons maintenant que nous voulions écrire une fonction pour trouver des fichiers dans le répertoire actuel qui contiennent une certaine chaîne de caractères dans leur nom.  

```haskell
import System.Directory

find' :: String -> IO [FilePath]
find' str = do
  entry <- listDirectory "."
  let found = -- filtrer les entrées
  return found
```

D'abord, nous récupérons la liste des fichiers et dossiers avec `listDirectory`, puis nous filtrons cette liste.  

Nous pourrions facilement créer notre propre fonction de filtrage avec du pattern matching et de la récursion. Mais, en réalité, c'est une fonction assez courante, donc elle existe sûrement déjà dans une bibliothèque !  

Effectivement, il existe un module appelé `Data.List` qui contient de nombreuses fonctions pour travailler avec les listes.  

L'une d'elles est `isInfixOf`. Elle prend deux listes et retourne `True` si la première liste est contenue, intacte et dans le même ordre, quelque part dans la seconde.  

C'est exactement ce dont nous avons besoin !  

```haskell
import System.Directory
import Data.List

find' :: String -> IO [FilePath]
find' str = do
  entry <- listDirectory "."
  let found = filter (str `isInfixOf`) entry
  return found

find' "11"
```

Sortie possible :  

```haskell
["11-Basic-IO.ipynb"]
```

Génial ! Grâce aux modules contenant du code pré-écrit, nous n'avons pas à tout coder nous-mêmes !  

---

## Contrôle de l’environnement  

Notre fonction fonctionne bien, mais son nom est un peu étrange. Pourquoi ne pas l’appeler simplement `find` au lieu de `find'` ?  

Si nous essayons de renommer notre fonction en `find` et de compiler ce code dans un programme Haskell standard, nous obtiendrons cette erreur :  

```
Ambiguous occurrence ‘find’
    It could refer to either ‘Data.List.find’ or ‘YourFileName.find’
```

Le problème est clair : il y a deux fonctions `find`, une provenant du module `Data.List` et une que nous avons définie. Le compilateur ne sait pas laquelle utiliser.  

Il existe plusieurs solutions :  

### Importer uniquement les fonctions nécessaires  

Une meilleure approche consiste à importer uniquement les fonctions ou types dont nous avons besoin au lieu du module entier :  

```haskell
import System.Directory (listDirectory) -- importer uniquement listDirectory
import Data.List (isInfixOf)            -- importer uniquement isInfixOf

find :: String -> IO [FilePath]
find str = do
  entry <- listDirectory "."
  let found = filter (str `isInfixOf`) entry
  return found
```

Si nous avons besoin d'importer plusieurs fonctions, nous les séparons par des virgules :  

```haskell
import Data.List (isInfixOf, sort) -- importer isInfixOf et sort
```

Cela nous permet d’éviter la pollution de notre environnement avec des éléments inutiles.  

### Cacher certaines fonctions lors de l’importation  

Si nous avons besoin de la plupart des fonctions d’un module mais qu’une seule pose problème, nous pouvons utiliser `hiding` :  

```haskell
import System.Directory (listDirectory)
import Data.List hiding (find)  -- importer tout Data.List sauf find
```

---

## Contrôle des espaces de noms  

Si nous avons deux modules avec des fonctions du même nom (par exemple `filter` de `Data.Map` et `filter` de `Prelude`), nous pouvons utiliser les **espaces de noms** avec `qualified` :  

```haskell
import qualified System.Directory (listDirectory) -- import qualifié

System.Directory.listDirectory "." -- Ceci fonctionne
listDirectory "." -- Ceci ne fonctionne plus
```

Nous devons maintenant utiliser `System.Directory.listDirectory`, ce qui évite toute confusion avec d'autres fonctions du même nom.  

Un autre exemple :  

```haskell
import Data.List hiding (find)
import System.Directory (listDirectory)
import qualified Data.Map

find :: String -> IO (Data.Map.Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Data.Map.fromList $ zip ([1 ..] :: [Int]) found -- Transformer en Map
  return foundMap
```

Sortie possible :  

```haskell
fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]
```

Et voilà ! Nous avons maintenant un meilleur contrôle sur notre environnement et nos importations en Haskell. 🎉


Nous n'avons ajouté qu'une seule ligne de code. Comme nous l'avons dit précédemment, les maps stockent des associations entre des clés uniques et des valeurs. Nous avons les valeurs, mais sans les clés !

Nous allons utiliser la fonction `zip` pour attribuer une clé unique à chaque valeur. Comme nous l'avons vu dans les devoirs de la leçon sur la récursion, la fonction `zip` prend deux listes et renvoie une liste de tuples contenant les paires correspondantes.

Nous associons une liste infinie de nombres ordonnés à partir de un avec la liste des entrées filtrées et triées. Ainsi, nous devrions obtenir une liste de paires où le premier élément est un nombre et le second est une entrée.

Heureusement, le module `Data.Map` fournit une fonction appelée `fromList`, qui prend une liste de paires et retourne une valeur de type `Map`. Dans ce cas, la valeur retournée est de type `Map Int String` car les clés sont des `Int` et les valeurs des `String`.

Grâce à cette dernière fonctionnalité, nous avons un contrôle total sur nos environnements. Cependant, écrire `Data.Map` partout devient vite lassant. Si nous qualifions les imports avec des noms longs ou plusieurs modules, notre code commence à être encombré et devient plus difficile à lire, comme cette phrase.

Haskell nous permet de renommer l'espace de noms pour le rendre plus pratique. Par exemple :

```haskell
import Data.List hiding (find)
import System.Directory (listDirectory)
import qualified Data.Map as Map -- Renommage de l’espace de noms
```

```haskell
find :: String -> IO (Map.Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Map.fromList $ zip ([1 ..] :: [Int]) found -- Liste vers Map
  return foundMap
```

```haskell
find "Creating"
fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]
```

Remarque : les noms des modules commencent par une majuscule. Si vous les renommez, le nouveau nom doit également commencer par une majuscule !

Et comme dernier conseil, nous pouvons combiner toutes ces techniques. Par exemple, si deux modules font à peu près la même chose et n'ont pas de conflits de noms, nous pourrions leur donner le même espace de noms et les traiter comme s'ils provenaient d'un seul module.

Cela ne s'applique pas pour l'instant, mais il existe une combinaison d'importations qui le permet. Notre fonction `find` est plutôt bien. Mais quelque chose me dérange : `Map.Map`. `Map.fromList` ne me dérange pas, en fait, je le préfère ! Cela me permet de savoir que `fromList` vient du module `Data.Map`. Mais `Map.Map` est redondant. Bien sûr que le constructeur de type `Map` vient du module `Data.Map` !

Évitons cette redondance en combinant quelques importations :

```haskell
import Data.List hiding (find)       
import System.Directory (listDirectory)
import qualified Data.Map as Map hiding (Map) -- Import qualifié + Renommage de l'espace de noms + Masquage de Map
import Data.Map (Map)                         -- Importation uniquement de Map
```

```haskell
find :: String -> IO (Map Int String)
find str = do
  entry <- listDirectory "."
  let found = sort $ filter (str `isInfixOf`) entry
  let foundMap = Map.fromList $ zip ([1 ..] :: [Int]) found
  return foundMap
```

```haskell
find "Creating"
fromList [(1,"08-Creating-non-parameterized-types.ipynb"),(2,"09-Creating-parameterized-and-recursive-types.ipynb"),(3,"10-Creating-Type-Classes.ipynb")]
```

En masquant le constructeur de type `Map` dans l'importation qualifiée et en l'important séparément, nous l'avons essentiellement retiré de l'espace de noms `Map` et ajouté à notre espace de noms principal.

Tout le reste reste inchangé, mais maintenant, la signature de `find` est plus lisible.

C'est à peu près tout sur l'importation des modules et la gestion de votre environnement. Mais souvenez-vous, nous avons dit que les modules nous permettent également de mieux structurer, réutiliser et maintenir notre code ? Voyons comment !

---

### Créer son propre module

Puisque les modules ne sont que des fichiers Haskell ordinaires qui peuvent être importés dans d'autres fichiers Haskell, il est facile d'en créer un soi-même.

Imaginons que nous voulions une autre version de la fonction `sum` qui renvoie une erreur si elle est appliquée à une liste vide au lieu de renvoyer `0`, comme le fait `sum`.

Pour créer un module qui expose une telle fonction, nous devons d'abord créer un fichier Haskell que nous appellerons `SumNonEmpty.hs`. En haut de ce fichier, nous écrivons une déclaration de module comme ceci :

```haskell
module SumNonEmpty where
```

Avec cette déclaration, nous avons défini le nom de notre module comme `SumNonEmpty`, qui, encore une fois, doit commencer par une majuscule.

Il est recommandé d'avoir le même nom pour le module et le fichier, bien que ce ne soit pas obligatoire.

Et maintenant, nous pouvons écrire le code que notre module fournit :

```haskell
module SumNonEmpty where

data MyData a b = Error a | Result b deriving (Show)

sumNonEmpty :: Num a => [a] -> MyData String a
sumNonEmpty [] = Error "List is empty"
sumNonEmpty xs = Result (sum xs)
```

Et voilà ! Nous avons créé notre propre module.

Nous pouvons maintenant l'importer dans un autre fichier (dans le même dossier) comme n'importe quel autre module :

```haskell
import SumNonEmpty
```

```haskell
sumNonEmpty []     -- Error "List is empty" 
sumNonEmpty [1..3] -- Result 6
```

Dans l'exemple précédent, le module exporté se trouve dans le même dossier que le fichier qui l'importe. Mais ils pourraient être placés à différents endroits. Dans ce cas, l'importation elle-même indique où se trouve le code.

Par exemple :

```haskell
import Data.Time.Calendar
import Data.Time.Clock.System
```

On peut en déduire que les fichiers sont organisés ainsi :

```
Data
  | 
  |--- Time
         |
         |--- Calendar.hs 
         |--- Clock
                 | 
                 |--- System.hs
```

---

### Contrôle des exports

Dans l'exemple précédent, notre module exporte tout ce qui est déclaré dans son fichier. Mais parfois, on ne veut pas tout exposer, comme dans le cas d'une fonction d'aide interne :

```haskell
module SumNonEmpty1 where

errorMessage1 = "List is empty"

data MyData1 a b = Error1 a | Result1 b deriving (Show)

sumNonEmpty1 :: Num a => [a] -> MyData1 String a
sumNonEmpty1 [] = Error1 errorMessage1
sumNonEmpty1 xs = Result1 (sum xs)
```

Ici, `errorMessage1` est accessible à l’extérieur, mais cela n’a pas de sens. La solution est d’indiquer explicitement ce que le module exporte :

```haskell
module SumNonEmpty2 (sumNonEmpty2, MyData2) where
```

Si nous voulons que les constructeurs du type `MyData2` soient accessibles, nous pouvons les exporter explicitement :

```haskell
module SumNonEmpty2 (sumNonEmpty2, MyData2(..)) where
```

Ou alors, nous pouvons fournir une fonction d’extraction :

```haskell
getResult :: (Num a) => a -> MyData3 String a -> a
getResult def (Result3 x) = x
getResult def _           = def
```

---

### Le module Prelude et les bibliothèques standard

Le module `Prelude` est importé par défaut et fournit des fonctions de base comme `head`, `sum`, et `length`. 

De plus, Haskell propose des bibliothèques standard intégrées, que l'on peut explorer via [Hoogle](https://hoogle.haskell.org/).

---

C'est tout pour aujourd'hui ! 🚀
