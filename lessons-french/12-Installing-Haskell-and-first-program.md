Installing-Haskell-and-first-program

Voici la traduction en français :  

---

### Installer Haskell et créer notre premier programme  

Nous commençons à nous habituer à coder en Haskell, et il est temps de passer aux choses sérieuses. Dans cette série de leçons, nous allons apprendre à gérer notre environnement de développement, à créer des projets Haskell, à gérer les erreurs et à résoudre des problèmes en général. Ce sont des compétences de base que tout développeur Haskell doit posséder.  

Cette première leçon est courte. Nous allons configurer notre environnement de développement local et compiler notre premier programme.  

---

### Plan  

1. Installer Haskell  
2. Installer GHCup  
3. Installer GHC, Cabal, Stack et HLS avec GHCup  
4. Installer les extensions VSCode  
5. Créer notre premier programme  
   - Écrire le programme Haskell le plus simple  
   - Compiler et exécuter notre programme  

---

## Installer les outils Haskell (sur tous les systèmes d’exploitation)  

Vous pouvez ignorer cette section si vous ne souhaitez pas installer Haskell en local et préférez utiliser un environnement de développement en ligne.  

Nous avons besoin d’un moyen de transformer notre code source Haskell en code natif que notre ordinateur peut exécuter. Pour cela, nous avons besoin d’un compilateur. Le compilateur Haskell le plus utilisé est GHC. Voyons comment l’installer et l’utiliser.  

GHCi (l’environnement interactif) est fourni avec GHC. Il existe plusieurs façons d’installer GHC. Nous pourrions le télécharger directement depuis son site officiel, mais il existe de meilleures options, comme :  

- L’outil Stack  
- L’outil en ligne de commande GHCup  

Nous allons utiliser GHCup, car Stack fait plus que simplement installer les outils Haskell et nous voulons y aller étape par étape. Mais si vous préférez Stack, vous pouvez l’utiliser.  

Pour installer nos outils, rendez-vous sur le site web de GHCup et exécutez la commande indiquée dans votre terminal.  

Vous pouvez cliquer sur "Show all platforms" si votre système d’exploitation n’apparaît pas.  

Une fois la commande exécutée, elle vous posera quelques questions sur ce que vous souhaitez installer. Assurez-vous d’installer au moins GHC, Haskell Language Server et cabal-install.  

Et voilà ! Nous avons tout ce qu’il nous faut ! En supposant, bien sûr, que vous ayez un éditeur de texte. Et Microsoft Word ne compte pas.  

Si vous n’en avez pas, installez VSCode. C’est l’éditeur le plus utilisé et il est très convivial.  

Si VSCode vous propose d’installer des extensions, acceptez. Sinon, recherchez "Haskell" dans le panneau des extensions et installez les deux plus téléchargées.  

---

## Compiler des programmes Haskell  

Dans cette section, nous allons voir comment compiler des fichiers Haskell simples. Plus tard, nous verrons comment compiler un projet plus complexe avec Cabal.  

Dans la leçon précédente, nous avons vu l’un des programmes Haskell les plus courts que l’on puisse écrire :  

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

Un programme simple qui affiche "Hello World" sur la sortie standard. Rappelez-vous que tous vos programmes doivent contenir une action appelée `main`, qui fonctionne comme le point d’entrée du programme.  

OK ! Maintenant, compilons ce programme !  

Si vous êtes sur un environnement Jupyter Notebook, vous devrez utiliser des outils en ligne de commande. Mais vous pouvez aussi simplement écrire un fichier avec l’extension `.hs` et le compiler avec `ghc`, comme montré à la fin.  

Tout d’abord, enregistrons l’action `main` dans un fichier :  

```sh
:!echo "main = putStrLn \"Hello World!\"" >> hello.hs
```

Si nous recherchons un fichier Haskell, nous voyons qu’il est bien là :  

```sh
:!ls | grep 'hello'
hello.hs
```

Et si nous vérifions son contenu, nous trouvons uniquement l’action `main` :  

```sh
:!cat hello.hs
main = putStrLn "Hello World!"
```

OK, compilons-le !  

Pour compiler un fichier, il suffit de passer son chemin en argument à la commande `ghc` :  

```sh
:!ghc hello.hs
```

Sortie :  

```
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

Si nous recherchons des fichiers nommés `hello`, nous trouvons trois nouveaux fichiers :  

```sh
:!ls | grep 'hello'
hello
hello.hi
hello.hs
hello.o
```

- `hello.hs` est le fichier que nous avons créé avec le code source.  
- `hello.o` est un fichier objet et `hello.hi` est un fichier d’interface qui permet de lier le fichier objet aux bibliothèques fournies avec GHC pour produire un exécutable. Nous ne nous en soucions pas pour le moment.  
- Le fichier qui nous intéresse est `hello` (ou `hello.exe` sous Windows). C’est le véritable exécutable, un fichier binaire que nous pouvons exécuter comme n’importe quel autre programme.  

Alors, lançons-le :  

```sh
:! ./hello
```

Sortie :  

```
Hello World!
```

Et voilà ! Nous avons compilé et exécuté notre propre programme Haskell ! Félicitations !  

Bien sûr, comme GHCi est fourni avec GHC, nous pouvons aussi charger le fichier `hello.hs` dans GHCi (`ghci`) et expérimenter avec la fonction `main`.  

GHCi, le REPL Haskell, permet de charger un fichier Haskell avec la commande `:l`. Peu importe si le fichier contient une fonction `main` ou non. Une fois chargé dans GHCi, vous pouvez appeler toutes les fonctions et tester leur fonctionnement. Si vous chargez un fichier `main.hs` qui importe des modules définis par l’utilisateur, ils seront également inclus comme lors du processus de compilation.  

---

Nous avons codé en Haskell pendant un certain temps maintenant. Mais jusqu’à présent, tout était assez simple et court. C’est pourquoi, si vous avez suivi les exercices, nous avons toujours écrit tout notre code dans un seul fichier.  

Mais que se passe-t-il si nous développons une application plus complexe ? Comme un site web, un jeu ou une blockchain ? Combien de milliers de lignes de code illisibles contiendrait ce fichier unique ?  

La solution naïve serait de le diviser en plusieurs fichiers. Mais cela ne résout toujours pas de nombreux problèmes que nous pourrions rencontrer. C’est pourquoi nous utilisons des **modules**.  

---

C’est tout pour aujourd’hui ! 🎉
