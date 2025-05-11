## Chargement des modules dans GHCi

Avant de continuer, voyons comment nous pouvons **charger les modules** que nous créons dans **GHCi**, afin de pouvoir utiliser les fonctions que nous définissons. Nous créons un **nouveau fichier**, par exemple **Practice.hs**, et nous y définissons notre module (n'oublions pas que le **nom du module** doit correspondre à **celui du fichier**), ainsi que notre fonction **triple** définie précédemment :

```haskell
module Practice where

triple x = 3 * x
```

Nous pouvons maintenant **lancer GHCi** depuis le terminal et **charger notre module** en utilisant son **chemin relatif** avec la commande **:load**, rendant ainsi la fonction accessible :

```sh
$ ghci
GHCi, version 8.10.2: https://www.haskell.org/ghc/ :? for help

{- Supposons que notre fichier Practice.hs se trouve dans le même répertoire
   depuis lequel nous avons lancé GHCi -}

ghci> :load Practice.hs
[1 of 1] Compiling Practice ( Practice.hs, interpreted )
Ok, un module chargé.

*Practice> triple 3
9
```

---

