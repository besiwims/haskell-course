Basic-IO

Voici la traduction en français :  

---

### Entrée/Sortie de base  
#### Plan  
- Fonctions pures  
- Introduction aux actions IO  
- Actions IO sous le capot  
- Actions IO en pratique  
- Le type `()`  
- Interaction avec l'utilisateur  
- `getChar`, `getLine` et `putStrLn`  
- Les actions sont des valeurs de première classe  
- Composer des actions IO (`>>` et `>>=`)  
- Le bloc `do`  
- Utilisation de `let`, imbrication de blocs `do`, sortie d'IO et `return`  
- L'action `main`  
- Récapitulatif  

---

### Fonctions pures  
Jusqu'à présent, nous avons travaillé avec des **fonctions pures**. Ces fonctions n'ont pas d'effets secondaires et prennent tous leurs arguments en entrée pour produire une valeur en sortie, qui dépend uniquement de ces arguments.

Ce que nous entendons par **entrée et sortie** est crucial ici. L'entrée d'une fonction ne peut être constituée que des valeurs fournies comme arguments, et la sortie est la valeur qu'elle retourne.

Par exemple :  

La fonction **lame** prend un seul paramètre numérique et retourne sa valeur multipliée par trois. La sortie de cette fonction dépend exclusivement de la valeur d'entrée. Ainsi, chaque fois que nous appliquons cette fonction à `4`, nous obtenons `12`.  

Même si nous ne montrons pas explicitement l’entrée de `max6`, nous savons que `filter` est appliqué partiellement et prend une liste d'entiers en entrée. Comme précédemment, la sortie dépend uniquement de l'entrée. Donc, chaque fois que nous appliquons `max6` à la même liste, nous obtenons le même résultat.  

Et bien sûr, en Haskell, les fonctions sont **curried**. Si une fonction semble prendre plusieurs arguments, elle prend en réalité un paramètre unique et retourne une fonction qui prend un autre paramètre, et ainsi de suite, jusqu'à ce que tous les paramètres soient appliqués et que nous obtenions la valeur finale.  

Tout va bien jusqu'à présent. **Mais que se passe-t-il si nous n'avons pas encore l'entrée ?**  

Et si nous voulons créer un **programme interactif** ? Un **site web** ? Un **jeu vidéo** ? Lorsque nous écrivons notre programme, nous n'avons **aucune idée** de ce que l'utilisateur va faire avec. Nous ne pouvons pas prévoir si un joueur déplacera son personnage à gauche ou à droite. Ou si un utilisateur sur un site web cliquera sur un bouton spécifique.  

Ces événements se produisent **pendant l'exécution du programme**, il est donc impossible pour le programmeur de les passer à l'avance comme arguments d'une fonction.  

Nous pourrions essayer, mais imaginez un jeu où tout est décidé à l'avance, sans aucune interaction possible. Ce ne serait plus un jeu, mais plutôt un **film**. Et même afficher une image sur l'écran nécessiterait d'envoyer et de recevoir des informations de l’écran de l’ordinateur **pendant l’exécution** du programme. Sans cela, un tel programme ne ferait rien... sauf chauffer votre ordinateur.  

La seule façon de fournir à notre programme les informations et capacités nécessaires est de lui donner **un moyen d'interagir avec le monde réel**.  

Et pour cela, **Haskell utilise les actions IO**.  

---

### Introduction aux actions IO  
Avant de commencer avec les **actions IO**, abordons un point important. Jusqu'à présent, je vous ai dit que tout ce que nous avons codé était **pur** et **n'avait aucune interaction** avec l'extérieur. Mais pourtant, nous avons **interagi** avec nos fonctions et **passé des arguments** dès la première leçon !  

C'est parce que nous avons **triché** en utilisant GHCi, qui effectue des **actions IO en arrière-plan** sans nous le dire explicitement. En réalité, si nous voulons que notre programme **interagisse avec le monde réel**, nous devons **utiliser des actions IO**.  

---

#### Qu'est-ce qu'une action IO ?  
Une **action IO (ou simplement "action" ou "computation")** est une opération qui peut **interagir avec le monde extérieur** et potentiellement le **modifier**.  

Le terme **IO action** peut prêter à confusion. Lorsqu'on parle d'IO actions, il ne s'agit **pas** des entrées et sorties des fonctions pures, mais bien de **l'interaction entre notre programme et le monde extérieur**.  

Les actions IO peuvent **modifier** le monde extérieur, mais elles ne sont pas **obligées** de le faire. C'est un point clé : elles **peuvent** interagir avec l'extérieur, mais elles **ne le font pas forcément**.  

Ces **actions IO** sont ce qu'on appelle des **effets de bord** (**side effects**).  

Un **effet de bord** est **tout effet observable autre que le retour d'une valeur**.  

Jusqu'à présent, toutes les fonctions que nous avons écrites étaient **pures**, car elles **ne faisaient que retourner une valeur**.  

Voyons quelques exemples pour mieux comprendre :  

- **Obtenir un texte tapé par un utilisateur** via le clavier  
- **Afficher du texte ou une image** sur l'écran  
- **Appeler une API ou une base de données**  

---

### Actions IO sous le capot  
Haskell utilise un type spécial pour garantir que l'interaction avec le monde réel reste **contrôlée et prévisible**. Ce type est défini comme suit :  

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

L’idée est que :  
1. L’action **prend l’état du monde réel** en entrée  
2. Elle **fait quelque chose**  
3. Elle retourne **un nouvel état du monde réel** et une **valeur de type `a`**  

Cela signifie que **chaque action IO transforme le monde réel** tout en produisant une valeur de retour.  

Bien entendu, ce n'est **pas réellement** ce qui se passe sous le capot, mais on peut s’imaginer le fonctionnement ainsi. En pratique, le type **IO** est un **type abstrait**, ce qui signifie que nous **n’avons pas besoin de comprendre** son implémentation exacte pour l’utiliser correctement.  

---

### Actions IO en pratique  

Le type `IO a` signifie qu'une action IO **interagit avec le monde réel** puis **retourne une valeur de type `a`**.  

Exemples :  

```haskell
action1 :: IO Bool      -- Effectue une action IO et retourne un Bool
action2 :: IO Int       -- Effectue une action IO et retourne un Int
action3 :: IO (Double -> Char) -- Effectue une action IO et retourne une fonction
action4 :: IO ()        -- Effectue une action IO et retourne ()
```

Trois points importants :  
1. **Une action IO produit une valeur après interaction avec le monde réel**  
2. **On ne peut pas obtenir cette valeur sans exécuter l’action**  
3. **Le type `()` représente un résultat dont on ne se soucie pas**  

---

### Interagir avec l'utilisateur  
Voici quelques fonctions de base :  

- `getChar :: IO Char` – Demande un caractère à l'utilisateur  
- `getLine :: IO String` – Demande une ligne de texte  
- `putStrLn :: String -> IO ()` – Affiche un texte à l'écran  

Exemple :  

```haskell
main :: IO ()
main = do
    putStrLn "Quel est ton nom ?"
    name <- getLine
    putStrLn ("Bonjour, " ++ name ++ " !")
```

Haskell exécute ces **actions IO** **dans l'ordre** et affiche le message après avoir reçu l’entrée de l’utilisateur.  

---
Voici la traduction en français :  

---

### Maintenant, c'est un bot bavard !  
Notre bot est maintenant complètement réhabilité, et il s'avère que c'est un bot bavard ! Ajoutons quelques fonctionnalités de conversation !  

Par exemple, ajoutons un message qui nous indique le nombre de lettres dans notre prénom :  

```haskell
lettersInName :: String -> String
lettersInName name =
  "Votre nom contient "
    ++ show (length name)
    ++ " lettres, au cas où vous vous poseriez la question..."
```

La fonction `lettersInName` est une fonction pure qui prend un nom et retourne un commentaire amusant à son sujet. Pour l'ajouter à notre bot bavard, nous devons le faire comme ceci :  

```haskell
chattyBot :: IO ()
chattyBot =
  putStrLn "Hey ! Quel est votre prénom ?"
    >> getLine
    >>= ( \name ->
            putStrLn ("Ravi de vous rencontrer, " ++ name ++ " !")
              >> putStrLn (lettersInName name)
        )
```

```haskell
chattyBot
<stdin>: hGetLine: end of file
```

Nous avons besoin de la valeur `name` (obtenue comme résultat de la seconde action), nous devons donc composer notre action à l'intérieur de cette fonction lambda.  

Et maintenant, c'est toujours la même logique. Nous pouvons continuer à ajouter plus d'actions de la même manière. Voici quelque chose d'un peu plus complexe :  

```haskell
finalChattyBot :: IO ()
finalChattyBot =
  putStrLn "Hey ! Quel est votre prénom ?"
    >> getLine
    >>= ( \name ->
            putStrLn ("Ravi de vous rencontrer, " ++ name ++ " !")
              >> putStrLn (lettersInName name)
              >> putStrLn ("Alors, " ++ name ++ ", que faites-vous pour vous amuser ?")
              >> getLine
              >>= ( \hobby ->
                      putStrLn ("Vous plaisantez, " ++ name ++ " ! J'adore " ++ hobby ++ " !")
                  )
        )
    >> putStrLn "OK, au revoir !"
```

```haskell
finalChattyBot
<stdin>: hGetLine: end of file
```

Comme vous pouvez le voir, si nous continuons à augmenter les interactions, nous commençons à voir un schéma. Un schéma laid et difficile à lire.  

Heureusement, nous avons une alternative plus élégante : **la notation `do`**.  

---

### La notation `do`  

La notation `do` est simplement du sucre syntaxique pour les expressions composées des opérateurs `>>` et `>>=`.  

Nous allons réécrire toutes les expressions précédentes avec la notation `do` pour voir la différence.  

Commençons par le bot impoli :  

```haskell
rudeBot :: IO ()
rudeBot = putStrLn "Hey!"
```

```haskell
rudeBotDo :: IO ()
rudeBotDo = do
    putStrLn "Hey!"
```

```haskell
rudeBotDo
Hey!
```

Comme vous pouvez le voir, nous écrivons le mot-clé `do` après le signe égal. Ensuite, nous commençons un bloc avec les actions.  

Dans ce cas, cette syntaxe n'est pas très utile. Il est plus simple de l'écrire sans la notation `do` !  

Voyons la deuxième version du bot impoli :  

```haskell
rudeBot :: IO ()
rudeBot = putStrLn "Hey!" >> putStrLn "Sortez de mon jardin !"
```

```haskell
rudeBotDo :: IO ()
rudeBotDo = do 
    putStrLn "Hey!"
    putStrLn "Sortez de mon jardin !"
```

```haskell
rudeBotDo
Hey!
Sortez de mon jardin !
```

Nous commençons maintenant à voir une légère amélioration. Ce n'est pas grand-chose, mais chaque action est sur une ligne distincte, ce qui les rend plus faciles à identifier.  

Sans la notation `do`, les actions vont de gauche à droite. Avec la notation `do`, elles vont de haut en bas.  

---

### Améliorons notre bot bavard  

Voyons maintenant comment utiliser la notation `do` pour le bot bavard :  

```haskell
chattyBot :: IO ()
chattyBot =
  putStrLn "Hey ! Quel est votre prénom ?"
    >> getLine
    >>= ( \name ->
            putStrLn ("Ravi de vous rencontrer, " ++ name ++ " !")
              >> putStrLn (lettersInName name)
        )
```

Avec la notation `do`, cela devient :  

```haskell
chattyBotDo :: IO ()
chattyBotDo = do
  putStrLn "Hey ! Quel est votre prénom ?"
  name <- getLine
  putStrLn ("Ravi de vous rencontrer, " ++ name ++ " !")
  putStrLn $ lettersInName name
```

```haskell
chattyBotDo
<stdin>: hGetLine: end of file
```

Maintenant, les différences deviennent évidentes ! Il faut quelques secondes pour comprendre `chattyBot`, alors que `chattyBotDo` est bien plus facile à suivre !  

Enfin, comparons le plus compliqué :  

```haskell
finalChattyBotDo :: IO ()
finalChattyBotDo = do
  putStrLn "Hey ! Quel est votre prénom ?"
  name <- getLine
  putStrLn ("Ravi de vous rencontrer, " ++ name ++ " !")
  putStrLn (lettersInName name)
  putStrLn ("Alors, " ++ name ++ ", que faites-vous pour vous amuser ?")
  hobby <- getLine
  putStrLn ("Vous plaisantez, " ++ name ++ " ! J'adore " ++ hobby ++ " !")
  putStrLn "OK, au revoir !"
```

---

### Utilisation de `let` dans la notation `do`  

Nous pouvons utiliser `let` dans un bloc `do` comme ceci :  

```haskell
unscramble :: IO ()
unscramble = do
  putStrLn "Écrivez un mélange de chiffres et de lettres :"
  arg <- getLine
  let numbers = filter (`elem` "1234567890") arg
      letters = filter (`notElem` numbers) arg
  putStrLn $ "Chiffres : " ++ numbers
  putStrLn $ "Lettres : " ++ letters
```

Quelques détails à noter :  
- Les variables définies avec `let` sont **paresseuses** et ne sont évaluées que lorsqu'elles sont utilisées.  
- Nous n'avons **pas besoin** du mot-clé `in` comme en dehors de `do`.  
- Nous pouvons aligner plusieurs définitions `let` sous le même niveau d'indentation.  

---

### La fonction `return` en Haskell  

En Haskell, `return` **ne fonctionne pas** comme dans d'autres langages impératifs.  

Voici un exemple :  

```haskell
twice :: IO String
twice = do
  str <- getLine
  let tw = str ++ str
  return tw
```

La fonction `return` en Haskell **n'interrompt pas** l'exécution, elle place simplement une valeur dans un contexte `IO`.  

---

### La fonction `main`  

En Haskell, l'exécution commence par la fonction `main`, qui doit toujours être de type `IO ()` :  

```haskell
main :: IO ()
main = putStrLn "Bonjour le monde !"
```

Ce programme affiche simplement **"Bonjour le monde !"** sur la console.  

---
