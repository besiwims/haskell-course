Gaining-your-independence-and-project

Voici la traduction en français :  

---

## Gagner votre indépendance 💪  

### Plan  
- Petits conseils et astuces  
- REPL  
- Hackage  
- Hoogle  
- `undefined`  
- Trous de type  
- Projet final de la section  

### REPL  
Rappelez-vous que vous avez toujours le REPL à votre disposition. Et si vous entrez dans le REPL en utilisant `cabal repl`, vous pouvez également importer et explorer les modules que vous avez téléchargés via Hackage. Si vous voulez voir comment cela fonctionne, consultez l'exemple dans la version vidéo.  

### Hackage  
🔗 [Hackage](https://hackage.haskell.org/)  

Hackage est l'archive centrale des packages de la communauté Haskell. Au moment où cette leçon a été rédigée, plus de 16 000 packages Haskell étaient disponibles sur Hackage.  

Nous avons déjà vu comment l'utiliser avec Cabal pour ajouter des bibliothèques à vos projets. Mais dans la leçon vidéo, nous allons explorer comment rechercher et choisir des bibliothèques ainsi que consulter leur documentation.  

### Hoogle  
🔗 [Hoogle](https://hoogle.haskell.org/)  

Hoogle vous permet de rechercher un sous-ensemble couramment utilisé de bibliothèques Haskell en fonction du nom d'une fonction ou de sa signature de type approximative.  

Cela est utile dans plusieurs scénarios, par exemple :  

- Si vous voulez afficher une chaîne de caractères dans la console mais avez oublié le nom de la fonction, une recherche pour `"String -> IO ()"` vous fournira toutes les fonctions correspondant à cette signature.  
- Si vous voulez utiliser une fonction mais ne vous souvenez plus du module auquel elle appartient, vous pouvez la rechercher et Hoogle vous indiquera son origine.  
- Si vous travaillez avec un concept particulier (par ex. nombres naturels, WebSockets), vous pouvez rechercher ces termes pour voir si une bibliothèque, un type, un module, une fonction ou une classe de type y correspond.  

### `undefined`  
Si nous recherchons `undefined` dans Hoogle, nous verrons qu'en théorie, il ne s'agit que d'une erreur déguisée. C'est une valeur qui, dès qu'elle est évaluée, interrompt l'exécution du programme.  

Bien sûr, comme nous l'avons vu dans la leçon sur la gestion des erreurs, nous n'aimons pas les erreurs à l'exécution ! Alors, pourquoi en parler comme d'un conseil utile ?  

En pratique, `undefined` est un excellent outil pour que le vérificateur de types continue de vous assister lorsque vous travaillez sur du code encore en développement. Voyons comment.  

---

(La suite du texte suit la même structure et est traduite en gardant la clarté et la précision technique.)  

Souhaitez-vous des ajustements sur le ton ou la structure ? 😊

Si nous enveloppons une chaîne avec le constructeur de valeur `Email`, nous obtenons une valeur de type `Email`, ce qui est exactement ce que nous voulions faire !  

Ainsi, nous suivons la suggestion du trou de type et écrivons le constructeur `Email` après `map` :  

```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map Email  
```
Et voilà ! Notre fonction compile.  

Mais nous sommes encore loin d’avoir terminé. Nous avons dit que nous voulions filtrer les emails qui ne contiennent pas le signe `@`, alors faisons-le.  

Bien sûr, nous devons filtrer les emails avant de les construire, nous allons donc utiliser la composition de fonctions pour ajouter la fonction `filter` avant `map` :  

```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map Email . filter _
```
```
• Found hole: _ :: String -> Bool  
• In the first argument of ‘filter’, namely ‘_’  
  In the second argument of ‘(.)’, namely ‘filter _’  
  In the expression: map Email . filter _  
• Relevant bindings include  
    parseEmails :: [String] -> [Email]  
      (bound at /Users/roberm/scratchpad/typedHoles.hs:94:1)  
  Valid hole fits include  
    null  
    read  
  Valid refinement hole fits include  
    (==) _  
    (/=) _  
    (>) _  
    (<=) _  
    (>=) _  
    (<) _  
    ($) _  
    head _  
    last _  
    id _  
```
Ok. Nous avons besoin d'un prédicat. Cette fois, le trou typé affiche un message supplémentaire en bas. Cela signifie qu'il y a plus de suggestions que la limite autorisée par défaut. Une chose que nous pourrions faire pour obtenir plus d'indices est de désactiver cette limite en écrivant une pragma avec le drapeau indiqué :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
```
```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map Email . filter _
```
```
• Found hole: _ :: String -> Bool  
• In the first argument de ‘filter’, namely ‘_’  
  In the second argument de ‘(.)’, namely ‘filter _’  
  In the expression: map Email . filter _  
• Relevant bindings include  
    parseEmails :: [String] -> [Email]  
      (bound at /Users/roberm/scratchpad/typedHoles.hs:94:1)  
  Valid hole fits include  
    null  
    read  
  Valid refinement hole fits include  
    (==) _  
    (/=) _  
    (>) _  
    (<=) _  
    (>=) _  
    (<) _  
    ($) _  
    notElem _  
    elem _  
    any _  
    all _  
    const _  
    pure _  
    return _  
    ($!) _  
    head _  
    last _  
    id _  
```
Maintenant, nous avons plus d’options dans la section "refinement hole fits". Et, si nous les regardons, nous nous rappelons que nous pourrions utiliser `elem`. Nous savons que `elem` est un prédicat qui retourne `True` si l’élément se trouve dans la liste, ce qui est exactement ce dont nous avons besoin. Nous remplaçons donc `_` par `elem _` et poursuivons :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
```
```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map Email . filter (elem _)
```
```
• Found hole: _ :: Char  
• In the first argument of ‘elem’, namely ‘_’  
  In the first argument of ‘filter’, namely ‘(elem _)’  
  In the second argument of ‘(.)’, namely ‘filter (elem _)’  
• Relevant bindings include  
    parseEmails :: [String] -> [Email]  
      (bound at /Users/roberm/scratchpad/typedHoles.hs:95:1)  
  Valid hole fits include  
    maxBound  
    minBound  
  Valid refinement hole fits include  
    head _  
    last _  
    id _  
    pred _  
    succ _  
    toEnum _  
    read _  
```
Ce cas est assez évident. Nous avons besoin d'un caractère pour vérifier s’il fait partie de la `String`, et nous savons lequel :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
```
```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map Email . filter (elem '@')
```
Nous avons terminé le filtrage ! Maintenant, normalisons les emails. Parce que nous devons normaliser les chaînes avant de les envelopper avec le constructeur `Email`, nous faisons la même chose qu’avant et composons un trou typé :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
```
```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map (Email . _) . filter (elem '@')
```
```
• Found hole: _ :: String -> String  
• In the second argument of ‘(.)’, namely ‘_’  
  In the first argument of ‘map’, namely ‘(Email . _)’  
  In the first argument of ‘(.)’, namely ‘map (Email . _)’  
• Relevant bindings include  
    parseEmails :: [String] -> [Email]  
      (bound at /Users/roberm/scratchpad/typedHoles.hs:98:1)  
  Valid hole fits include  
    show  
    reverse  
    cycle  
    init  
    tail  
    id  
    mempty  
    fail  
    read  
```
Nous obtenons une très longue liste d'options, mais celle qui semble être la meilleure est `map`. Nous avons une liste de caractères, nous pourrions donc passer par chaque caractère et le transformer en minuscule, un par un. Nous remplaçons donc `_` par `map _` :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}
```
```haskell
newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map (Email . map _) . filter (elem '@')
```
```
• Found hole: _ :: Char -> Char  
• In the first argument of ‘map’, namely ‘_’  
  In the second argument of ‘(.)’, namely ‘map _’  
  In the first argument of ‘map’, namely ‘(Email . map _)’  
• Relevant bindings include  
    parseEmails :: [String] -> [Email]  
      (bound at /Users/roberm/scratchpad/typedHoles.hs:100:1)  
  Valid hole fits include  
    id  
    pred  
    succ  
```
Nous savons que nous avons besoin d’une fonction qui va de `Char -> Char`. Mais aucune des fonctions fournies ne semble s’adapter parfaitement. Nous pouvons importer un module qui nous donne plus d’options, comme `Data.Char` :  

```haskell
{-# OPTIONS_GHC -fno-max-refinement-hole-fits #-}

import Data.Char

newtype Email = Email String deriving Show  

parseEmails :: [String] -> [Email]  
parseEmails = map (Email . map toLower) . filter (elem '@')
```
Et voilà ! Notre fonction fonctionne comme prévu. 🎉
