### Programmation fonctionnelle vs Programmation impérative

Maintenant que nous avons examiné les fonctions en Haskell, explorons le concept de **programmation fonctionnelle**. En programmation fonctionnelle, la méthode de calcul de base est **l’application de fonctions à des arguments**. Par conséquent, la programmation fonctionnelle est mieux décrite comme un **style de programmation**, et les langages fonctionnels supportent et encouragent ce style.

En revanche, en **programmation impérative**, la méthode de calcul de base repose sur **la modification de valeurs stockées**. Autrement dit, les fonctions dans les langages impératifs ne sont pas des fonctions mathématiques pures, mais plutôt une **séquence d’instructions** que le programme doit suivre pour obtenir le résultat.

Pour mieux comprendre cela, voyons comment une tâche simple – **calculer la somme des nombres naturels entre 1 et n** – serait généralement traitée dans un langage impératif comme le C :

```c
int sum = 0;
for (i = 1; i <= n; i++) {
    sum = sum + i;
}
```

Ce programme commence par initialiser une variable `sum` à zéro, puis effectue une **boucle** (répète la même action) à travers tous les nombres de 1 à `n`, en **mettant à jour la valeur stockée** de la variable `sum` à chaque itération.
Dans le cas où `n = 3` :

```c
sum = 0;
{ première itération }
i = 1;
sum = 1;
{ deuxième itération }
i = 2;
sum = 3;
{ troisième itération }
i = 3;
sum = 6;
```

Voyons maintenant comment cette même tâche peut être réalisée en Haskell – nous pouvons y parvenir avec une **combinaison de deux fonctions** :

* `[n .. m]` – qui produit une liste de nombres de `n` à `m`, par exemple `[1..3]` donne `[1, 2, 3]`
* `sum` – qui calcule la **somme d’une liste**

La fonction `sum` est une **fonction prédéfinie** dans la bibliothèque standard de Haskell appelée **Prelude**, qui est automatiquement importée dans tous les modules Haskell. Les **bibliothèques** sont des collections de fonctions déjà écrites par d'autres pour résoudre divers problèmes, et que nous pouvons utiliser sans avoir à tout coder nous-mêmes.

```haskell
sum [1..3]
= { application de [..] }
sum [1, 2, 3]
= { application de sum }
1 + 2 + 3
= { application de + }
6
```

Cet exemple montre que **l'exécution de programmes Haskell déclenche une séquence d’applications de fonctions** – la méthode de calcul de base en programmation fonctionnelle.
En programmation fonctionnelle, le code **décrit ce qu’il faut calculer**, mais **pas explicitement comment arriver au résultat final** en suivant une séquence d’étapes.

Cela nous amène à un autre point important – **Haskell n’a pas d’affectation de variables**. Le signe égal `=` en Haskell **n’est pas un opérateur d’affectation** comme dans les langages impératifs, mais correspond plutôt **au signe égal mathématique**.

---

