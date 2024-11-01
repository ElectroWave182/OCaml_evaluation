Exercice 1 : Sur les listes
------------


1)

fonction '_a * '_a list -> int


let rec count = fun

(* Cas récursifs *)
(element, tete :: queue)
when element = tete ->
	count (element, queue) + 1

| (element, tete :: queue) ->
	count (element, queue)

(* Cas de base : [] *)
| _ -> 0
;;


count (2, [2 ; 1 ; -5 ; 2 ; 3 ; 2]) ;;
-> 3

count (2, [2 ; 2 ; 2]) ;;
-> 3

count (2, [1 ; -5 ; 3]) ;;
-> 0

count (2, []) ;;
-> 0


2)

fonction int * '_a -> '_a list


let rec repet = fun

(* Cas de base : aucune répétition *)
(0, _) -> []

(* Cas récursif *)
| (nbRep, element) ->
	let appel = repet (nbRep - 1, element)
	in
		element :: appel
;;


repet (7, 3) ;;
-> [3 ; 3 ; 3 ; 3 ; 3 ; 3 ; 3]

repet (7, `a`) ;;
-> [`a` ; `a` ; `a` ; `a` ; `a` ; `a` ; `a`]

repet (0, `a`) ;;
-> []


3)

3.1)

fonction int * '_a list -> '_a list


let rec nPremiersPositif = fun

(* Cas de base : 0Premiers *)
(0, _) -> []

(* Cas récursif *)
| (restant, tete :: queue) ->
	let appel = nPremiersPositif (restant - 1, queue)
	in
		tete :: appel

(* Cas d'erreur : restant > 0 et [] *)
| _ -> failwith "La liste est trop courte."
;;


3.2)

fonction int * '_a list -> '_a list


let nPremiers = fun

(* Cas d'erreur : restant négatif *)
(restant, _)
when restant < 0 ->
	failwith "Un nombre d'éléments négatif a été désigné."

| (restant, liste) ->
	nPremiersPositif (restant, liste)
;;


nPremiers (0, [2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8]) ;;
-> []

nPremiers (4, [2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8]) ;;
-> [2 ; 3 ; 4 ; 5]

nPremiers (4, [2 ; 3 ; 4]) ;;
-> Erreur

nPremiers (0, []) ;;
-> []


4)

4.1)

fonction int * int * '_a list -> '_a list


let rec trancheValable = fun

(* Cas de base : indice de fin dépassé *)
(indiceA, indiceB, _)
when indiceA < 1
& indiceB < 1
-> []

(* Cas récursif : entre les 2 indices *)
| (indiceA, indiceB, tete :: queue)
when indiceA <= 1
or indiceB <= 1
->
	let appel = trancheValable (indiceA - 1, indiceB - 1, queue)
	in
		tete :: appel

(* Cas récursif : avant l'indice de début *)
| (indiceA, indiceB, tete :: queue)
-> trancheValable (indiceA - 1, indiceB - 1, queue)

(* Cas d'erreur : indice de fin > 0 et [] *)
| _ -> failwith "La liste est trop courte."
;;


4.2)

fonction int * int * '_a list -> '_a list


let tranche = fun

(* Cas d'erreur : indice négatif *)
(indiceA, indiceB, _)
when indiceA <= 0
or indiceB <= 0 ->
	failwith "Un nombre d'éléments négatif a été désigné."

| (indiceA, indiceB, liste) ->
	trancheValable (indiceA, indiceB, liste)
;;


tranche (2, 4, [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7]) ;;
-> [2 ; 3 ; 4]

tranche (2, 6, [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7]) ;;
-> [2 ; 3 ; 4 ; 5 ; 6]

tranche (6, 2, [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7]) ;;
-> [2 ; 3 ; 4 ; 5 ; 6]

tranche (0, 5, [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7]) ;;
-> Erreur

tranche (1, 1, [7]) ;;
-> [7]

tranche (2, 9, [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7]) ;;
-> Erreur



Exercice 2 : Dans la cour de récréation
------------


type chifoumi =
	Pierre
	| Feuille
	| Ciseaux
;;


1)

fonction chifoumi -> chifoumi


let quiGagne = fun

Pierre -> Feuille
| Feuille -> Ciseaux
| Ciseaux -> Pierre
;;


quiGagne (Pierre) ;;
-> Feuille

quiGagne (Feuille) ;;
-> Ciseaux

quiGagne (Ciseaux) ;;
-> Pierre


2)

fonction chifoumi * chifoumi -> chifoumi


let duel = fun

(armeA, armeB)
when armeA = quiGagne (armeB)
-> armeA

| (armeA, armeB)
when quiGagne (armeA) = armeB
-> armeB

| _ -> failwith "Égalité !"
;;


duel (Feuille, Ciseaux) ;;
-> Ciseaux

duel (Feuille, Feuille) ;;
-> Erreur



Exercice 3 : Arbre binaire de recherche
------------


type arbre_binaire =
	Vide
	| Noeud of int * arbre_binaire * arbre_binaire
;;


let abr1 = Noeud (8, Noeud (3, Noeud (2, Vide, Vide), Noeud (6, Vide, Vide)), Noeud (19, Vide, Vide)) ;;

let abr2 = Noeud (5, Noeud (3, Noeud (2, Vide, Vide), Noeud (5, Vide, Vide)), Noeud (7, Vide, Noeud (8, Vide, Vide))) ;;


1)

fonction arbre_binaire -> int


let rec taille = fun

(* Cas de base : arbre nul *)
Vide -> 0

(* Cas récursif *)
| (Noeud (_, gauche, droite)) ->
	taille (gauche) + taille (droite) + 1
;;


taille (abr1) ;;
-> 5

taille (abr2) ;;
-> 6


2)

fonction int * arbre_binaire -> bool


let rec recherche = fun

(* Cas de base : trouvé *)
(element, (Noeud (valeur, _, _)))
when element = valeur
-> true

(* Cas récursif *)
| (element, (Noeud (_, gauche, droite))) ->
    recherche (element, gauche)
    or recherche (element, droite)

(* Cas de base : pas trouvé *)
| (_, Vide) -> false
;;


recherche (6, abr1) ;;
-> true

recherche (12, abr2) ;;
-> false


3)

fonction int * arbre_binaire -> arbre_binaire


let rec insertion = fun

(* Cas récursif : fils gauche *)
(element, (Noeud (valeur, gauche, droite)))
when element <= valeur
->
	Noeud (
		valeur,
		insertion (element, gauche),
		droite
	)

(* Cas récursif : fils droit *)
| (element, (Noeud (valeur, gauche, droite)))
->
	Noeud (
		valeur,
		gauche,
		insertion (element, droite)
	)

(* Cas de base : vide *)
| (element, _) ->
	Noeud (element, Vide, Vide)
;;


insertion (4, abr1) ;;
-> Noeud (8, Noeud (3, Noeud (2, Vide, Vide), Noeud (6, Noeud (4, Vide, Vide), Vide)), Noeud (19, Vide, Vide))

insertion (3, abr2) ;;
-> Noeud (5, Noeud (3, Noeud (2, Vide, Noeud (3, Vide, Vide)), Noeud (5, Vide, Vide)), Noeud (7, Vide, Noeud (8, Vide, Vide)))


4)

4.1)

fonction '_a list -> '_a list


let rec reverse = fun

(* Cas récursif *)
(tete :: queue) ->
	reverse (queue) @ [tete]

(* Cas de base : [] *)
| _ -> []
;;


4.2)

fonction int list -> arbre_binaire


let rec list_to_arbre_inverse = fun

(* Cas récursif *)
(tete :: queue) ->
	insertion (tete, list_to_arbre (queue))

(* Cas de base : [] *)
| _ -> Vide
;;


4.3)

fonction int list -> arbre_binaire


let list_to_arbre = fun

liste ->
	list_to_arbre_inverse
	(
		reverse (liste)
	)
;;


list_to_arbre ([8 ; 3 ; 2 ; 19 ; 6]) ;;
-> Noeud (8, Noeud (3, Noeud (2, Vide, Vide), Noeud (6, Vide, Vide)), Noeud (19, Vide, Vide))
