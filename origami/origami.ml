(*Zadanie Origami*)
(*Autor kodu: Grzegorz Szumigaj*)
(*Code review: Konrad Skublicki*)

(*typ reprezentujący punkt na płaszczyźnie rzeczywistej o współżędnych (x, y)*)
type point = float * float;;

(*typ przechowyjący wektor w przestrzenie*)
type vector = float * float;;

(*typ reprezentujący kartkę origami*)
type kartka = point -> int;;

(*typ prsechowujący informację dla danego x o jego signum*)
type sign =
    | Zero
    | Positive
    | Negative;;

(*mały epsilon, do sprawdzania z dokładnością do niego czy proste współliniowe*)
let eps = 1e-9;; 

(*sprawdza znak danej liczby*)
let compare (x : float) =
    if x > 0. +. eps then Positive
    else if x < 0. -. eps then Negative
    else Zero;;

(*funkcja przyjmuje współżędne lewego dolnego i prawego dolnego wierzchołka prostokąta i
    zwraca kartkę, która sprawdza czy punkt znajduje się w tym prostokącie*)
let prostokat ((ldx, ldy) : point) ((pgx, pgy) : point) =
    let (pom : kartka) = function (x, y) ->
        if x >= ldx && x <= pgx && y >= ldy && y <= pgy
        then 1 else 0 in pom;;

(*funkcja przyjmuje współżędne środka koła i długość jego promienia i
    zwraca kartkę, która sprawdza czy punkt znajduje się w tym kole*)
let kolko ((sx, sy) : point) (r : float) =
    let (pom : kartka) = function (x, y) ->
        if sqrt ((x -. sx)*.(x -. sx) +. (y -. sy)*.(y -. sy)) <= r
        then 1 else 0 in pom;;


(*funkcja składa kartkę origami wzdłuż prostej z jej prawej storny na lewą*)
let zloz ((x1, y1) : point) ((x2, y2) : point) (f : kartka) =
    (*dla danych pynktów tworzy wektor który wskazuje z jednego punktu w drugi*)
    let make_v ((xa, ya) : point) ((xb, yb) : point) = 
        let (v : vector) = ((xa -. xb), (ya -. yb)) in v in
    (*funkcja odbija punkt na drugą stronę prostej*)
    let mirror ((x, y) : point) ((xa, ya) : point) ((xb, yb) : point) =
        let ((p_converted) : point) =
            if xa = xb (*przypadek zdegenerowanej funkcji liniowej*)
            then ((2. *. xa -. x), y)
            else 
                let a = (yb -. ya)/.(xb -. xa)
                and b = (ya *. xb -. xa *. yb)/.(xb -. xa) in
                let ny = y -. b in
                (((1. -. a *. a)/.(1. +. a *. a)) *. x +. ((2. *. a)/.(1. +. a *. a)) *. ny,
                ((2. *. a)/.(1. +. a *. a)) *. x -. ((1. -. a *. a)/.(1. +. a *. a)) *. ny +. b) in
        p_converted in
    (*funkcja sprawdza iloczyn wektorowy (wektory zaczepione będą zawsze w jednym miejscu)*)
    let iloczyn_wektorowy ((vx, vy) : vector) ((ux, uy) : vector) =
        vx *. uy -. ux *. vy in
    (*odbija punkt przez prostą w zależności od tego po której stronie jest*)
    let (pom : kartka) = function (x, y) -> 
        match compare (iloczyn_wektorowy (make_v (x2, y2) (x1, y1)) (make_v (x, y) (x1, y1))) with
        | Positive -> f (x, y) + f (mirror (x, y) (x1, y1) (x2, y2))
        | Negative -> 0
        | Zero -> f (x, y) in
    pom;;

(*identyczna do funkcji zloz*)
let zloz_fun (f : kartka) (p1 : point) (p2 : point) = 
    zloz p1 p2 f;;

(*aplikuję funkcję zloz wielokrotnie do danej kartki*)
let skladaj (t : (point * point) list) (f : kartka) =
    let (t1, t2) = List.split t in
    List.fold_left2 zloz_fun f t1 t2 ;;