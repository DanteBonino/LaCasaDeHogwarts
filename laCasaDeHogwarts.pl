%Base de conocimientos:

caracteristicasDe(harry, sangreMestiza, [coraje,amistad,orgullo,inteligencia], [slytherin]).
caracteristicasDe(draco, sangrePura, [inteligencia, orgullo], [hufflepuff]).
caracteristicasDe(hermione, sangreImpura, [inteligencia, orgullo, responsabilidad], []).

casaApropiada(hufflepuff,[amistad]).
casaApropiada(gryffindor,[coraje]).
casaApropiada(slytherin,[inteligencia, orgullo]).
casaApropiada(ravenclaw, [inteligencia, responsabilidad]).

%Punto 1:
permiteEntrar(Casa,Persona):-
    casaApropiada(Casa,_),
    Casa \= slytherin,
    caracteristicasDe(Persona,_,_,_).
permiteEntrar(slytherin, Persona):-
    caracteristicasDe(Persona, TipoDeSangre,_,_),
    TipoDeSangre \= sangreImpura.

%Punto 2:
tieneCaracterApropiado(Persona, Casa):-
    casaApropiada(Casa, CaracteristicasImportantes),
    caracteristicasDe(Persona,_,_,_),
    forall(member(CaracteristicaImportante, CaracteristicasImportantes), tieneEstaCaracterisitca(Persona, CaracteristicaImportante)).

%Punto 3:
podriaSerSeleccionado(Persona, Casa):-
    permiteEntrar(Casa, Persona),
    tieneCaracterApropiado(Persona, Casa),
    not(odiaLaCasa(Persona, Casa)).
podriaSerSeleccionado(hermione,gryffindor).

odiaLaCasa(Persona,Casa):-
    caracteristicasDe(Persona,_,_,[Casa]).

%Punto 4:
/*
cadenaDeAmistades(Magos):-
    forall(member(Mago, Magos), esAmistosoYPuedeEstarEnLaMismaCasaQueElSiguiente(Mago, Magos)).

esAmistosoYPuedeEstarEnLaMismaCasaQueElSiguiente(Mago, Magos):-
    esAmistoso(Mago),
    puedeEstarEnLaCasaDelSiguiente(Mago, Magos).


esAmistoso(Mago):-
    tieneEstaCaracterisitca(Mago, amistad).

puedeEstarEnLaCasaDelSiguiente(Mago, Magos):-
*/

tieneEstaCaracterisitca(Persona, Caracteristica):-
    caracteristicasDe(Persona, _, Caracteristicas, _),
    member(Caracteristica, Caracteristicas).

/* Duda Punto 4 -> Todavía no sé en qué casa están*/

% cadenaDeCasas(Magos)
/*
cadenaDeCasas([Mago1, Mago2 | MagosSiguientes]):-
  puedeQuedarSeleccionadoPara(Mago1, Casa),
  puedeQuedarSeleccionadoPara(Mago2, Casa),
  cadenaDeCasas([Mago2 | MagosSiguientes]).
cadenaDeCasas([_]).
cadenaDeCasas([]).
*/

cadenaDeCasas(Magos):-
    forall(consecutivos(Mago1, Mago2, Magos),
           puedenQuedarEnLaMismaCasa(Mago1, Mago2, _)).
  
  consecutivos(Anterior, Siguiente, Lista):-
    nth1(IndiceAnterior, Lista, Anterior),
    IndiceSiguiente is IndiceAnterior + 1,
    nth1(IndiceSiguiente, Lista, Siguiente).
  
  puedenQuedarEnLaMismaCasa(Mago1, Mago2, Casa):-
    podriaSerSeleccionado(Mago1, Casa),
    podriaSerSeleccionado(Mago2, Casa),
    Mago1 \= Mago2.

/* Parte Dos */
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, irA(seccionRestringida)).
hizo(harry, irA(bosque)).
hizo(harry, irA(tercerPiso)).
hizo(draco, irA(mazmorras)).
hizo(ron, buenaAccion(50, ganarAlAjedrezMagico)).
hizo(hermione, buenaAccion(50, salvarASusAmigos)).
hizo(harry, buenaAccion(60, ganarleAVoldemort)).
hizo(hermione, responderPregunta(dondeSeEncuentraUnBezoar, 20, snape)).
hizo(hermione, responderPregunta(comoHacerLevitarUnaPluma, 25, flitwick)).

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(irA(Lugar), PuntajeQueResta):-
  lugarProhibido(Lugar, Puntos),
  PuntajeQueResta is Puntos * -1.
puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje).
puntajeQueGenera(responderPregunta(_, Dificultad, snape), Puntos):-
    Puntos is Dificultad // 2.
puntajeQueGenera(responderPregunta(_, Dificultad, Profesor), Dificultad):- Profesor \= snape.

lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringida, 10).
lugarProhibido(tercerPiso, 75).

%Punto 1:
%a
esBuenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

hizoAlgunaAccion(Mago):-
  hizo(Mago, _).
hizoAlgoMalo(Mago):-
  hizo(Mago, Accion),
  puntajeQueGenera(Accion, Puntaje),
  Puntaje < 0.

%b
esRecurrente(Accion):-
    hizo(UnaPersona, Accion),
    hizo(OtraPersona, Accion),
    UnaPersona \= OtraPersona.

%Punto 2:
puntajeTotal(Casa, PuntajeTotal):-
    esDe(_,Casa),
    findall(Puntos, puntosDeMiembroPorAccion(Casa, _, Puntos), PuntosTotales),
    sum_list(PuntosTotales, PuntajeTotal).


puntosDeMiembroPorAccion(Casa, Mago, Puntos):-
    esDe(Mago, Casa),
    hizo(Mago, Accion),
    puntajeQueGenera(Accion, Puntos).

    

%Punto 3:
ganadoraDeLaCopa(Casa):-
    puntajeTotal(Casa, PuntajeTotal),
    forall(puntajeTotal(OtrasCasa, OtroPuntajeTotal), PuntajeTotal >= OtroPuntajeTotal).

%Punto 4:
