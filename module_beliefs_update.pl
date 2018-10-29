:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.
:- dynamic baneado/3,life/2,lifeTotal/2,skill/2,lastAction/2,hasConTime/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

update_beliefs(Perc):-
	
	forall(member(time(_T),Perc),retractall(time(_))),
	forall((member(node(Id,Pos,Con),Perc),node(Id,Pos,Con)),retract(node(Id,Pos,Con))),
	forall((member(has(Ent1,Ent2),Perc),has(Ent1,_)),retractall(has(Ent1,_))),
	forall((member(at(Ent,_Nodo),Perc)),retractall(has(Ent,_))),
	forall((member(has(Ent1,Ent2),Perc),hasConTime(Ent1,_E2,_T1),time(T)),(retractall(hasConTime(Ent1,_,_)),assert(hasConTime(Ent1,Ent2,T)))),
	forall((at(Ent,IdNodo),member(node(IdNodo,_Pos,_Con),Perc)),retractall(at(Ent,_IdNodo3))),
	forall((member(at(Ent,_IdNodo),Perc),at(Ent,IdNodo1),not(member(node(IdNodo,_Pos1,_Con1),Perc))),retract(at(Ent,IdNodo1))),
	forall((member(atPos(Ent,_Pos2),Perc),atPos(Ent,_Pos3)),retractall(atPos(Ent,_))),
	forall((member(entity_descr(Ent,Desc),Perc),entity_descr(Ent,Desc1)),(retract(entity_descr(Ent,Desc1)),analizarDesc(Ent,Desc))),
	forall(member(Rel, Perc), assert(Rel)).

analizarDesc([agent,IdAgent],Desc):-
	forall(member([life,Life],Desc),(retractall(life(IdAgent,_)),assert(life(IdAgent,Life)))),
	forall(member([lifeTotal,LifeT],Desc),(retractall(lifeTotal(IdAgent,_)),assert(lifeTotal(IdAgent,LifeT)))),
	forall(member([skill,Skill],Desc),(retractall(skill(IdAgent,_)),assert(skill(IdAgent,Skill)))),
	forall(member([lastAction,LastA],Desc),(retractall(lastAction(IdAgent,_)),assert(lastAction(IdAgent,LastA)))).

analizarDesc([inn,IdPosada],Desc):-
	retractall(baneado(IdPosada,_,_)),
	forall(member([forbidden_entry,[IdAgent,Time]],Desc),assert(baneado(IdPosada,IdAgent,Time))).	

analizarDesc(_Ent,_Desc).

esGHI(inn).
esGHI(home).
esGHI(grave).
































