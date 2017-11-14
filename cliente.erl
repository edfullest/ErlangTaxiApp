-module(cliente). 
-export([pedir_taxi/0]).
-import('matriz', [genera_nodo/1]).

% La funcion genera_nodo genera la matriz a donde se conectara, con todo y su hostname
% En este caso, se genera servidor_taxi@HOSTNAME
nodo_servidor() -> genera_nodo("servidor_taxi").
nodo_taxi() -> genera_nodo("taxi").
pedir_taxi(Quien, Location) ->
    pide_taxi({pedir, Quien, Location}).

% cliente 
pide_taxi(Mensaje) ->
    Matriz = nodo_servidor(), 
    monitor_node(Matriz, true), 
    {servidor, Matriz} ! {self(), Mensaje}, 
    receive
        {servidor, {T_PID, NumServicio, Auto, Placa}} ->
        	io:format("Se le asigno un auto tipo ~p con placas ~p.", [Auto,Placa]).
        	io:format("Su numero de servicio es: ~p | El T_PID de su taxi es: ~p.", [NumServicio, T_PID]).
            monitor_node(Matriz, false),
            receive
            	{taxi, llega} ->
            		ok
            after wait() -> 
            	cancelar(T_PID)
            end.

        {servidor, noTaxi} ->
            noTaxi;

        {nodedown, Matriz} ->
        	noServer
end.

cancelar(T_PID) ->
	Matriz = nodo_taxi(),
	{taxi, Matriz} ! {self(), cancelar}.


wait() -> random:uniform(10000).