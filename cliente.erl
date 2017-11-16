-module(cliente). 
-export([pedir_taxi/2, pide_taxi/2]).
-import('matriz', [genera_nodo/1]).

% La funcion genera_nodo genera la matriz a donde se conectara, con todo y su hostname
% En este caso, se genera servidor_taxi@HOSTNAME
nodo_servidor() -> genera_nodo("servidor_taxi").
% nodo_taxi() -> genera_nodo("taxi").

pedir_taxi(Quien, {X, Y}) ->
    spawn(cliente, pide_taxi, [Quien, {X, Y}]).

% cliente 
pide_taxi(Quien, {X, Y}) ->
    Matriz = nodo_servidor(), 
    monitor_node(Matriz, true), 
    {servidor_taxi, Matriz} ! {pedir_cliente, Quien, {X, Y}, self()}, 
    receive
        {NumServicio, T_PID, Tipo, Placas} ->
        	io:format("~ts Se le asigno un auto tipo ~p con placas ~p. ~n", ["🚕",Tipo, Placas]),
        	io:format("~ts Su numero de servicio es: ~p | El T_PID de su taxi es: ~p. ~n", ["✔",NumServicio, T_PID]),
            monitor_node(Matriz, false),
            receive
            	{taxi, llega} ->
            		ok
            after wait() -> 
            	cancelar(T_PID)
            end;
        no_taxis_disponibles ->
            noTaxi;
        {nodedown, Matriz} ->
        	noServer
end.

cancelar(T_PID) -> T_PID ! {self(), cancelar}.

wait() -> rand:uniform(10000).