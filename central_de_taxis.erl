-module(central_de_taxis). 
-export([matriz/0]).
-import('matriz', [genera_nodo/1]).

matriz() -> genera_nodo("central_de_taxis").


% 'servidorTaxi@' ++ inet:gethostname() ++ '.local'. % funciones de interfase 


% crearCentral(Nombre, {X,Y}, )

% registro_central(Quien) ->
%     registro({registro_central,Quien, Locacion}).

% % cliente 
% registro(Mensaje) ->
%     Matriz = matriz(), 
%     monitor_node(Matriz, true), 
%     {servidor_taxi, Matriz} ! {self(), Mensaje}, 
%     receive
%         {servidor_taxi, Respuesta} ->
%             monitor_node(Matriz, false), 
%             Respuesta;
% 		{solicita_taxi, Quien} ->
% 			De ! {servidor_taxi, disponibilidad(Quien)};
%         {nodedown, Matriz} ->
%             no
% end.

% inicio() ->
%     register(central_de_taxis,
%         spawn(central_de_taxis, central_de_taxis, [[]])).

% disponibilidad(Quien) ->
	
	
	