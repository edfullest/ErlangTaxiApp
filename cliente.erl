-module(cliente). 
-export([consulta/1, deposita/2, retira/2]).
% nombre largo del servidor (nombre@máquina) 
matriz() -> 'servidor@DESKTOP-HQ62L6T.local'. % funciones de interfase 
pedir_taxi(Quien, Location) ->
    pide_taxi({pedir,Quien, Location}).

% cliente 
pide_taxi(Mensaje) ->
    Matriz = matriz(), 
    monitor_node(Matriz, true), 
    {servidor_taxi, Matriz} ! {self(), Mensaje}, 
    receive
        {servidor_taxi, Respuesta} ->
            monitor_node(Matriz, false), 
            Respuesta;
        {nodedown, Matriz} ->
            no
end.


