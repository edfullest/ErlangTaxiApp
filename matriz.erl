-module(matriz). 
-export([genera_nodo/1]).

% Este es el nodo a donde se conectará la central 
genera_nodo(Servidor) -> case inet:gethostname() of
                        {ok, Hostname} ->
                        Servidor ++ Hostname ++ ".local"                        
                       end.