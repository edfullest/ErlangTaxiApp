-module(servidor). -export([inicio/0, servidor/1]).

matriz() -> 'servidor@DESKTOP-HQ62L6T.local'.
servidor(Datos) ->
    receive
        {De, {pedir, Quien, Locacion}} ->
            case busca_taxi(Datos) of
                indefinido ->
                    De ! {servidor_taxi, no}, servidor(Datos);
                {De2, {si, {Tipo,Placa}}}  ->
					De ! {De2, Tipo, Placa},

                    servidor(Datos));
                _ ->
                    De ! {servidor_banco, no}, servidor(Datos)
            end
		{De, {registro_central, Quien, Locacion}} ->
			registro(Quien,Locacion,Datos);
			servidor(Datos);
    end.

inicio() ->
    register(servidor_taxi,
        spawn(servidor_taxi, servidor, [[]])).


busca_taxi(Datos) ->
	{central_de_taxis, Matriz} ! {self(), Quien}.

registro(Quien, X, [{Quien, Locacion}|T]) ->
    [{Quien, X}|T];
registro(Quien, X, [H|T]) ->
    [H|deposita(Quien, X, T)];
registro(Quien, X, []) ->
    [{Quien, X}].