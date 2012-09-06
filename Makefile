default: unit

compile:
	rebar compile

clean:
	rebar clean

unit:
	rebar eunit

analyze: compile app.plt
	dialyzer --plt app.plt -Wunderspecs -Werror_handling -Wunmatched_returns ./ebin

app.plt:
	dialyzer --build_plt --apps erts stdlib kernel --output_plt app.plt
