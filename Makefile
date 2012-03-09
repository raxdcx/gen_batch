default: unit

compile:
	rebar compile

clean:
	rebar clean

unit:
	rebar eunit

analyze: compile plt
	dialyzer --plt app.plt -Wunderspecs -Wrace_conditions -Werror_handling -Wunmatched_returns ./ebin

plt: app.plt
	dialyzer --check_plt --plt app.plt

app.plt:
	dialyzer --build_plt --apps stdlib kernel --output_plt app.plt -r ebin
