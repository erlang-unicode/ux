-module(ux_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
        ux_load_case/0,
        ux_load_case/1,
        ux_race_cond_case/0,
        ux_race_cond_case/1
]).


suite() ->
    [{timetrap, {minutes, 3}}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(main_group, Config) ->
    init_locations(Config);
init_per_group(_Group, Config) ->
    Config.

end_per_group(main_group, Config) ->
    end_locations(Config);
end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    application:start(ux),
    Config.

end_per_suite(Config) ->
    ok.

end_locations(_Config) ->
    ok.

init_locations(Config) ->
    %% Setup locations that some of the test cases use
%   DataDir = ?config(data_dir, Config),
    Config.

init_per_testcase(ux_race_cond_case, Config) ->
    ux:stop(),
    ux:start(),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Configuration
%% ----------------------------------------------------------------------



%% Tests
%% ----------------------------------------------------------------------
groups() ->
    [{main_group, [], [
                ux_load_case,
                ux_race_cond_case
    ]}].

all() ->
    [{group, main_group}].



ux_load_case() ->
    [{require, common_conf, ux_common_config}].

ux_race_cond_case() ->
    [{require, common_conf, ux_common_config}].


ux_load_case(Cfg) ->
    ux_string:to_lower([1090,1077,1089,1090]), %% тест
    ux_string:to_nfkc([1090,1077,1089,1090]), %% тест
    ok.


ux_race_cond_case(Cfg) ->
    [spawn_link(fun() ->
        ux_string:to_lower([1090,1077,1089,1090]), %% тест
        ux_string:to_nfkc([1090,1077,1089,1090]) %% тест
        end) || _ <- lists:seq(1, 20)].
