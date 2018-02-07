%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
%% @doc Handle the OAuth redirect of the Google logon handshake.
%% See http://developers.facebook.com/docs/authentication/

%% Copyright 2010-2014 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(controller_google_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    html/1
    ]).

-include_lib("controller_html_helper.hrl").

html(Context) ->
    State = z_context:get_q("state", Context),
    Code = z_context:get_q("code", Context),
    SessionState = z_context:get_session(google_state, Context),
    html_1(Code, State, SessionState, Context).

html_1(Code, State, State, Context) when Code =/= undefined, Code =/= <<>>, Code =/= "" ->
    access_token(fetch_access_token(Code, Context), Context);
html_1(_Code, _State, _SessionState, Context) ->
    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
    html_error(cancel, Context1).

access_token({ok, AccessToken, TokenType, ExpiresIn}, Context) ->
    Data = [
        {access_token, AccessToken},
        {token_type, TokenType},
        {expires, z_datetime:timestamp() + ExpiresIn}
    ],
    user_data(fetch_user_data(AccessToken, TokenType), Data, Context);
access_token({error, _Reason}, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessTokenData, Context) ->
    case auth_user(UserProps, AccessTokenData, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[google] Undefined auth_user return for user with props ~p", [UserProps]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[google] Duplicate connection for user with props ~p", [UserProps]),
            html_error(duplicate, Context);
        {error, _} = Err ->
            lager:warning("[google] Error return ~p for user with props ~p", [Err, UserProps]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end;
user_data({error, _Reason}, _AccessData, Context) ->
    html_error(service_user_data, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "Google"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    Vars = [
        {service, "Google"},
        {error, Error},
        {auth_link, controller_google_authorize:redirect_location(Context)++"&auth_type=rerequest"}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).


auth_user(GProps, AccessTokenData, Context) ->
    GoogleUserId = proplists:get_value(<<"id">>, GProps),
    lager:debug("[google] Authenticating ~p ~p", [GoogleUserId, GProps]),
    PersonProps = [
        {title, proplists:get_value(<<"name">>, GProps)},
        {name_first, proplists:get_value(<<"given_name">>, GProps)},
        {name_surname, proplists:get_value(<<"family_name">>, GProps)},
        {website, proplists:get_value(<<"link">>, GProps)},
        {email, proplists:get_value(<<"email">>, GProps, <<>>)},
        {depiction_url, proplists:get_value(<<"picture">>, GProps)}
    ],
    Args = controller_google_authorize:get_args(Context),
    z_notifier:first(#auth_validated{
            service = google,
            service_uid = GoogleUserId,
            service_props = AccessTokenData,
            props = PersonProps,
            is_connect = z_convert:to_bool(proplists:get_value("is_connect", Args))
        },
        Context).

% Exchange the code for an access token
fetch_access_token(RequestToken, Context) ->
    {AppId, AppSecret, _Scope} = mod_google:get_config(Context),
    ContextNoLang = z_context:set_language(undefined, Context),
    RedirectUrl = z_context:abs_url(z_dispatcher:url_for(google_redirect, ContextNoLang), Context),
    Args = lists:flatten([
        "code=", z_utils:url_encode(RequestToken),
        "&client_id=", z_utils:url_encode(AppId),
        "&client_secret=", z_utils:url_encode(AppSecret),
        "&redirect_uri=", z_utils:url_encode(RedirectUrl),
        "&grant_type=authorization_code"
    ]),
    CT = "application/x-www-form-urlencoded",
    Options = [
        {body_format,binary}
    ],
    case httpc:request(post, {"https://accounts.google.com/o/oauth2/token", [], CT, Args}, [], Options) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            {struct, Qs} = mochijson:binary_decode(Payload),
            {ok, proplists:get_value(<<"access_token">>, Qs),
                 proplists:get_value(<<"token_type">>, Qs),
                 proplists:get_value(<<"expires_in">>, Qs)};
        Other ->
            lager:error("[google] error fetching access token [code ~p] ~p", [RequestToken, Other]),
            {error, {http_error, "https://accounts.google.com/o/oauth2/token", Other}}
    end.


% Given the access token, fetch data about the user
-spec fetch_user_data( AccessToken :: binary(), TokenType :: binary() ) -> list().
fetch_user_data(AccessToken, TokenType) ->
    case fetch_gdata(AccessToken, TokenType, "https://www.googleapis.com/oauth2/v1/userinfo") of
        {ok, Data} ->
            {struct, Props} = mochijson:binary_decode(Data),
            {ok, Props};
        Other ->
            Other
    end.

-spec fetch_gdata(string(), binary(), binary()) -> {ok, list()} | {error, term()}.
fetch_gdata(Token, TokenType, Url) ->
    Hs = [
        {"Authorization", z_convert:to_list(TokenType) ++ " " ++ z_convert:to_list(Token)}
    ],
    case httpc:request(get,{Url, Hs}, [], [{body_format,binary}]) of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            {ok, Payload};
        {ok, {{_, 401, _}, _Headers, Payload}} ->
            lager:info("Google error 401: ~p (url:~p)", [Payload, Url]),
            {error, oauth};
        {ok, {{_, 503, _}, _Headers, Payload}} ->
            lager:info("Google error 503: ~p (url:~p)", [Payload, Url]),
            {error, retry};
        Other ->
            lager:info("Google error: ~p (url:~p)", [Other, Url]),
            {error, Other}
    end.

