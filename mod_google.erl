%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%%
%% @doc Google integration. Adds Google login.

%% Copyright 2018 Marc Worrell
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

-module(mod_google).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Google").
-mod_description("Adds Google login.").
-mod_prio(390).

-export([
    observe_auth_logoff/3,
    event/2
]).
-export([
    get_config/1
]).

-include("zotonic.hrl").


% You have to add your Google appid and secret to the config.
% By default, we only request access to the Google user's e-mail address and basic userinfo.
-define(GOOGLE_SCOPE, "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email").


%% @doc Reset the received google access token (as set in the session)
observe_auth_logoff(auth_logoff, AccContext, _Context) ->
    z_context:set_session(google_logon, false, AccContext),
    z_context:set_session(google_access_token, undefined, AccContext).


%% @doc Return the google appid, secret and scope
-spec get_config(z:context()) -> {AppId::string(), Secret::string(), Scope::string()}.
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_google, appid, Context)),
      z_convert:to_list(m_config:get_value(mod_google, appsecret, Context)),
      z_convert:to_list(m_config:get_value(mod_google, scope, ?GOOGLE_SCOPE, Context))
    }.


event(#submit{message=admin_google}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_render:growl(?__("Saved the Google settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Google settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        K1 = z_convert:to_list(Key),
                        case is_setting(K1) of
                            true -> m_config:set_value(mod_google, list_to_atom(K1), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)).

is_setting("appid") -> true;
is_setting("appsecret") -> true;
is_setting("scope") -> true;
is_setting("useauth") -> true;
is_setting(_) -> false.

