{% wire id="admin_google" type="submit" postback="admin_google" delegate=`mod_google` %}
<form name="admin_google" id="admin_google" class="form-horizontal" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-google"></span> Google</h3>
                <div class="widget-content">
                    <p class="help-block">
                        {_ Application keys can be found in _} <a href="https://console.developers.google.com/apis/dashboard" title="Developer Dashboard" target="_blank">{_ Your Google Developer Dashboard _}</a>
                    </p>

                    <p class="help-block">
                        {_ The OAuth Authorization Callback URL is _}
                        <tt>{% url google_redirect use_absolute_url z_language=`undefined` %}</tt>
                    </p>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="google_app_id">{_ Cient-ID _}</label>
                        <div class="col-md-9">
                            <input type="text" id="google_appid" name="appid" value="{{ m.config.mod_google.appid.value|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="google_appsecret">{_ Client Secret _}</label>
                        <div class="col-md-9">
                            <input type="text" id="google_appsecret" name="appsecret" value="{{ m.config.mod_google.appsecret.value|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="google_scope">{_ Scope _}</label>
                        <div class="col-md-9">
                            <input type="text" id="google_scope" name="scope" value="{{ m.config.mod_google.scope.value|default:'https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email'|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" id="google_useauth" name="useauth" {% if m.config.mod_google.useauth.value %}checked="checked"{% endif %} value="1" />
                                    {_ Use Google authentication _}
                                </label>
                            </div>
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9  col-md-offset-3">
                            <button class="btn btn-primary" type="submit">{_ Save Google Settings _}</button>
                        </div>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>
