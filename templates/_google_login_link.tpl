{% with
    "#000"
    as
    brand_color
%}
{% if is_connect and 'google'|member:identity_types %}
	<a id="{{ #fbdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: {{ brand_color }}"><span class="z-icon z-icon-google-plus"></span> {_ Disconnect from Google _}</a>
	{% wire id=#fbdis
			action={confirm title=_"Disconnect from Google"
							text=_"Do you want to disconnect your Google account?"
							ok=_"Disconnect"
							action={auth_disconnect id=m.acl.user type="google"}
					}
	%}
{% else %}
	<a href="{% url logon_service service='google' is_connect=is_connect %}" target="_blank" class="btn z-btn-social do_popupwindow" data-popupwindow="height:300" style="background-color: {{ brand_color }}"><span class="z-icon z-icon-google-plus"></span> {% if is_connect %}{_ Connect with Google _}{% else %}{_ Log in with Google _}{% endif %}</a>
{% endif %}
{% endwith %}
