{% if m.config.mod_google.useauth.value and m.config.mod_google.appid.value %}
<li id="logon_google">
	{% include "_google_login_link.tpl" %}
</li>
{% endif %}
