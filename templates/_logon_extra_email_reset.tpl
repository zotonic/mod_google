{% if 'google'|member:identity_types and m.config.mod_google.useauth.value and m.config.mod_google.appid.value %}
<p>{_ You have coupled your <strong>Google</strong> account. _} <a href="{% url logon use_absolute_url%}">Log on with Google</a></p>
{% endif %}
