from django.conf.urls import url
from expenses.views import *

urlpatterns=[
	 url(r'^$', IndexPage.as_view(), name="index"),
         url(r'^login/$', login, name='login'),
         url(r'^logout/$', user_logout, name='logout'),
         url(r'^adduser/$', adduser, name='adduser'),
         url(r'^removeuser/$', removeuser, name='removeuser'),
         url(r'^(?P<backend>[^/]+)/login$', fblogin, name='fblogin'),
]


