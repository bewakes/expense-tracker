from django.conf.urls import url
from expenses.views import *

urlpatterns=[
	 url(r'^$', IndexPage.as_view(), name="index"),
         # url(r'^login/$', login, name='login'),
         url(r'^logout/$', user_logout, name='logout'),
]
