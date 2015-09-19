from django.conf.urls import patterns, url
from expenses.views import *

urlpatterns=patterns('',
	 url(r'^$', IndexPage.as_view(), name="index"),
	 url(r'^view/$', ViewExpenses.as_view(), name="view"),
	 url(r'^add-items/$', AddItems.as_view(), name="view"),
	 url(r'^comment/$', comment_request, name="comment"),
)
