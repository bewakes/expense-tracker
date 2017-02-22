from django.conf.urls import url
from expenses.views import *

urlpatterns=[
	 url(r'^$', IndexPage.as_view(), name="index"),
	 url(r'^view/$', ViewExpenses.as_view(), name="view"),
	 #url(r'^add-items/$', AddItems.as_view(), name="view"),
	 url(r'^comment/$', comment_request, name="comment"),
	 url(r'^graph/$', show_graph, name="graph"),
]
