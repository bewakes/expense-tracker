"""domestic URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/1.10/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  url(r'^$', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  url(r'^$', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.conf.urls import url, include
    2. Add a URL to urlpatterns:  url(r'^blog/', include('blog.urls'))
"""
from django.conf.urls import url, include
from django.contrib import admin

from expenses.views import *
from .hook import webhook

from rest_framework import routers

router = routers.SimpleRouter()
router.register(r'categories', CategoryViewSet)
router.register(r'items', ItemViewSet)
router.register(r'users', UserViewSet)
router.register(r'orgusers', OrgUsersViewSet, base_name='orgusers')
router.register(r'organizations', OrganizationViewSet)
router.register(r'expense', ExpenseViewSet)
router.register(r'income', IncomeViewSet)
router.register(r'feedback', FeedbackViewSet)

urlpatterns = [
    url(r'^ulala/', admin.site.urls),
    url(r'^$', include('expenses.urls')),
    url(r'^expenses/', include('expenses.urls')),
    url('', include('social_django.urls', namespace='social')),
    #url(r'^login/', login, name='login'),
    url(r'^identity/', identity, name='identity'),
    url(r'^webhook/', webhook, name='webhook'),
]
urlpatterns += router.urls
