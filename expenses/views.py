from django.shortcuts import render, get_object_or_404, redirect
from django.http import HttpResponse, HttpResponseRedirect, Http404
from django.views.generic import View

from rest_framework import viewsets
from rest_framework.permissions import IsAuthenticated

import datetime
from django.contrib.auth import logout

from expenses.models import *
from expenses.serializers import CategorySerializer

import json, re

months = ['BAISAKH', 'JESTHA', 'ASHAR', 'SHRAWAN', 'BHADRA', 'ASHOJ', 'KARTIK', 'MANGSIR', 'POUSH', 'MAGH', 'FALGUN', 'CHAITRA']

# Create your views here.

class IndexPage(View):
    context = {}

    def get(self, request):
        if not request.user.is_authenticated():
            return redirect('login')
        return render(request, 'expenses/index.html', self.context)


class CategoryViewSet(viewsets.ModelViewSet):
    """
    ViewSet for categories
    """
    queryset = Category.objects.all()
    serializer_class = CategorySerializer
    permission_classes = [IsAuthenticated]


def login(request):
    if not request.user.is_authenticated():
        return render(request, "expenses/login.html", {})
    else:
        return redirect('index')

def user_logout(request):
    logout(request)
    return redirect('login')
