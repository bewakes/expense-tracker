from django.shortcuts import render, get_object_or_404, redirect
from django.http import HttpResponse, HttpResponseRedirect, Http404
from django.views.generic import View
from django.db.models import Q, Sum

from rest_framework import viewsets, status
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework.decorators import api_view

from datetime import timedelta
from django.contrib.auth import logout

from expenses.models import *
from expenses.serializers import *

import json, re

months = ['BAISAKH', 'JESTHA', 'ASHAR', 'SHRAWAN', 'BHADRA', 'ASHOJ', 'KARTIK', 'MANGSIR', 'POUSH', 'MAGH', 'FALGUN', 'CHAITRA']
EXPENSES_LIMIT = 5
TOP_LIMIT = 7

# Create your views here.

@api_view(['GET'])
def identity(request):
    if not request.user.is_authenticated():
        return Response({}, status=status.STATUS_401_UNAUTHORIZED)
    serializer = UserSerializer(request.user)
    return Response(serializer.data)


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
    queryset = Category.valid_objects.all()
    serializer_class = CategorySerializer
    permission_classes = [IsAuthenticated]

    def get_queryset(self):
        allcats = Category.valid_objects.all()
        if self.request.user.is_superuser:
            return allcats
        try:
            org = int(self.request.query_params.get('organization'))
        except:
            orgs = Organization.objects.filter(owner=self.request.user)
            if orgs:
                return allcats.filter(organization=orgs[0])
            return []
        return allcats.filter(organization_id=org)

class ItemViewSet(viewsets.ModelViewSet):
    """
    ViewSet for items
    """
    queryset = Item.valid_objects.all()
    serializer_class = ItemSerializer
    permission_classes = [IsAuthenticated]

    def get_queryset(self):
        allitems = Item.valid_objects.all()
        if self.request.user.is_superuser:
            return allitems
        try:
            org = int(self.request.query_params.get('organization'))
        except:
            orgs = Organization.objects.filter(owner=self.request.user)
            if orgs:
                return allitems.filter(organization=orgs[0])
            return []
        return allitems.filter(organization_id=org)


class ExpenseViewSet(viewsets.ModelViewSet):
    """
    ViewSet for expenses
    """
    queryset = Expense.valid_objects.all()
    serializer_class = ExpenseSerializer
    permission_classes = [IsAuthenticated]

    # def list(self, request, *args, **kwargs):
    #     queryset = self.get_queryset()

    def list(self, request):
        allexpenses = Expense.valid_objects.all()
        today = timezone.now().date()

        try:
            orgid = int(request.query_params.get('organization'))
        except:
            orgid = Organization.objects.filter(owner=request.user)[0].id

        top = request.query_params.get('top')
        if top:
            expenses = Expense.objects.filter(category__organization_id=orgid).\
                order_by('-cost')[:TOP_LIMIT]
            return Response(ExpenseSerializer(expenses, many=True).data)

        group_by = request.query_params.get('group_by') or 'date'


        fromDate = self.request.query_params.get('fromDate', None)
        toDate = self.request.query_params.get('toDate', None)

        forDate = request.query_params.get('forDate', None)
        toDate = request.query_params.get('toDate', None)

        try:
            offset = int(request.query_params.get('offset'))
        except:
            offset = 0
        limit = EXPENSES_LIMIT
        if not forDate and not fromDate and not toDate:
            return Response(Expense.objects.\
                filter(category__organization_id=orgid).\
                order_by('-date').\
                values(group_by).\
                annotate(total=Sum('cost'))[offset*limit:limit*(offset+1)])

        elif not fromDate or not toDate:
            expenses = Expense.objects.\
                filter(category__organization_id=orgid,date=forDate).\
                order_by('-date')
            return Response(ExpenseSerializer(expenses, many=True).data)
        else:
            return Response(Expense.objects.\
                filter(category__organization_id=orgid,date__gte=fromDate, date__lt=toDate).\
                order_by('-date').\
                values(group_by).\
                annotate(total=Sum('cost'))[0:5][offset:limit*(offset+1)])


class UserViewSet(viewsets.ModelViewSet):
    """
    ViewSet for users
    """
    queryset = AppUser.objects.all()
    serializer_class = UserSerializer
    permission_classes = [IsAuthenticated]

    def get_queryset(self):
        allusers = AppUser.objects.all()
        try:
            query = self.request.query_params.get('query')
            return allusers.filter(
                Q(username__contains=query) |
                Q(first_name__contains=query) |
                Q(last_name__contains=query)
            )
        except Exception as e:
            print(e)
            return allusers

class OrgUsersViewSet(viewsets.ViewSet):
    """
    ViewSet for org users
    """
    def list(self, request):
        """
        List the users for org
        """
        try:
            orgid = int(request.query_params.get('organization'))
        except Exception as e:
            return Response([])
        try:
            org = Organization.objects.get(id=orgid)
        except:
            return Response([])
        users = org.users
        serializer = UserSerializer(users, many=True)
        return Response(serializer.data)

class OrganizationViewSet(viewsets.ModelViewSet):
    """
    ViewSet for organization
    """
    queryset = Organization.objects.all()
    serializer_class = OrganizationSerializer
    permission_classes = [IsAuthenticated]

    def perform_create(self, serializer):
        user = self.request.user
        org = serializer.save()
        user.organizations.add(org)
        user.save()

@api_view(['POST'])
def adduser(request):
    """
    add user to organization
    """
    try:
        data = request.data
        org = Organization.objects.get(pk=data['organization'])
        user = AppUser.objects.get(pk=data['user'])

        if not org in request.user.organizations.all():
            return Response({"detail":"User does not have permission"}, status=status.HTTP_403_FORBIDDEN)

        if org in user.organizations.all():
            return Response({"detail":"Already added"}, status=status.HTTP_400_BAD_REQUEST)

        user.organizations.add(org)
        user.save()
        return Response({"detail":"user added"})
    except Exception as e:
        print(e)
        return Response({"detail":"invalid user/orgid"}, status=status.HTTP_400_BAD_REQUEST)

@api_view(['POST'])
def removeuser(request):
    """
    remove user from organization
    """
    try:
        data = request.data
        org = Organization.objects.get(pk=data['organization'])
        user = AppUser.objects.get(pk=data['user'])

        if request.user==user:
            return Response({"detail":"Can't remove yourself"}, status=status.HTTP_403_FORBIDDEN)
        if not org in request.user.organizations.all():
            return Response({"detail":"User does not have permission"}, status=status.HTTP_403_FORBIDDEN)
        user.organizations.remove(org)
        return Response({"detail":"user removed"})
    except Exception as e:
        print(e)
        return Response({"detail":"invalid user/orgid"}, status=status.HTTP_400_BAD_REQUEST)

def login(request):
    if not request.user.is_authenticated():
        return render(request, "expenses/login.html", {})
    else:
        return redirect('index')

def user_logout(request):
    logout(request)
    return redirect('login')
