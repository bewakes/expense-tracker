import datetime
import random
import string
import json

from django.shortcuts import render, redirect
from django.http import HttpResponse
from django.views.generic import View
from django.db.models import Q, Sum

from rest_framework import viewsets, status
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from rest_framework.decorators import api_view

from django.contrib.auth import logout
from django.utils import timezone

from expenses.models import (
    Expense, Item, Organization, Category, Income, AppUser, Feedback, Token
)
from expenses.serializers import (
    ExpenseSerializer, ItemSerializer, CategorySerializer, IncomeSerializer,
    UserSerializer, OrganizationSerializer, FeedbackSerializer
)

from django.contrib.auth import login as auth_login
from social_django.utils import psa

months = [
    'BAISAKH', 'JESTHA', 'ASHAR', 'SHRAWAN', 'BHADRA', 'ASHOJ',
    'KARTIK', 'MANGSIR', 'POUSH', 'MAGH', 'FALGUN', 'CHAITRA'
]
EXPENSES_LIMIT = 5
TOP_LIMIT = 7

# Create your views here.


@api_view(['GET'])
def identity(request):
    if not request.user.is_authenticated:
        return Response({}, status=status.HTTP_401_UNAUTHORIZED)
    serializer = UserSerializer(request.user)
    return Response(serializer.data)


class IndexPage(View):
    context = {}

    def get(self, request):
        if not request.user.is_authenticated:
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


class IncomeViewSet(viewsets.ModelViewSet):
    """
    ViewSet for incomes
    """
    queryset = Income.objects.all()
    serializer_class = IncomeSerializer
    permission_classes = [IsAuthenticated]


def datediff(days):
    return datetime.datetime.now().date() - datetime.timedelta(days=days)


DURATIONS = {
    'week': lambda x: datediff(7*x),
    'month': lambda x: datediff(30*x),
    'year': lambda x: datediff(365*x),
}


class ExpenseViewSet(viewsets.ModelViewSet):
    """
    ViewSet for expenses
    """
    queryset = Expense.objects.all()
    serializer_class = ExpenseSerializer
    permission_classes = [IsAuthenticated]

    def list(self, request):
        try:
            orgid = int(request.query_params.get('organization'))
        except Exception as e:
            orgid = Organization.objects.filter(owner=request.user)[0].id

        top = request.query_params.get('top')
        if top:
            expenses = Expense.objects.filter(
                    category__organization_id=orgid
                ).\
                order_by('-cost')[:TOP_LIMIT]
            return Response(ExpenseSerializer(expenses, many=True).data)

        group_by = request.query_params.get('group_by') or 'date'

        fromDate = self.request.query_params.get('fromDate', None)
        toDate = self.request.query_params.get('toDate', None)

        forDate = request.query_params.get('forDate', None)
        toDate = request.query_params.get('toDate', None)

        duration = request.query_params.get('duration')
        if duration:
            try:
                num = int(request.query_params.get('n'))
            except (ValueError, TypeError):
                num = 1
            now = datetime.datetime.now().date()
            fromDate = DURATIONS.get(duration, lambda x: now)(num)
            toDate = now + datetime.timedelta(days=1)

        individual = request.query_params.get('individual')
        if individual:
            filterargs = {
                'category__organization_id': orgid
            }
            if fromDate:
                filterargs['date__gte'] = fromDate
            if toDate:
                filterargs['date__lte'] = toDate
            expenses = Expense.objects.filter(
                **filterargs
            ).order_by('-date')
            return Response(ExpenseSerializer(expenses, many=True).data)

        query = request.query_params.get('query')
        orfilter = Q(category__organization_id=orgid)
        if query:
            orfilter = Q(category__name__icontains=query) |\
                Q(items__icontains=query) |\
                Q(description__icontains=query)

        try:
            offset = int(request.query_params.get('offset'))
        except Exception as e:
            offset = 0
        limit = EXPENSES_LIMIT
        if not forDate and not fromDate and not toDate:
            print('if not forDate and not fromDate and not toDate')
            return Response(Expense.objects.
                filter(Q(category__organization_id=orgid), orfilter).
                order_by('-date').
                values(group_by).
                annotate(total=Sum('cost'))[offset*limit:limit*(offset+1)])
        elif not fromDate or not toDate:
            print('not fromDate or not toDate')
            expenses = Expense.objects.\
                filter(Q(
                    category__organization_id=orgid,
                    date=forDate
                    ), orfilter
                ).\
                order_by('-date')
            return Response(ExpenseSerializer(expenses, many=True).data)
        else:
            print('else', fromDate, toDate)
            return Response(Expense.objects.
                filter(Q(
                    category__organization_id=orgid,
                    date__gte=fromDate,
                    date__lt=toDate
                    ), orfilter
                ).
                order_by('-date').
                values(group_by).
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


class FeedbackViewSet(viewsets.ModelViewSet):
    queryset = Feedback.objects.all()
    serializer_class = FeedbackSerializer
    permission_classes = [IsAuthenticated]


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
    if not request.user.is_authenticated:
        return render(request, "expenses/login.html", {})
    else:
        return redirect('index')


def user_logout(request):
    logout(request)
    return redirect('login')


@psa('social:complete')
def fblogin(request, backend):
    token = request.GET.get('access_token')
    user = request.backend.do_auth(token)
    if user:
        auth_login(request, user)
        return HttpResponse('{"status":true}', content_type="application/json")
    else:
        return HttpResponse('', status_code=403)  # 'ERROR'

def get_token(request):
    if not request.user.is_authenticated:
        return HttpResponse(json.dumps({}), content_type="application/json")
    # Check for token
    token = Token.objects.filter(app_user=request.user).first()
    if not token:
        now = timezone.now()
        token = Token.objects.create(
            app_user=request.user,
            value=Token.get_random_string(),
            expiry=now + datetime.timedelta(days=30),
        )
    # TODO: check for expiry and auto renew
    data = {
        'value': token.value,
        'expiry': token.expiry.strftime('%Y-%m-%d')
    }
    return HttpResponse(json.dumps(data), content_type="application/json")
