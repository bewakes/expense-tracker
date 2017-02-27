from django.contrib.auth.models import User
from rest_framework import serializers

from expenses.models import AppUser, Category, Item, Expense, Organization


class UserSerializer(serializers.ModelSerializer):
    class Meta:
        model = AppUser
        fields = ('id', 'username', 'email', 'address', 'has_setup')

class CategorySerializer(serializers.ModelSerializer):
    class Meta:
        model = Category
        fields = ('id', 'user', 'name', 'uses')
