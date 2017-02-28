from django.contrib.auth.models import User
from rest_framework import serializers

from expenses.models import AppUser, Category, Item, Expense, Organization


class UserSerializer(serializers.ModelSerializer):
    default_organization = serializers.SerializerMethodField(source='get_default_organization')
    def get_default_organization(self, user):
        orgs = user.organizations.filter(owner=user)
        if orgs:
            return OrganizationSerializer(orgs[0]).data
        return None

    class Meta:
        model = AppUser
        fields = ('id', 'username', 'email', 'address', 'has_setup', 'default_organization')

class CategorySerializer(serializers.ModelSerializer):
    class Meta:
        model = Category
        fields = ('id', 'organization', 'name', 'uses')

class ItemSerializer(serializers.ModelSerializer):
    class Meta:
        model = Item
        fields = ('id', 'organization', 'name', 'uses')

class OrganizationSerializer(serializers.ModelSerializer):
    class Meta:
        model = Organization
        fields = ('id', 'name')
