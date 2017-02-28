from django.db import models
from django.contrib.auth.models import User

class Organization(models.Model):
    name = models.CharField(max_length=100)
    owner = models.OneToOneField('AppUser')
    is_individual = models.BooleanField(default=True)

    def __str__(self):
        return '{}'.format(self.name)

class AppUser(User):
    address = models.CharField(max_length=100, blank=True, null=True)
    occupation = models.CharField(max_length=100, blank=True, null=True)
    organizations = models.ManyToManyField(Organization, null=True, related_name="users")
    has_setup = models.BooleanField(default=False) # means user has set up for individual or organizational record


class CategoryManager(models.Manager):
    def get_queryset(self):
        return super().get_queryset().filter(is_deleted=False)

class Category(models.Model):
    objects = models.Manager()
    valid_objects = CategoryManager()

    organization = models.ForeignKey(Organization, related_name='categories')
    name = models.CharField(max_length=50)
    uses = models.IntegerField(default=0)
    is_deleted = models.BooleanField(default=False)

    def __str__(self):
        return self.name

    def delete(self):
        self.is_deleted = True
        self.save()


class ItemsManager(models.Manager):
    def get_queryset(self):
        return super().get_queryset().filter(is_deleted=False)

class Item(models.Model):
    objects = models.Manager()
    valid_objects = ItemsManager()

    name = models.CharField(max_length=50)
    category = models.ForeignKey(Category, null=True)
    organization = models.ForeignKey(Organization, related_name='items')
    uses = models.IntegerField(default=0)
    is_deleted = models.BooleanField(default=False)

    def __str__(self):
        return self.name

    def delete(self):
        self.is_deleted = True
        self.save()


class Expense(models.Model):
    date = models.DateTimeField('date')
    item = models.ForeignKey(Item)
    comment = models.CharField(max_length=1000)
    cost = models.IntegerField(default=0)
    user = models.ForeignKey(AppUser)
    is_deleted = models.BooleanField(default=False)

    def __str__(self):
        return '{} {} - {}'.format(str(self.date), str(self.cost), self.item.name)

    def delete(self):
        self.is_deleted = True
        self.save()


# not used currently
class ItemExpense(models.Model):
    item = models.ForeignKey(Item, null=True)
    expense = models.ForeignKey(Expense, null=True)
    quantity = models.FloatField(default=1)
    cost = models.IntegerField(default=0)

    def __str__(self):
        return str(self.expense.date) +" "+ self.item.name+" "+str(self.cost)
