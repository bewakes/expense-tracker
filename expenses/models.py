from django.db import models
from django.contrib.auth.models import User

class Organization(models.Model):
    name = models.CharField(max_length=100)
    user = models.OneToOneField('AppUser')

class AppUser(User):
    address = models.CharField(max_length=100, blank=True, null=True)
    occupation = models.CharField(max_length=100, blank=True, null=True)
    # profile_image = models.ImageField(null=True)


class Category(models.Model):
    user = models.ForeignKey(AppUser)
    name = models.CharField(max_length=50)
    uses = models.IntegerField(default=0)

    def __str__(self):
        return self.name


class Item(models.Model):
    name = models.CharField(max_length=50)
    category = models.ForeignKey(Category, null=True)
    user = models.ForeignKey(AppUser)
    uses = models.IntegerField(default=0)

    def __str__(self):
        return self.name


class Expense(models.Model):
    date = models.DateTimeField('date')
    item = models.ForeignKey(Item)
    comment = models.CharField(max_length=1000)
    cost = models.IntegerField(default=0)
    user = models.ForeignKey(AppUser)

    def __str__(self):
        return '{} {} - {}'.format(str(self.date), str(self.cost), self.item.name)

# not used currently
class ItemExpense(models.Model):
    item = models.ForeignKey(Item, null=True)
    expense = models.ForeignKey(Expense, null=True)
    quantity = models.FloatField(default=1)
    cost = models.IntegerField(default=0)

    def __str__(self):
        return str(self.expense.date) +" "+ self.item.name+" "+str(self.cost)
