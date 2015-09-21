from django.db import models


class Category(models.Model):
    name = models.CharField(max_length=50)

    def __str__(self):
        return self.name


class Item(models.Model):
    name = models.CharField(max_length=50)
    category = models.ForeignKey(Category, null=True)

    def __str__(self):
        return self.name


class Expense(models.Model):
    date = models.DateField('date')
    comment = models.CharField(max_length=1000)
    category = models.ForeignKey(Category, null=True)
    cost = models.IntegerField(default=0)
    
    def __str__(self):
        return str(self.date) + " "+ str(self.cost)

class ItemExpense(models.Model):
    date = models.DateField('date')
    item = models.ForeignKey(Item, null=True)
    quantity = models.FloatField(default=1)
    cost = models.IntegerField(default=0)

    def __str__(self):
        return str(self.date) +" "+ self.item.name+" "+str(self.cost)
