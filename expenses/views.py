from django.shortcuts import render, get_object_or_404
from django.http import HttpResponse, HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.template import RequestContext
from django.utils import timezone
from django.views.generic import View
from django.db.models import Sum
import datetime

from expenses.models import *
from expenses.forms import ExpenseForm

import json

# Create your views here.

class IndexPage(View):
    context = {}
    
    def get(self, request):   
        form  = ExpenseForm()
        self.context={'expense_form':form}

        # send categories and items associated
        cats = Category.objects.all()
        cats_items = {}
        for x in cats:
            items = Item.objects.filter(category=x)
            cats_items[x.name]=[x.name for x in items]
        self.context['cats_items'] = json.dumps(cats_items)
        
        return render(request, 'expenses/index.html', self.context)

    def post(self, request):
        form = ExpenseForm(request.POST)
        jsonstr = request.POST['json_string']
        if form.is_valid():
            exp_date = form.cleaned_data['date']
            comment = form.cleaned_data['comment']
            total_cost = form.cleaned_data['total']
        else:
            return HttpResponse('invalid form')

        expense_dict = json.loads(jsonstr)
        try:
            category = Category.objects.filter(name=expense_dict['category'])[0]
            for item in expense_dict['items']:
                itm_name = str(list(item.keys())[0])
                itm = Item.objects.all().filter(name=itm_name)[0]
                itm_exp = ItemExpense(date=exp_date, item=itm, 
                            quantity=float(item[itm_name]['quantity']), cost=int(item[itm_name]['cost'])
                )
                itm.save()
            expenses = Expense.objects.filter(date=exp_date, category=category)
            if len(expenses) == 0: # means no expense yet for the day on the same category
                expense = Expense(date=exp_date, category=category, comment=comment, cost=total_cost)
                expense.save()
            else: # already exists, add total
                expense = expenses[0]
                expense.cost+=total_cost
                expense.save()

            self.context['expense_form'] = ExpenseForm()
            self.context['message'] = 'Expense added'
            return render(request, 'expenses/index.html', self.context)
        except Exception as e:
            return HttpResponse(repr(e))


class ViewExpenses(View):
    context = {}

    def get(self, request):
        expenses = Expense.objects.all()
        categories = [x.name for x in Category.objects.all()]

        dates = []
        for exp in expenses:
            if exp.date not in dates:
                dates.append(exp.date)

        date_expenses={}
        for date in dates:
            exps = expenses.filter(date=date)
            #temp = {x:0 for x in categories}
            temp = [0]*len(categories)
            for x in exps:
                temp[categories.index(x.category.name)] = x.cost
                s = sum(temp)
                temp.append(s)
            date_expenses[str(date)] = temp

        return render(request, 'expenses/view.html', {'expenses':date_expenses, 'categories':categories})

class AddItems(View):
    context = {}

    def get(self, request):
        cats = [x.name for x in Category.objects.all()]
        self.context['categories'] = cats
        self.context['message'] = ""
        return render(request, 'expenses/add-items.html', self.context)

    def post(self, request):
        cat = request.POST.get('category', '')
        if cat=='':
            self.context['message'] = 'Empty Category'
            return render(request, 'expenses/add-items.html', self.context)
        try:
            category = Category.objects.get(name=cat)
            items = request.POST.get('items', '')

            #return HttpResponse(category)
            if items=='':
                self.set_message('empty items')
                return render(request, 'expenses/add-items.html', self.context)

            items = [x.upper().strip() for x in items.split(',')]
            existing_items = [x.name for x in Item.objects.filter(category=category)]

            for each in items:
                if not each in existing_items:
                    itm = Item(category=category, name=each)
                    itm.save()

            self.set_message('Items added for category: '+ cat)
            return render(request, 'expenses/add-items.html', self.context)
        except Exception as e:
            return HttpResponse(repr(e))
            self.context['message'] = 'Category not found'
            return render(request, 'expenses/add-items.html', self.context)

    def set_message(self, message):
        self.context['message'] = message

