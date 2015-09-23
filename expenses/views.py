from django.shortcuts import render, get_object_or_404
from django.http import HttpResponse, HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.template import RequestContext
from django.utils import timezone
from django.views.generic import View
from date_data.date_data import *
import datetime

from expenses.models import *
from expenses.forms import ExpenseForm

import json

months = ['BAISAKH', 'JESTHA', 'ASHAR', 'SHRAWAN', 'BHADRA', 'ASHOJ', 'KARTIK', 'MANGSIR', 'POUSH', 'MAGH', 'FALGUN', 'CHAITRA']

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
        self.context['cats_items'] = json.dumps(cats_items).replace('"', '\\"').replace('\'', '\\\'')
        
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
        self.context['expense_form'] = ExpenseForm()
        try:
            category = Category.objects.filter(name=expense_dict['category'])[0]
            itm = None
            qty= None
            cst=None
            for item in expense_dict['items']:
                itm_name = str(list(item.keys())[0])
                itm = Item.objects.all().filter(name=itm_name)[0]

                qty = float(item[itm_name]['quantity'])
                cst = int(item[itm_name]['cost'])
                if qty==0 or cst==0:
                    self.context['message'] = 'Zero value for some items'
                    return render(request, 'expenses/index.html', self.context)
                                
            expenses = Expense.objects.filter(date=exp_date, category=category)
            expense=None
            if len(expenses) == 0: # means no expense yet for the day on the same category
                expense = Expense(date=exp_date, category=category, comment=comment, cost=total_cost)
                expense.save()
            else: # already exists, add total
                expense = expenses[0]
                expense.cost+=total_cost
                expense.comment+='\n'+comment
                expense.save()
            # save the item expense ( as it depends on expense)
            if len(expense_dict['items']) > 0:
                itm_exp = ItemExpense(expense=expense, item=itm, 
                            quantity=qty, cost=cst)
                itm_exp.save()

            self.context['message'] = 'Expense added'
            return render(request, 'expenses/index.html', self.context)
        except Exception as e:
            return HttpResponse(repr(e))


class ViewExpenses(View):
    context = {}

    def get(self, request):
        d = datetime.date(2015, 2, 19)
        #return HttpResponse(str(convert_to_nepali(d)))
        expenses = Expense.objects.all()
        categories = [x.name for x in Category.objects.all()]
        print(categories)

        dates = []
        for exp in expenses:
            if exp.date not in dates:
                dates.append(exp.date)
        dates.sort()

        #return HttpResponse(dates)
        date_expenses=[]
        nep_dates = []
        for date in dates:
            exps = expenses.filter(date=date)
            #temp = {x:0 for x in categories}
            temp = [0]*len(categories)
            print('len exps', len(exps))
            for x in exps:
                ind = categories.index(x.category.name)
                temp[ind] = x.cost
            s = sum(temp)
            temp.append(s)

            nep = convert_to_nepali(date)
            nep = str(nep[0])+ ' '+ months[nep[1]-1] + ' '+ str(nep[2])
            nep_dates.append([nep, str(date)])

            date_expenses.append(temp)

        self.context['categories']= categories
        self.context['dates_expenses'] = list(zip(nep_dates, date_expenses))

        return render(request, 'expenses/view.html', self.context)


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

# parse date
def parse(date_str):
    temp = [int(x) for x in date_str.split('-')]
    return datetime.date(temp[0],temp[1],temp[2])

def comment_request(request):
    try:
        post_date = request.POST.get('date', '')
        if post_date=='':
            return HttpResponse('***')
        exps = Expense.objects.filter(date=parse(post_date))

        if exps[0].comment=='':
            return HttpResponse('***')
        return HttpResponse(exps[0].comment)
    except Exception as e:
        return HttpResponse(str(e))


def convert_to_nepali(engdate, delta=-1):

    # date min 2000, push 17
    # date max 2090, Chaitra 30
    start_eng = datetime.date(1944, 1, 1)

    if delta==-1:
        delta_days = (engdate-start_eng).days

    else:delta_days=delta

    # nepali_date has array of months
    nep_yr = 2000
    nep_mt = 9
    nep_dy = 17

    ind = nep_yr - 2000
    mt = nep_mt-1
    rem_days = nepali_date[0][8] - nep_dy

    while True:
        delta_days -= rem_days
        #print('delta_days:', delta_days)
        mt+=1
        if delta_days == 0:
            #print('del 0 here')
            return (nep_yr, mt, nepali_date[ind][mt])
        if delta_days < 0:
            #print('neg here')
            return(nep_yr, mt, nepali_date[ind][mt-1]+delta_days)
        if mt>11:
            nep_yr+=1
            mt=0
            ind+=1
        rem_days = nepali_date[ind][mt]
