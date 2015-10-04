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

import json, re

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
                expense.comment+='<br>'+comment
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

    def get_records(self, start_date=datetime.datetime.now().date(), end_date=datetime.datetime.now().date()):
        context = {}
        expenses = Expense.objects.all()
        categories = [x.name for x in Category.objects.all()]

        dates = []
        for exp in expenses:
            if exp.date not in dates and exp.date<=end_date and exp.date>=start_date:
                dates.append(exp.date)
        dates.sort()

        date_expenses=[]
        nep_dates = []
        sum_total = 0
        for date in dates:
            exps = expenses.filter(date=date)
            temp = [0]*len(categories)
            for x in exps: 
                ind = categories.index(x.category.name)
                temp[ind] = x.cost
            s = sum(temp)
            sum_total+=s
            temp.append(s)

            nep = convert_to_nepali(date)
            nep = str(nep[0])+ ' '+ months[nep[1]-1] + ' '+ str(nep[2])
            nep_dates.append([nep, str(date)])

            date_expenses.append(temp)

        context['categories']= categories
        context['dates_expenses'] = list(zip(nep_dates, date_expenses))
        context['grand_total'] = sum_total
        return context

    def get(self, request):
        end = datetime.datetime.now().date()
        start = end -  datetime.timedelta(days=7)
        self.context = self.get_records(start, end)
        self.context['detail_title'] = 'Last week\'s expense'
        self.context['months'] = months
        self.context['years'] = [2072 + x for x in range(0,2015 - datetime.datetime.now().year+1)]
        return render(request, 'expenses/view.html', self.context)

    def post(self, request):
        year = self.request.POST.get('year', '')
        month = self.request.POST.get('month', '')
        
        if not re.match(r'[0-9]{4}', year) or month not in months:
            raise Http404('requested date not found')

        date_range = get_eng_date_range(year, month)   
        self.context = self.get_records(date_range[0], date_range[1])
        self.context['detail_title'] = 'Details for '+year+' '+month
        self.context['months'] = months
        self.context['years'] = [2072 + x for x in range(0,2015 - datetime.datetime.now().year+1)]

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

        comments = ""
        for x in exps:
            comments+=x.comment+'\n'
        c = comments.strip()
        c = c.replace('\n','')
        if c=='':
            return HttpResponse('***')
        return HttpResponse(comments)
    except Exception as e:
        return HttpResponse(str(e))


def convert_to_nepali(engdate, delta=-1):

    # date min 2000, push 17
    # date max 2090, Chaitra 30
    try:
        engdate = engdate.date()
    except:
        pass
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


def get_eng_date_range(nep_year, nep_month):
        start_eng_year = int(nep_year) - 57   # generally, engyr = nepyr-57
        month_ind = months.index(nep_month)+1
        if month_ind > 9: # but if month is > MAGH then nepyr-57+1
            start_eng_year+=1

        end_eng_year = start_eng_year
        start_eng_month = (month_ind+3)%13
        end_eng_month = start_eng_month+1
        if end_eng_month>12:
            end_eng_month=1
            end_eng_year += 1
        start_date = 15
        end_date = 20
        # we check from 15th of start month to 20th of next month so that we wont miss anything 

        # check first 6 days for our desired month
        nep = convert_to_nepali(datetime.datetime(start_eng_year, start_eng_month, start_date))
        # the month should not be equal to the month we seek( generally new month starts from 17/18)
        if nep[1]-1==month_ind:
            raise Http404('Something error with month calculation')
        for x in range(6):
            nep = convert_to_nepali(datetime.datetime(start_eng_year, start_eng_month, start_date+x))
            if nep[1]-1==month_ind:
                start_date = start_date+x
                break

        # now check last 6 days for our desired month
        nep = convert_to_nepali(datetime.datetime(end_eng_year, end_eng_month, end_date))
        # the month should not be equal to the month we seek( generally new month starts from 17/18)
        if nep[1]==month_ind:
            raise Http404('Something error with month calculation')
        for x in range(6):
            nep = convert_to_nepali(datetime.datetime(end_eng_year, end_eng_month, end_date-x))
            if nep[1]==month_ind:
                end_date = end_date-x
                break
        return [datetime.datetime(start_eng_year, start_eng_month, start_date).date(),
                    datetime.datetime(end_eng_year, end_eng_month, end_date).date()]



def show_graph(request):
    return HttpResponse(get_eng_date_range(2072, 'POUSH'))
    return render(request, "expenses/graph.html",{})
