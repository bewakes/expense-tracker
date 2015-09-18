from django import forms
from expenses.models import Category, Item

import datetime

def get_choices_for_category():
    cats = Category.objects.all()
    return [(cats[i], cats[i]) for i in range(len(cats))]

class ExpenseForm(forms.Form):
    date = forms.DateField(initial = datetime.date.today, required=True)
    category = forms.ChoiceField(choices=get_choices_for_category, required=True)
    total = forms.IntegerField(required=True)
    #item = forms.ChoiceField(label='Item', choices=[])
    #quantity = forms.DecimalField(min_value=0, max_value=100000.0)
    #cost = forms.IntegerField(required=True)
    comment = forms.CharField(widget=forms.Textarea(attrs={'placeholder':'Comment about  the expense'}), required=False)
