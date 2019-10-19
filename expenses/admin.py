from django.contrib import admin
from expenses.models import (
    AppUser,
    Organization,
    Category,
    Item,
    Expense,
    ItemExpense,
    Feedback,
    Token,
)

admin.autodiscover()

admin.site.register(AppUser)
admin.site.register(Organization)
admin.site.register(Category)
admin.site.register(Item)
admin.site.register(Expense)
admin.site.register(ItemExpense)
admin.site.register(Feedback)
admin.site.register(Token)
