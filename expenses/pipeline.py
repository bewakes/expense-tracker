import urllib.request
from expenses.models import Organization, AppUser, Category
from requests import request

DEFAULT_CATEGORIES = ['Groccery', 'Stationery', 'Lunch', 'Transportation']

def create_org(strategy, backend, user, response, details,
is_new=False, *args, **kwargs):
    print('in pipeline')
    if is_new:
        # create Organization for user
        org = Organization.objects.create(owner=user, name=user.first_name+" "+user.last_name)
        user.organizations.add(org)
        user.save()

        # add categories for user/org
        try:
            for cat in DEFAULT_CATEGORIES:
                category = Category.objects.create(organization=org, name=cat)
        except Exception as e:
            print(e)
            pass
