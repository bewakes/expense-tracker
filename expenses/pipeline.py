import urllib.request
from expenses.models import Organization, AppUser
from requests import request

def create_org(strategy, backend, user, response, details,
is_new=False, *args, **kwargs):
    print('in pipeline')
    if is_new:
        # create Organization for user
        org = Organization.objects.create(owner=user, name=user.first_name+" "+user.last_name)
        user.organizations.add(org)
        user.save()
