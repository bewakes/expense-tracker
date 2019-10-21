from rest_framework import authentication
from rest_framework import exceptions

from expenses.models import Token


class TokenAuthentication(authentication.BaseAuthentication):
    def authenticate(self, request):
        auth_header = request.META.get('HTTP_AUTHORIZATION')
        if not auth_header:
            return None

        token = auth_header.split()
        if token[0] == 'Token':
            token_obj = Token.objects.filter(value=token[-1]).first()
            return (token_obj.app_user, None)
        return None
