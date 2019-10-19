from expenses.models import Token


def TokenAuthenticationMiddleware(get_response):
    def middleware(request):
        auth_header = request.META.get('HTTP_AUTHORIZATION')
        response = get_response(request)
        if auth_header is None:
            return response

        token = auth_header.split()
        if token[0] == 'Token':
            token_obj = Token.objects.filter(value=token[-1]).first()
            if not token_obj:
                return response
            request.user = token_obj.app_user
            response = get_response(request)

        return response 
    return middleware
