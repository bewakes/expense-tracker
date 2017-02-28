from rest_framework.response import Response
from rest_framework.decorators import api_view
from rest_framework import status

from django.conf import settings

from subprocess import Popen, PIPE

@api_view(['GET', 'POST'])
def webhook(request):
    """
    execute the command here
    """
    print('webhook request')
    p = Popen(['sh', settings.WEBHOOK_SCRIPT_PATH, settings.PASSWORD], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    rc = p.returncode
    if not rc:
        return Response({'status':'success', 'output':output})
    else:
        return Response({'status': 'failure', 'error': err}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
