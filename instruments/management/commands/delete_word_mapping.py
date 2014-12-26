from django.core.management.base import NoArgsCommand
from common.models import *


class Command(NoArgsCommand):

    def handle(self, *args, **options):

        WordMapping.objects.all().delete()
        WordInfo.objects.all().delete()