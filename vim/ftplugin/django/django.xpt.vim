XPTemplate priority=personal

XPT auto " AutoField\()
`field^ = models.AutoField()
XPT bool " BooleanField\()
`field^ = models.BooleanField(`default=True^)
XPT char " CharField\()
`field^ = models.CharField(max_length=`length^`blank=True^)
XPT comma " CommaSeparatedIntegerField\()
`field^ = models.CommaSeparatedIntegerField(max_length=`^`, blank=True^)
XPT date " DateTimeField\()
date_`created^ = models.DateTimeField(`auto_now_add=True^, `auto_now=True^`, blank=True, null=True^)
XPT decimal " DecimalField\()
`field^ = models.DecimalField(max_digits=`^, decimal_places=`^)
XPT email " EmailField\()
`email^ = models.EmailField(max_length=`75^)
XPT file " FileField\()
`field^ = models.FileField(upload_to=`^)
XPT float " FloatField\()
`field^ = models.FloatField()
XPT int " IntegerField\()
`field^ = models.IntegerField()
XPT ip " IPAddressField\()
`ip^ = models.IPAddressField()
XPT nullbool " NullBooleanField\()
`field^ = models.NullBooleanField()
XPT posint " PositiveIntegerField\()
`field^ = models.PositiveIntegerField()
XPT possmallint " PositiveSmallIntegerField\()
`field^ = models.PositiveSmallIntegerField()
XPT slug " SlugField\()
`slug^ = models.SlugField(max_length=`50^`, blank=True^)
XPT smallint " SmallIntegerField\()
`field^ = models.SmallIntegerField()
XPT text " TextField\()
`body^ = models.TextField(`blank=True^)
XPT url " URLField\()
`url^ = models.URLField(max_length=`200`, blank=True^)

XPT bn " blank/null for models
blank=True, null=True

XPT resp " HttpResponse\()
XSET http=Choose(['HttpResponse', 'HttpResponseForbidden', 'HttpResponseRedirect'])
return `http^(`^);

XPT fk " ForeignKey\()
`field^ = models.ForeignKey(`Model^`, related_name=''^)
XPT m2m " ManyToManyField\()
`field^ = models.ManyToManyField(`OtherModel^)

XPT admin " class ModelAdmin\()
class `Model^Admin(admin.ModelAdmin):
    list_display = (`CURSOR^,)
admin.site.register(`Model^, `Model^Admin)

XPT r2r " render_to_response\()
return render_to_response(
    '`template^.html',
    `^,
    context_instance=RequestContext(request)
)


XPT rq " request
request
XPT rg " request.GET['']
request.GET['`^']
XPT rgc " request.GET
request.GET
XPT rp " request.POST['']
request.POST['`^']
XPT rpc " request.POST
request.POST
XPT ru " request.user
request.user
XPT set " queryset filter
`^set = `Model^.`objects^.`filter^(`owner=request.user^)

XPT dg " @require_GET
@require_GET
XPT dp " @require_POST
@require_POST
XPT da " @require_AJAX
@require_AJAX
XPT dl " @login_required
@login_required
XPT db " @check_blacklist
@check_blacklist

XPT ajax " AJAX framework view
@require_AJAX
def `name^(request):
    ret = {
        'success': False
    }

    `cursor^

    ret['success'] = True
    return ret

XPT ir " from notifications import report
from notifications import report
XPT r " report
report(`7^, '`reason^')

XPT 404 " raise Http404
raise Http404

XPT iu " from django.contrib.auth.models import User
from django.contrib.auth.models import User

XPT id " from notifications.debug import debug
from notifications.debug import debug

XPT i_ " import ugettext_lazy as _
from django.utils.translation import ugettext_lazy as _

XPT ex " if set.exists\()
if `set^.exists():
    `pass^

XPT gfm " Contenttype.get_for_model\(...)
ContentType.objects.get_for_model(`Model^)

XPT ict " import ContentType
from django.contrib.contenttypes.models import ContentType

XPT f " objects.filter\(...)
`objects^.filter(`^)
