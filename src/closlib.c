/*Generated by genstub. Do not edit.*/

#line 14 "./closlib.stub"

#define LIBSAGITTARIUS_BODY


#line 15 "./closlib.stub"

#include <sagittarius.h>

#include <sagittarius/generic.h>

static struct sg__rcRec {
  SgObject d28[13];
} sg__rc = {
  {  /* SgObject d28 */
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
  },
};
static SgObject closlib_slot_ref(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SgObject name_scm;
  SgSymbol* name;
  SG_ENTER_SUBR("slot-ref");
  if (SG_ARGC != 2)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("slot-ref"), 2, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  name_scm = SG_ARGREF(1);
  if (!SG_SYMBOLP(name_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[0], SG_MAKE_STRING("symbol"), name_scm, SG_NIL);
  name = SG_SYMBOL(name_scm);
  {
{
SgObject SG_RESULT = (SgObject)NULL;

#line 19 "./closlib.stub"
SG_RESULT=(Sg_SlotRef(o,name));
SG_RETURN(SG_OBJ_SAFE(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib_slot_ref__STUB, 2, 0,closlib_slot_ref, SG_FALSE, NULL);

static SgObject closlib_slot_setX(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SgObject name_scm;
  SgSymbol* name;
  SgObject v_scm;
  SgObject v;
  SG_ENTER_SUBR("slot-set!");
  if (SG_ARGC != 3)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("slot-set!"), 3, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  name_scm = SG_ARGREF(1);
  if (!SG_SYMBOLP(name_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[1], SG_MAKE_STRING("symbol"), name_scm, SG_NIL);
  name = SG_SYMBOL(name_scm);
  v_scm = SG_ARGREF(2);
  v = (v_scm);
  {

#line 22 "./closlib.stub"
Sg_SlotSet(o,name,v);
SG_RETURN(SG_UNDEF);
  }
}
static SG_DEFINE_SUBR(closlib_slot_setX__STUB, 3, 0,closlib_slot_setX, SG_FALSE, NULL);

static SgObject closlib_slot_ref_using_accessor(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SgObject sa_scm;
  SgSlotAccessor* sa;
  SG_ENTER_SUBR("slot-ref-using-accessor");
  if (SG_ARGC != 2)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("slot-ref-using-accessor"), 2, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  sa_scm = SG_ARGREF(1);
  if (!SG_SLOT_ACCESSORP(sa_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[2], SG_MAKE_STRING("slot-accessor"), sa_scm, SG_NIL);
  sa = SG_SLOT_ACCESSOR(sa_scm);
  {
{
SgObject SG_RESULT = (SgObject)NULL;

#line 25 "./closlib.stub"
SG_RESULT=(Sg_SlotRefUsingAccessor(o,sa));
SG_RETURN(SG_OBJ_SAFE(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib_slot_ref_using_accessor__STUB, 2, 0,closlib_slot_ref_using_accessor, SG_FALSE, NULL);

static SgObject closlib_slot_set_using_accessorX(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SgObject sa_scm;
  SgSlotAccessor* sa;
  SgObject v_scm;
  SgObject v;
  SG_ENTER_SUBR("slot-set-using-accessor!");
  if (SG_ARGC != 3)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("slot-set-using-accessor!"), 3, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  sa_scm = SG_ARGREF(1);
  if (!SG_SLOT_ACCESSORP(sa_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[3], SG_MAKE_STRING("slot-accessor"), sa_scm, SG_NIL);
  sa = SG_SLOT_ACCESSOR(sa_scm);
  v_scm = SG_ARGREF(2);
  v = (v_scm);
  {

#line 28 "./closlib.stub"
Sg_SlotSetUsingAccessor(o,sa,v);
SG_RETURN(SG_UNDEF);
  }
}
static SG_DEFINE_SUBR(closlib_slot_set_using_accessorX__STUB, 3, 0,closlib_slot_set_using_accessorX, SG_FALSE, NULL);

static SgObject closlib_class_of(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SG_ENTER_SUBR("class-of");
  if (SG_ARGC != 1)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("class-of"), 1, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  {
{
SgObject SG_RESULT = (SgObject)NULL;

#line 31 "./closlib.stub"
SG_RESULT=(Sg_ClassOf(o));
SG_RETURN(SG_OBJ_SAFE(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib_class_of__STUB, 1, 0,closlib_class_of, SG_FALSE, NULL);

static SgObject closlib_is_aP(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject o_scm;
  SgObject o;
  SgObject klass_scm;
  SgObject klass;
  SG_ENTER_SUBR("is-a?");
  if (SG_ARGC != 2)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("is-a?"), 2, SG_ARGC, SG_NIL);
  o_scm = SG_ARGREF(0);
  o = (o_scm);
  klass_scm = SG_ARGREF(1);
  klass = (klass_scm);
  {
{
int SG_RESULT = (int)NULL;

#line 34 "./closlib.stub"
SG_RESULT=(Sg_TypeP(o,klass));
SG_RETURN(SG_MAKE_BOOL(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib_is_aP__STUB, 2, 0,closlib_is_aP, SG_FALSE, NULL);

static SgObject call_fallback_proc(SgObject* args,int nargs,SgGeneric* gf){{
#line 38 "./closlib.stub"
return (Sg_VMApply(SG_OBJ((gf)->data),Sg_ArrayToList(args,nargs)));}}
static SgObject closlib__25ensure_generic_function(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject name_scm;
  SgSymbol* name;
  SgObject lib_scm;
  SgLibrary* lib;
  SG_ENTER_SUBR("%ensure-generic-function");
  if (SG_ARGC != 2)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("%ensure-generic-function"), 2, SG_ARGC, SG_NIL);
  name_scm = SG_ARGREF(0);
  if (!SG_SYMBOLP(name_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[6], SG_MAKE_STRING("symbol"), name_scm, SG_NIL);
  name = SG_SYMBOL(name_scm);
  lib_scm = SG_ARGREF(1);
  if (!SG_LIBRARYP(lib_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[6], SG_MAKE_STRING("library"), lib_scm, SG_NIL);
  lib = SG_LIBRARY(lib_scm);
  {
{
SgObject SG_RESULT = (SgObject)NULL;

#line 41 "./closlib.stub"
{SgGloc* g=Sg_FindBinding(lib,name,SG_FALSE);SgObject val=SG_FALSE;
#line 43 "./closlib.stub"
if (SG_GLOCP(g)){{
val=(SG_GLOC_GET(g));}}
if ((!(Sg_TypeP(val,SG_CLASS_GENERIC)))){{
if ((SG_SUBRP(val))||(SG_CLOSUREP(val))){
val=(Sg_MakeBaseGeneric(SG_OBJ(name),call_fallback_proc,val));} else {
val=(Sg_MakeBaseGeneric(SG_OBJ(name),NULL,NULL));}
Sg_InsertBinding(lib,name,val);}}
SG_RESULT=(val);}
SG_RETURN(SG_OBJ_SAFE(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib__25ensure_generic_function__STUB, 2, 0,closlib__25ensure_generic_function, SG_FALSE, NULL);

static SgObject closlib__25make_next_method(SgObject *SG_FP, int SG_ARGC, void *data_)
{
  SgObject gf_scm;
  SgObject gf;
  SgObject methods_scm;
  SgObject methods;
  SgObject args_scm;
  SgObject args;
  SG_ENTER_SUBR("%make-next-method");
  if (SG_ARGC != 3)
    Sg_WrongNumberOfArgumentsViolation(
     SG_INTERN("%make-next-method"), 3, SG_ARGC, SG_NIL);
  gf_scm = SG_ARGREF(0);
  gf = (gf_scm);
  methods_scm = SG_ARGREF(1);
  if (!SG_LISTP(methods_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[7], SG_MAKE_STRING("list"), methods_scm, SG_NIL);
  methods = (methods_scm);
  args_scm = SG_ARGREF(2);
  if (!SG_LISTP(args_scm))
    Sg_WrongTypeOfArgumentViolation(
       sg__rc.d28[7], SG_MAKE_STRING("list"), args_scm, SG_NIL);
  args = (args_scm);
  {
{
SgObject SG_RESULT = (SgObject)NULL;
SgObject cise__29;
#line 53 "./closlib.stub"
{SgObject* argv;int argc;
if ((!(Sg_TypeP(gf,SG_CLASS_GENERIC)))){{
Sg_WrongTypeOfArgumentViolation(sg__rc.d28[8],sg__rc.d28[9],gf,
#line 57 "./closlib.stub"
SG_LIST3(gf,methods,args));}}
SG_FOR_EACH(cise__29,methods) {{SgObject mp=SG_CAR(cise__29);
if ((!(Sg_TypeP(mp,SG_CLASS_METHOD)))){{
Sg_WrongTypeOfArgumentViolation(sg__rc.d28[8],sg__rc.d28[11],mp,
#line 62 "./closlib.stub"
SG_LIST3(gf,methods,args));}}}}
argc=(Sg_Length(args));
argv=(Sg_ListToArray(args,FALSE));
SG_RESULT=(Sg_MakeNextMethod(SG_GENERIC(gf),methods,argv,argc,FALSE));}
SG_RETURN(SG_OBJ_SAFE(SG_RESULT));
}
  }
}
static SG_DEFINE_SUBR(closlib__25make_next_method__STUB, 3, 0,closlib__25make_next_method, SG_FALSE, NULL);

void Sg__Init_sagittarius_clos () {
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE));
  sg__rc.d28[0] = SG_MAKE_STRING("slot-ref");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[0]), &closlib_slot_ref__STUB);
  SG_PROCEDURE_NAME(&closlib_slot_ref__STUB) = sg__rc.d28[0];
  SG_PROCEDURE_NAME(&closlib_slot_setX__STUB) = sg__rc.d28[1];
  Sg_SetterSet(SG_PROCEDURE(&closlib_slot_ref__STUB), SG_PROCEDURE(&closlib_slot_setX__STUB), TRUE);
  sg__rc.d28[1] = SG_MAKE_STRING("slot-set!");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[1]), &closlib_slot_setX__STUB);
  SG_PROCEDURE_NAME(&closlib_slot_setX__STUB) = sg__rc.d28[1];
  sg__rc.d28[2] = SG_MAKE_STRING("slot-ref-using-accessor");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[2]), &closlib_slot_ref_using_accessor__STUB);
  SG_PROCEDURE_NAME(&closlib_slot_ref_using_accessor__STUB) = sg__rc.d28[2];
  sg__rc.d28[3] = SG_MAKE_STRING("slot-set-using-accessor!");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[3]), &closlib_slot_set_using_accessorX__STUB);
  SG_PROCEDURE_NAME(&closlib_slot_set_using_accessorX__STUB) = sg__rc.d28[3];
  sg__rc.d28[4] = SG_MAKE_STRING("class-of");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[4]), &closlib_class_of__STUB);
  SG_PROCEDURE_NAME(&closlib_class_of__STUB) = sg__rc.d28[4];
  sg__rc.d28[5] = SG_MAKE_STRING("is-a?");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[5]), &closlib_is_aP__STUB);
  SG_PROCEDURE_NAME(&closlib_is_aP__STUB) = sg__rc.d28[5];
  sg__rc.d28[6] = SG_MAKE_STRING("%ensure-generic-function");
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[6]), &closlib__25ensure_generic_function__STUB);
  SG_PROCEDURE_NAME(&closlib__25ensure_generic_function__STUB) = sg__rc.d28[6];
  sg__rc.d28[7] = SG_MAKE_STRING("%make-next-method");
  sg__rc.d28[8] = Sg_Intern(sg__rc.d28[7]); /* %make-next-method */
  sg__rc.d28[10] = SG_MAKE_STRING("generic");
  sg__rc.d28[9] = Sg_Intern(sg__rc.d28[10]); /* generic */
  sg__rc.d28[12] = SG_MAKE_STRING("method");
  sg__rc.d28[11] = Sg_Intern(sg__rc.d28[12]); /* method */
  Sg_InsertBinding(lib, Sg_Intern(sg__rc.d28[7]), &closlib__25make_next_method__STUB);
  SG_PROCEDURE_NAME(&closlib__25make_next_method__STUB) = sg__rc.d28[7];
}
