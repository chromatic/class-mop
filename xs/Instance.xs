#include "mop.h"

#define CHECK_INSTANCE(instance) STMT_START{                          \
        if(!(SvROK(instance) && SvTYPE(SvRV(instance)) == SVt_PVHV)){ \
            croak("Invalid object");                                  \
        }                                                             \
        if(SvTIED_mg(SvRV(instance), PERL_MAGIC_tied)){               \
            croak("MOP::Instance: tied HASH is not yet supported");   \
        }                                                             \
    } STMT_END

SV*
mop_instance_create_instance(pTHX_ HV* const stash) {
    assert(stash);
    return sv_bless( newRV_noinc((SV*)newHV()), stash );
}

bool
mop_instance_has_slot(pTHX_ SV* const instance, SV* const slot) {
    assert(instance);
    assert(slot);
    CHECK_INSTANCE(instance);
    return hv_exists_ent((HV*)SvRV(instance), slot, 0U);
}

SV*
mop_instance_get_slot(pTHX_ SV* const instance, SV* const slot) {
    HE* he;
    assert(instance);
    assert(slot);
    CHECK_INSTANCE(instance);
    he = hv_fetch_ent((HV*)SvRV(instance), slot, FALSE, 0U);
    return he ? HeVAL(he) : NULL;
}

SV*
mop_instance_set_slot(pTHX_ SV* const instance, SV* const slot, SV* const value) {
    HE* he;
    SV* sv;
    assert(instance);
    assert(slot);
    assert(value);
    CHECK_INSTANCE(instance);
    he = hv_fetch_ent((HV*)SvRV(instance), slot, TRUE, 0U);
    sv = HeVAL(he);
    sv_setsv_mg(sv, value);
    return sv;
}

SV*
mop_instance_delete_slot(pTHX_ SV* const instance, SV* const slot) {
    assert(instance);
    assert(slot);
    CHECK_INSTANCE(instance);
    return hv_delete_ent((HV*)SvRV(instance), slot, 0, 0U);
}

void
mop_instance_weaken_slot(pTHX_ SV* const instance, SV* const slot) {
    HE* he;
    assert(instance);
    assert(slot);
    CHECK_INSTANCE(instance);
    he = hv_fetch_ent((HV*)SvRV(instance), slot, FALSE, 0U);
    if(he){
        sv_rvweaken(HeVAL(he));
    }
}

static const mop_instance_vtbl mop_default_instance = {
	mop_instance_create_instance,
	mop_instance_has_slot,
	mop_instance_get_slot,
	mop_instance_set_slot,
	mop_instance_delete_slot,
	mop_instance_weaken_slot,
};


const mop_instance_vtbl*
mop_get_default_instance_vtbl(pTHX){
    return &mop_default_instance;
}


MODULE = Class::MOP::Instance  PACKAGE = Class::MOP::Instance

PROTOTYPES: DISABLE

BOOT:
    INSTALL_SIMPLE_READER(Instance, associated_metaclass);

void*
can_xs(SV* self)
PREINIT:
    CV* const default_method  = get_cv("Class::MOP::Instance::get_slot_value", FALSE);
    SV* const can             = newSVpvs_flags("can", SVs_TEMP);
    SV* const method          = newSVpvs_flags("get_slot_value", SVs_TEMP);
    SV* code_ref;
CODE:
    /* $self->can("get_slot_value") == \&Class::MOP::Instance::get_slot_value */
    code_ref = mop_call1(aTHX_ self, can, method);
    if(SvROK(code_ref) && SvRV(code_ref) == (SV*)default_method){
        RETVAL = (void*)&mop_default_instance;
    }
    else{
        RETVAL = NULL;
    }
OUTPUT:
    RETVAL

