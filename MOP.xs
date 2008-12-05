/* There's a lot of cases of doubled parens in here like this:

  while ( (he = ...) ) {

This shuts up warnings from gcc -Wall
*/

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define NEED_sv_2pv_flags
#define NEED_sv_2pv_nolen
#include "ppport.h"

SV *key_name;
U32 hash_name;

SV *key_package;
U32 hash_package;

SV *key_package_name;
U32 hash_package_name;

SV *key_body;
U32 hash_body;

SV* method_metaclass;
SV* associated_metaclass;
SV* wrap;


#define check_package_cache_flag(stash) mop_check_package_cache_flag(aTHX_ stash)
#ifdef HvMROMETA /* 5.10.0 */

#ifndef mro_meta_init
#define mro_meta_init(stash) Perl_mro_meta_init(aTHX_ stash) /* used in HvMROMETA macro */
#endif /* !mro_meta_init */

static UV
mop_check_package_cache_flag(pTHX_ HV* stash) {
    assert(SvTYPE(stash) == SVt_PVHV);

    return HvMROMETA(stash)->pkg_gen; /* mro::get_pkg_gen($pkg) */
}

#else /* pre 5.10.0 */

static UV
mop_check_package_cache_flag(pTHX_ HV* stash) {
    PERL_UNUSED_ARG(stash);
    assert(SvTYPE(stash) == SVt_PVHV);

    return PL_sub_generation;
}
#endif

#define call0(s, m)  mop_call0(aTHX_ s, m)
static SV*
mop_call0(pTHX_ SV* const self, SV* const method) {
    dSP;
    SV* ret;

    PUSHMARK(SP);
    XPUSHs(self);
    PUTBACK;

    call_sv(method, G_SCALAR | G_METHOD);

    SPAGAIN;
    ret = POPs;
    PUTBACK;

    return ret;
}

int
get_code_info (SV *coderef, char **pkg, char **name)
{
    if (!SvOK(coderef) || !SvROK(coderef) || SvTYPE(SvRV(coderef)) != SVt_PVCV) {
        return 0;
    }

    coderef = SvRV(coderef);
    /* I think this only gets triggered with a mangled coderef, but if
       we hit it without the guard, we segfault. The slightly odd return
       value strikes me as an improvement (mst)
    */
#ifdef isGV_with_GP
    if ( isGV_with_GP(CvGV(coderef)) ) {
#endif
        *pkg     = HvNAME( GvSTASH(CvGV(coderef)) );
        *name    = GvNAME( CvGV(coderef) );
#ifdef isGV_with_GP
    } else {
        *pkg     = "__UNKNOWN__";
        *name    = "__ANON__";
    }
#endif

    return 1;
}

typedef enum {
    TYPE_FILTER_NONE,
    TYPE_FILTER_CODE,
    TYPE_FILTER_ARRAY,
    TYPE_FILTER_IO,
    TYPE_FILTER_HASH,
    TYPE_FILTER_SCALAR,
} type_filter_t;

static HV *
get_all_package_symbols(HV *stash, type_filter_t filter)
{
    HE *he;
    HV *ret = newHV();

    (void)hv_iterinit(stash);

    if (filter == TYPE_FILTER_NONE) {
        while ( (he = hv_iternext(stash)) ) {
            STRLEN keylen;
            char *key = HePV(he, keylen);
            hv_store(ret, key, keylen, SvREFCNT_inc(HeVAL(he)), 0);
        }

        return ret;
    }

    while ( (he = hv_iternext(stash)) ) {
        SV *const gv = HeVAL(he);
        SV *sv = NULL;
        char *key;
        STRLEN keylen;
        char *package;
        SV *fq;

        switch( SvTYPE(gv) ) {
#ifndef SVt_RV
            case SVt_RV:
#endif
            case SVt_PV:
            case SVt_IV:
                /* expand the gv into a real typeglob if it
                 * contains stub functions and we were asked to
                 * return CODE symbols */
                if (filter == TYPE_FILTER_CODE) {
                    if (SvROK(gv)) {
                        /* we don't really care about the length,
                           but that's the API */
                        key = HePV(he, keylen);
                        package = HvNAME(stash);
                        fq = newSVpvf("%s::%s", package, key);
                        sv = (SV *)get_cv(SvPV_nolen(fq), 0);
                        break;
                    }

                    key = HePV(he, keylen);
                    gv_init((GV *)gv, stash, key, keylen, GV_ADDMULTI);
                }
                /* fall through */
            case SVt_PVGV:
                switch (filter) {
                    case TYPE_FILTER_CODE:   sv = (SV *)GvCVu(gv); break;
                    case TYPE_FILTER_ARRAY:  sv = (SV *)GvAV(gv);  break;
                    case TYPE_FILTER_IO:     sv = (SV *)GvIO(gv);  break;
                    case TYPE_FILTER_HASH:   sv = (SV *)GvHV(gv);  break;
                    case TYPE_FILTER_SCALAR: sv = (SV *)GvSV(gv);  break;
                    default:
                        croak("Unknown type");
                }
                break;
            default:
                continue;
        }

        if (sv) {
            char *key = HePV(he, keylen);
            hv_store(ret, key, keylen, newRV_inc(sv), 0);
        }
    }

    return ret;
}


static void
mop_update_method_map(pTHX_ SV* const self, SV* const class_name, HV* const stash, HV* const map) {
    const char* const class_name_pv = HvNAME(stash); /* must be HvNAME(stash), not SvPV_nolen_const(class_name) */
    SV*   method_metaclass_name;
    char* method_name;
    I32   method_name_len;
    SV *coderef;
    HV *symbols;
    dSP;

    symbols = get_all_package_symbols(stash, TYPE_FILTER_CODE);

    (void)hv_iterinit(symbols);
    while ( (coderef = hv_iternextsv(symbols, &method_name, &method_name_len)) ) {
        CV* cv = (CV *)SvRV(coderef);
        char* cvpkg_name;
        char* cv_name;
        SV* method_slot;
        SV* method_object;

        if (!get_code_info(coderef, &cvpkg_name, &cv_name)) {
            continue;
        }

        /* this checks to see that the subroutine is actually from our package  */
        if ( !(strEQ(cvpkg_name, "constant") && strEQ(cv_name, "__ANON__")) ) {
            if ( strNE(cvpkg_name, class_name_pv) ) {
                continue;
            }
        }

        method_slot = *hv_fetch(map, method_name, method_name_len, TRUE);
        if ( SvOK(method_slot) ) {
            SV* const body = call0(method_slot, key_body); /* $method_object->body() */
            if ( SvROK(body) && ((CV*) SvRV(body)) == cv ) {
                continue;
            }
        }

        method_metaclass_name = call0(self, method_metaclass); /* $self->method_metaclass() */

        /*
            $method_object = $method_metaclass->wrap(
                $cv,
                associated_metaclass => $self,
                package_name         => $class_name,
                name                 => $method_name
            );
        */
        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        EXTEND(SP, 8);
        PUSHs(method_metaclass_name); /* invocant */
        mPUSHs(newRV_inc((SV*)cv));
        PUSHs(associated_metaclass);
        PUSHs(self);
        PUSHs(key_package_name);
        PUSHs(class_name);
        PUSHs(key_name);
        mPUSHs(newSVpv(method_name, method_name_len));
        PUTBACK;

        call_sv(wrap, G_SCALAR | G_METHOD);
        SPAGAIN;
        method_object = POPs;
        PUTBACK;
        /* $map->{$method_name} = $method_object */
        sv_setsv(method_slot, method_object);

        FREETMPS;
        LEAVE;
    }
}

/*
get_code_info:
  Pass in a coderef, returns:
  [ $pkg_name, $coderef_name ] ie:
  [ 'Foo::Bar', 'new' ]
*/

MODULE = Class::MOP   PACKAGE = Class::MOP

BOOT:
    key_name = newSVpvs("name");
    key_body = newSVpvs("body");
    key_package = newSVpvs("package");
    key_package_name = newSVpvs("package_name");

    PERL_HASH(hash_name, "name", 4);
    PERL_HASH(hash_body, "body", 4);
    PERL_HASH(hash_package, "package", 7);
    PERL_HASH(hash_package_name, "package_name", 12);

    method_metaclass     = newSVpvs("method_metaclass");
    wrap                 = newSVpvs("wrap");
    associated_metaclass = newSVpvs("associated_metaclass");


PROTOTYPES: ENABLE


void
get_code_info(coderef)
    SV* coderef
    PREINIT:
        char* pkg  = NULL;
        char* name = NULL;
    PPCODE:
        if (get_code_info(coderef, &pkg, &name)) {
            EXTEND(SP, 2);
            PUSHs(newSVpv(pkg, 0));
            PUSHs(newSVpv(name, 0));
        }


MODULE = Class::MOP   PACKAGE = Class::MOP::Package

void
get_all_package_symbols(self, filter=TYPE_FILTER_NONE)
    SV *self
    type_filter_t filter
    PROTOTYPE: $;$
    PREINIT:
        HV *stash = NULL;
        HV *symbols = NULL;
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call get_all_package_symbols as a class method");
        }

        if (GIMME_V == G_VOID) {
            XSRETURN_EMPTY;
        }


        PUTBACK;

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_package, 0, hash_package)) ) {
            stash = gv_stashsv(HeVAL(he), 0);
        }


        if (!stash) {
            switch (GIMME_V) {
                case G_SCALAR: XSRETURN_UNDEF; break;
                case G_ARRAY:  XSRETURN_EMPTY; break;
            }
        }

        symbols = get_all_package_symbols(stash, filter);

        switch (GIMME_V) {
            case G_SCALAR:
                PUSHs(sv_2mortal(newRV_inc((SV *)symbols)));
                break;
            case G_ARRAY:
                warn("Class::MOP::Package::get_all_package_symbols in list context is deprecated. use scalar context instead.");

                EXTEND(SP, HvKEYS(symbols) * 2);

                while ((he = hv_iternext(symbols))) {
                    PUSHs(hv_iterkeysv(he));
                    PUSHs(sv_2mortal(SvREFCNT_inc(HeVAL(he))));
                }

                break;
            default:
                break;
        }

        SvREFCNT_dec((SV *)symbols);

void
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call name as a class method");
        }

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_package, 0, hash_package)) )
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

MODULE = Class::MOP   PACKAGE = Class::MOP::Attribute

void
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call name as a class method");
        }

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_name, 0, hash_name)) )
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

MODULE = Class::MOP   PACKAGE = Class::MOP::Method

void
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call name as a class method");
        }

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_name, 0, hash_name)) )
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

void
package_name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call package_name as a class method");
        }

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_package_name, 0, hash_package_name)) )
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

void
body(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if ( ! SvROK(self) ) {
            die("Cannot call body as a class method");
        }

        if ( (he = hv_fetch_ent((HV *)SvRV(self), key_body, 0, hash_body)) )
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;


MODULE = Class::MOP    PACKAGE = Class::MOP::Class

void
get_method_map(self)
    SV* self
    PREINIT:
        SV* const class_name = HeVAL( hv_fetch_ent((HV*)SvRV(self), key_package, TRUE, hash_package) );
        HV* const stash      = gv_stashsv(class_name, TRUE);
        UV  const current    = check_package_cache_flag(stash);
        SV* const cache_flag = *hv_fetchs((HV*)SvRV(self), "_package_cache_flag", TRUE);
        SV* const map_ref    = *hv_fetchs((HV*)SvRV(self), "methods", TRUE);
    PPCODE:
        /* in  $self->{methods} does not yet exist (or got deleted) */
        if ( ! (SvROK(map_ref) && SvTYPE(SvRV(map_ref)) == SVt_PVHV) ) {
            SV* new_map_ref = newRV_noinc((SV*)newHV());
            sv_2mortal(new_map_ref);
            sv_setsv(map_ref, new_map_ref);
        }

        if ( ! (SvOK(cache_flag) && SvUV(cache_flag) == current) ) {
            ENTER;
            SAVETMPS;

            mop_update_method_map(aTHX_ self, class_name, stash, (HV*)SvRV(map_ref));
            sv_setuv(cache_flag, check_package_cache_flag(stash)); /* update_cache_flag() */

            FREETMPS;
            LEAVE;
        }

        XPUSHs(map_ref);

