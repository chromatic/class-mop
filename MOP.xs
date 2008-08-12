
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define NEED_sv_2pv_nolen
#include "ppport.h"

SV *key_name;
U32 hash_name;

SV *key_package;
U32 hash_package;

SV *key_body;
U32 hash_body;

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

    PERL_HASH(hash_name, "name", 4);
    PERL_HASH(hash_body, "body", 4);
    PERL_HASH(hash_package, "package", 7);


PROTOTYPES: ENABLE


void
get_code_info(coderef)
  SV* coderef
  PREINIT:
    char* name;
    char* pkg;
  PPCODE:
    if( SvOK(coderef) && SvROK(coderef) && SvTYPE(SvRV(coderef)) == SVt_PVCV){
      coderef = SvRV(coderef);
      /* I think this only gets triggered with a mangled coderef, but if
         we hit it without the guard, we segfault. The slightly odd return
         value strikes me as an improvement (mst)
      */
#ifdef isGV_with_GP
      if ( isGV_with_GP(CvGV(coderef))) {
#endif
        pkg     = HvNAME( GvSTASH(CvGV(coderef)) );
        name    = GvNAME( CvGV(coderef) );
#ifdef isGV_with_GP
      } else {
        pkg     = "__UNKNOWN__";
        name    = "__ANON__";
      }
#endif

      EXTEND(SP, 2);
      PUSHs(newSVpvn(pkg, strlen(pkg)));
      PUSHs(newSVpvn(name, strlen(name)));
    }


MODULE = Class::MOP   PACKAGE = Class::MOP::Package

void
get_all_package_symbols(self, ...)
    SV *self
    PROTOTYPE: $;$
    PREINIT:
        HV *stash = NULL;
        SV *type_filter = NULL;
    PPCODE:

        switch ( GIMME_V ) {
            case G_VOID: return; break;
            case G_SCALAR: ST(0) = &PL_sv_undef; return; break;
        }

        if ( items > 1 ) type_filter = ST(1);

        PUTBACK;

        if ( SvROK(self) ) {
            SV **val = hv_fetchs((HV *)SvRV(self), "package", 0);
            if ( val ) {
                stash = gv_stashsv(*val, 0);
            }
        }

        if ( stash ) {
            register HE *entry;

            (void)hv_iterinit(stash);

            if ( type_filter && SvPOK(type_filter) ) {
                const char *const type = SvPV_nolen(type_filter);


                while ((entry = hv_iternext(stash))) {
                    SV *const gv = hv_iterval(stash, entry);
                    SV *const key = hv_iterkeysv(entry);
                    SV *sv;
                    char *package = HvNAME(stash);
                    STRLEN pkglen = strlen(package);
                    char *fq;
                    STRLEN fqlen;

                    SPAGAIN;

                    switch( SvTYPE(gv) ) {
                        case SVt_PVGV:
                            switch (*type) {
                                case 'C': sv = (SV *)GvCVu(gv); break; /* CODE */
                                case 'A': sv = (SV *)GvAV(gv); break; /* ARRAY */
                                case 'I': sv = (SV *)GvIO(gv); break; /* IO */
                                case 'H': sv = (SV *)GvHV(gv); break; /* HASH */
                                case 'S': sv = (SV *)GvSV(gv); break; /* SCALAR */
                                default:
                                          croak("Unknown type %s\n", type);
                            }
                            break;
                        case SVt_RV:
                            /* BAH! constants are horrible */
                            fqlen = pkglen + SvCUR(key) + 3;
                            fq = (char *)alloca(fqlen);
                            snprintf(fq, fqlen, "%s::%s", package, SvPV_nolen(key));
                            sv = (SV*)get_cv(fq, 0);
                            break;
                        default:
                            continue;
                    }

                    if ( sv ) {
                        SPAGAIN;
                        EXTEND(SP, 2);
                        PUSHs(key);
                        PUSHs(sv_2mortal(newRV_inc(sv)));
                        PUTBACK;
                    }
                }
            } else {
                EXTEND(SP, HvKEYS(stash) * 2);

                while ((entry = hv_iternext(stash))) {
                    SV *key = hv_iterkeysv(entry);
                    SV *sv = hv_iterval(stash, entry);
                    SPAGAIN;
                    PUSHs(key);
                    PUSHs(sv);
                    PUTBACK;
                }
            }

        }

SV *
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if (SvROK(self) && (he = hv_fetch_ent((HV *)SvRV(self), key_package, 0, hash_package)))
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

MODULE = Class::MOP   PACKAGE = Class::Attribute

SV *
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if (SvROK(self) && (he = hv_fetch_ent((HV *)SvRV(self), key_name, 0, hash_name)))
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

MODULE = Class::MOP   PACKAGE = Class::Method

SV *
name(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if (SvROK(self) && (he = hv_fetch_ent((HV *)SvRV(self), key_name, 0, hash_name)))
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;

SV *
body(self)
    SV *self
    PREINIT:
        register HE *he;
    PPCODE:
        if (SvROK(self) && (he = hv_fetch_ent((HV *)SvRV(self), key_body, 0, hash_body)))
            XPUSHs(HeVAL(he));
        else
            ST(0) = &PL_sv_undef;
