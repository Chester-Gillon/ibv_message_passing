pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package slp_h is

   SLP_LIFETIME_DEFAULT : constant := 10800;  --  /usr/include/slp.h:80
   SLP_LIFETIME_MAXIMUM : constant := 65535;  --  /usr/include/slp.h:81

   SLP_LAST_CALL : constant := 1;  --  /usr/include/slp.h:91

   SLP_OK : constant := 0;  --  /usr/include/slp.h:105

   SLP_LANGUAGE_NOT_SUPPORTED : constant := -1;  --  /usr/include/slp.h:109

   SLP_PARSE_ERROR : constant := -2;  --  /usr/include/slp.h:116

   SLP_INVALID_REGISTRATION : constant := -3;  --  /usr/include/slp.h:124

   SLP_SCOPE_NOT_SUPPORTED : constant := -4;  --  /usr/include/slp.h:131

   SLP_AUTHENTICATION_ABSENT : constant := -6;  --  /usr/include/slp.h:139

   SLP_AUTHENTICATION_FAILED : constant := -7;  --  /usr/include/slp.h:145

   SLP_INVALID_UPDATE : constant := -13;  --  /usr/include/slp.h:150

   SLP_REFRESH_REJECTED : constant := -15;  --  /usr/include/slp.h:156

   SLP_NOT_IMPLEMENTED : constant := -17;  --  /usr/include/slp.h:163

   SLP_BUFFER_OVERFLOW : constant := -18;  --  /usr/include/slp.h:167

   SLP_NETWORK_TIMED_OUT : constant := -19;  --  /usr/include/slp.h:173

   SLP_NETWORK_INIT_FAILED : constant := -20;  --  /usr/include/slp.h:179

   SLP_MEMORY_ALLOC_FAILED : constant := -21;  --  /usr/include/slp.h:185

   SLP_PARAMETER_BAD : constant := -22;  --  /usr/include/slp.h:189

   SLP_NETWORK_ERROR : constant := -23;  --  /usr/include/slp.h:194

   SLP_INTERNAL_SYSTEM_ERROR : constant := -24;  --  /usr/include/slp.h:199

   SLP_HANDLE_IN_USE : constant := -25;  --  /usr/include/slp.h:205

   SLP_TYPE_ERROR : constant := -26;  --  /usr/include/slp.h:212

   SLP_RETRY_UNICAST : constant := -27;  --  /usr/include/slp.h:218

   SLP_REG_FLAG_FRESH : constant := (1);  --  /usr/include/slp.h:521
   SLP_REG_FLAG_WATCH_PID : constant := (2 ** 1);  --  /usr/include/slp.h:522

   subtype SLPError is int;  -- /usr/include/slp.h:89

   type SLPBoolean is 
     (SLP_FALSE,
      SLP_TRUE);
   pragma Convention (C, SLPBoolean);  -- /usr/include/slp.h:235

   type srvurl is record
      s_pcSrvType : Interfaces.C.Strings.chars_ptr;  -- /usr/include/slp.h:250
      s_pcHost : Interfaces.C.Strings.chars_ptr;  -- /usr/include/slp.h:256
      s_iPort : aliased int;  -- /usr/include/slp.h:260
      s_pcNetFamily : Interfaces.C.Strings.chars_ptr;  -- /usr/include/slp.h:264
      s_pcSrvPart : Interfaces.C.Strings.chars_ptr;  -- /usr/include/slp.h:270
   end record;
   pragma Convention (C_Pass_By_Copy, srvurl);  -- /usr/include/slp.h:248

   subtype SLPSrvURL is srvurl;  -- /usr/include/slp.h:273

   type SLPHandle is new System.Address;  -- /usr/include/slp.h:280

   --  skipped function type SLPRegReport

   --  skipped function type SLPSrvTypeCallback

   --  skipped function type SLPSrvURLCallback

   --  skipped function type SLPAttrCallback

   function SLPOpen
     (pcLang : Interfaces.C.Strings.chars_ptr;
      isAsync : SLPBoolean;
      phSLP : System.Address) return SLPError;  -- /usr/include/slp.h:431
   pragma Import (C, SLPOpen, "SLPOpen");

   procedure SLPClose (hSLP : SLPHandle);  -- /usr/include/slp.h:472
   pragma Import (C, SLPClose, "SLPClose");

   function SLPAssociateIFList (hSLP : SLPHandle; McastIFList : Interfaces.C.Strings.chars_ptr) return SLPError;  -- /usr/include/slp.h:485
   pragma Import (C, SLPAssociateIFList, "SLPAssociateIFList");

   function SLPAssociateIP (hSLP : SLPHandle; unicast_ip : Interfaces.C.Strings.chars_ptr) return SLPError;  -- /usr/include/slp.h:504
   pragma Import (C, SLPAssociateIP, "SLPAssociateIP");

   function SLPReg
     (hSLP : SLPHandle;
      pcSrvURL : Interfaces.C.Strings.chars_ptr;
      usLifetime : unsigned_short;
      pcSrvType : Interfaces.C.Strings.chars_ptr;
      pcAttrs : Interfaces.C.Strings.chars_ptr;
      fresh : SLPBoolean;
      callback : access procedure
        (arg1 : SLPHandle;
         arg2 : SLPError;
         arg3 : System.Address);
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:525
   pragma Import (C, SLPReg, "SLPReg");

   function SLPDereg
     (hSLP : SLPHandle;
      pcSrvURL : Interfaces.C.Strings.chars_ptr;
      callback : access procedure
        (arg1 : SLPHandle;
         arg2 : SLPError;
         arg3 : System.Address);
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:588
   pragma Import (C, SLPDereg, "SLPDereg");

   function SLPDelAttrs
     (hSLP : SLPHandle;
      pcSrvURL : Interfaces.C.Strings.chars_ptr;
      pcAttrs : Interfaces.C.Strings.chars_ptr;
      callback : access procedure
        (arg1 : SLPHandle;
         arg2 : SLPError;
         arg3 : System.Address);
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:616
   pragma Import (C, SLPDelAttrs, "SLPDelAttrs");

   function SLPFindSrvTypes
     (hSLP : SLPHandle;
      pcNamingAuthority : Interfaces.C.Strings.chars_ptr;
      pcScopeList : Interfaces.C.Strings.chars_ptr;
      callback : access function
        (arg1 : SLPHandle;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : SLPError;
         arg4 : System.Address) return SLPBoolean;
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:647
   pragma Import (C, SLPFindSrvTypes, "SLPFindSrvTypes");

   function SLPFindSrvs
     (hSLP : SLPHandle;
      pcServiceType : Interfaces.C.Strings.chars_ptr;
      pcScopeList : Interfaces.C.Strings.chars_ptr;
      pcSearchFilter : Interfaces.C.Strings.chars_ptr;
      callback : access function
        (arg1 : SLPHandle;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : unsigned_short;
         arg4 : SLPError;
         arg5 : System.Address) return SLPBoolean;
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:694
   pragma Import (C, SLPFindSrvs, "SLPFindSrvs");

   function SLPFindAttrs
     (hSLP : SLPHandle;
      pcURLOrServiceType : Interfaces.C.Strings.chars_ptr;
      pcScopeList : Interfaces.C.Strings.chars_ptr;
      pcAttrIds : Interfaces.C.Strings.chars_ptr;
      callback : access function
        (arg1 : SLPHandle;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : SLPError;
         arg4 : System.Address) return SLPBoolean;
      pvCookie : System.Address) return SLPError;  -- /usr/include/slp.h:740
   pragma Import (C, SLPFindAttrs, "SLPFindAttrs");

   function SLPGetRefreshInterval return unsigned_short;  -- /usr/include/slp.h:790
   pragma Import (C, SLPGetRefreshInterval, "SLPGetRefreshInterval");

   function SLPFindScopes (hSLP : SLPHandle; ppcScopeList : System.Address) return SLPError;  -- /usr/include/slp.h:807
   pragma Import (C, SLPFindScopes, "SLPFindScopes");

   function SLPParseSrvURL (pcSrvURL : Interfaces.C.Strings.chars_ptr; ppSrvURL : System.Address) return SLPError;  -- /usr/include/slp.h:832
   pragma Import (C, SLPParseSrvURL, "SLPParseSrvURL");

   function SLPEscape
     (pcInbuf : Interfaces.C.Strings.chars_ptr;
      ppcOutBuf : System.Address;
      isTag : SLPBoolean) return SLPError;  -- /usr/include/slp.h:861
   pragma Import (C, SLPEscape, "SLPEscape");

   function SLPUnescape
     (pcInbuf : Interfaces.C.Strings.chars_ptr;
      ppcOutBuf : System.Address;
      isTag : SLPBoolean) return SLPError;  -- /usr/include/slp.h:891
   pragma Import (C, SLPUnescape, "SLPUnescape");

   procedure SLPFree (pvMem : System.Address);  -- /usr/include/slp.h:921
   pragma Import (C, SLPFree, "SLPFree");

   function SLPGetProperty (pcName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/slp.h:933
   pragma Import (C, SLPGetProperty, "SLPGetProperty");

   procedure SLPSetProperty (pcName : Interfaces.C.Strings.chars_ptr; pcValue : Interfaces.C.Strings.chars_ptr);  -- /usr/include/slp.h:949
   pragma Import (C, SLPSetProperty, "SLPSetProperty");

   function SLPParseAttrs
     (pcAttrList : Interfaces.C.Strings.chars_ptr;
      pcAttrId : Interfaces.C.Strings.chars_ptr;
      ppcAttrVal : System.Address) return SLPError;  -- /usr/include/slp.h:964
   pragma Import (C, SLPParseAttrs, "SLPParseAttrs");

end slp_h;
