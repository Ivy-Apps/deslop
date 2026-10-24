# The primary user's identity, shared by every system builder. Only the
# declaration lives here: how a platform turns it into an account (groups,
# password, Home Manager profile) is that platform's user module's business.
{ lib, ... }:
{
  options.myConfig.user = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "Primary user's login name";
    };
    fullName = lib.mkOption {
      type = lib.types.str;
      description = "Primary user's full name";
    };
    email = lib.mkOption {
      type = lib.types.str;
      description = "Primary user's email address";
    };
  };
}
